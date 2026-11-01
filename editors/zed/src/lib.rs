use std::env;
use std::path::Path;
use zed_extension_api::{
    register_extension,
    serde_json::{self, Value},
    settings::LspSettings,
    Command, Extension, LanguageServerId, Result, Worktree,
};

/// Zed extension entry point for tynix.
///
/// The extension does not implement language logic itself. Its only job is to
/// locate the `tynix-lsp` binary, pass through workspace shell environment
/// variables, and respect any user override configured in Zed settings.
struct Tynix;

fn trimmed_nonempty(value: String) -> Option<String> {
    let trimmed = value.trim().to_string();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed)
    }
}

fn normalize_binary_path(path: Option<String>) -> Option<String> {
    path.and_then(trimmed_nonempty)
}

fn normalize_binary_arguments(arguments: Option<Vec<String>>) -> Vec<String> {
    arguments
        .unwrap_or_default()
        .into_iter()
        .filter_map(trimmed_nonempty)
        .collect()
}

fn configured_binary(settings: Option<LspSettings>) -> Option<(String, Vec<String>)> {
    let binary = settings?.binary?;
    let path = normalize_binary_path(binary.path)?;
    Some((path, normalize_binary_arguments(binary.arguments)))
}

fn build_command(command: String, args: Vec<String>, env: Vec<(String, String)>) -> Command {
    Command { command, args, env }
}

fn default_binary_candidates(home: Option<&str>) -> Vec<String> {
    let mut candidates = Vec::new();

    if let Some(home) = home {
        candidates.push(format!("{home}/.nix-profile/bin/tynix-lsp"));
        candidates.push(format!(
            "{home}/.local/state/nix/profiles/profile/bin/tynix-lsp"
        ));
        candidates.push(format!(
            "{home}/.local/state/nix/profiles/home-manager/home-path/bin/tynix-lsp"
        ));
    }

    candidates.push("/run/current-system/sw/bin/tynix-lsp".to_string());
    candidates
}

fn resolve_default_binary_with<F>(home: Option<&str>, exists: F) -> Option<String>
where
    F: Fn(&str) -> bool,
{
    default_binary_candidates(home)
        .into_iter()
        .find(|candidate| exists(candidate))
}

/// Wrap user-provided `lsp.tynix-lsp.settings` under the `tynix` section that
/// tynix-lsp requests through `workspace/configuration`.
///
/// Settings that are already namespaced (`{ "tynix": { ... } }`) are passed
/// through unchanged so both spellings work in Zed's settings.json.
fn workspace_configuration(settings: Option<Value>) -> Option<Value> {
    let settings = settings?;
    match &settings {
        Value::Object(map) if map.len() == 1 && map.contains_key("tynix") => Some(settings),
        _ => Some(serde_json::json!({ "tynix": settings })),
    }
}

fn resolve_default_binary() -> Option<String> {
    let home = env::var("HOME").ok();
    resolve_default_binary_with(home.as_deref(), |candidate| Path::new(candidate).exists())
}

impl Extension for Tynix {
    fn new() -> Self {
        Self {}
    }

    /// Resolve the command used to launch the tynix language server.
    ///
    /// A per-worktree configured binary wins. When no override is present, the
    /// extension falls back to looking up `tynix-lsp` on the worktree PATH so it
    /// works naturally inside the Nix dev shell.
    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Command> {
        let env = worktree.shell_env();

        let configured = LspSettings::for_worktree(language_server_id.as_ref(), worktree).ok();
        if let Some((path, arguments)) = configured_binary(configured) {
            return Ok(build_command(path, arguments, env));
        }

        let path = worktree
            .which("tynix-lsp")
            .or_else(|| resolve_default_binary())
            .ok_or_else(|| "tynix-lsp must be installed and available in $PATH.".to_string())?;
        Ok(build_command(path, vec![], env))
    }

    /// Forward `lsp.tynix-lsp.initialization_options` from Zed settings.
    fn language_server_initialization_options(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Option<Value>> {
        Ok(
            LspSettings::for_worktree(language_server_id.as_ref(), worktree)
                .ok()
                .and_then(|settings| settings.initialization_options),
        )
    }

    /// Forward `lsp.tynix-lsp.settings` as the `tynix` configuration section.
    fn language_server_workspace_configuration(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Option<Value>> {
        Ok(workspace_configuration(
            LspSettings::for_worktree(language_server_id.as_ref(), worktree)
                .ok()
                .and_then(|settings| settings.settings),
        ))
    }
}

register_extension!(Tynix);

#[cfg(test)]
mod tests {
    use super::{
        build_command, default_binary_candidates, normalize_binary_arguments,
        normalize_binary_path, resolve_default_binary_with, workspace_configuration,
    };
    use zed_extension_api::serde_json::json;

    #[test]
    fn normalize_binary_path_drops_blank_values() {
        assert_eq!(normalize_binary_path(None), None);
        assert_eq!(normalize_binary_path(Some(String::new())), None);
        assert_eq!(normalize_binary_path(Some("   ".into())), None);
    }

    #[test]
    fn normalize_binary_path_trims_explicit_values() {
        assert_eq!(
            normalize_binary_path(Some(" tynix-lsp ".into())),
            Some("tynix-lsp".into())
        );
        assert_eq!(
            normalize_binary_path(Some("/nix/store/bin/tynix-lsp".into())),
            Some("/nix/store/bin/tynix-lsp".into())
        );
    }

    #[test]
    fn normalize_binary_arguments_compacts_argv() {
        assert_eq!(normalize_binary_arguments(None), Vec::<String>::new());
        assert_eq!(
            normalize_binary_arguments(Some(vec!["".into(), " --stdio ".into(), "  ".into()])),
            vec!["--stdio".to_string()]
        );
        assert_eq!(
            normalize_binary_arguments(Some(vec!["--log-file".into(), "/tmp/tynix.log".into()])),
            vec!["--log-file".to_string(), "/tmp/tynix.log".to_string()]
        );
    }

    #[test]
    fn build_command_preserves_command_args_and_env() {
        let command = build_command(
            "tynix-lsp".into(),
            vec!["--stdio".into()],
            vec![("TYNIX_ENV".into(), "1".into())],
        );

        assert_eq!(command.command, "tynix-lsp");
        assert_eq!(command.args, vec!["--stdio".to_string()]);
        assert_eq!(
            command.env,
            vec![("TYNIX_ENV".to_string(), "1".to_string())]
        );
    }

    #[test]
    fn default_binary_candidates_prioritize_common_nix_profiles() {
        assert_eq!(
            default_binary_candidates(Some("/home/alice")),
            vec![
                "/home/alice/.nix-profile/bin/tynix-lsp".to_string(),
                "/home/alice/.local/state/nix/profiles/profile/bin/tynix-lsp".to_string(),
                "/home/alice/.local/state/nix/profiles/home-manager/home-path/bin/tynix-lsp"
                    .to_string(),
                "/run/current-system/sw/bin/tynix-lsp".to_string(),
            ]
        );
    }

    #[test]
    fn resolve_default_binary_with_returns_first_existing_candidate() {
        let resolved = resolve_default_binary_with(Some("/home/alice"), |candidate| {
            candidate == "/home/alice/.local/state/nix/profiles/profile/bin/tynix-lsp"
        });

        assert_eq!(
            resolved,
            Some("/home/alice/.local/state/nix/profiles/profile/bin/tynix-lsp".to_string())
        );
        assert_eq!(
            resolve_default_binary_with(Some("/home/alice"), |_| false),
            None
        );
    }

    #[test]
    fn workspace_configuration_namespaces_plain_settings() {
        assert_eq!(workspace_configuration(None), None);
        assert_eq!(
            workspace_configuration(Some(json!({ "inlayHints": { "enabled": false } }))),
            Some(json!({ "tynix": { "inlayHints": { "enabled": false } } }))
        );
        assert_eq!(
            workspace_configuration(Some(json!({ "tynix": { "trace": "off" } }))),
            Some(json!({ "tynix": { "trace": "off" } }))
        );
    }
}
