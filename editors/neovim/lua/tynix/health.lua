---`:checkhealth tynix`
local M = {}

local health = vim.health or require("health")
local start = health.start or health.report_start
local ok = health.ok or health.report_ok
local warn = health.warn or health.report_warn
local error = health.error or health.report_error
local info = health.info or health.report_info

local function first_line(text)
  return (vim.split(vim.trim(text or ""), "\n", { plain = true })[1] or "")
end

local function command_version(argv)
  local output = vim.fn.system(argv)
  if vim.v.shell_error ~= 0 then
    return nil, first_line(output)
  end
  return first_line(output)
end

local function check_neovim()
  start("Neovim")
  local version = vim.version()
  local rendered = string.format("%d.%d.%d", version.major, version.minor, version.patch)
  if vim.fn.has("nvim-0.11") == 1 then
    ok("Neovim " .. rendered .. " (native vim.lsp.config / vim.lsp.enable)")
  elseif vim.fn.has("nvim-0.10") == 1 then
    ok("Neovim " .. rendered .. " (using the vim.lsp.start fallback; 0.11+ recommended)")
  else
    warn("Neovim " .. rendered .. " is older than 0.10; tynix is tested on 0.10+", {
      "Upgrade Neovim to 0.11 or newer for the best experience.",
    })
  end
end

local function check_setup()
  start("Plugin setup")
  local tynix = require("tynix")
  if tynix.options == nil then
    warn('require("tynix").setup() has not been called', {
      'Call require("tynix").setup() from your config (lazy.nvim: `opts = {}`).',
    })
  else
    ok("setup() called")
  end

  local filetype = vim.filetype.match({ filename = "example.d.tynix" })
  if filetype == "tynix" then
    ok("`.tynix` / `.d.tynix` files use the `tynix` filetype")
  else
    error("`.d.tynix` is detected as " .. tostring(filetype) .. " instead of `tynix`")
  end
end

local function check_executables()
  start("Executables")
  local opts = require("tynix").options or {}
  local cmd = require("tynix.config").normalize_cmd(opts.cmd)
  local server = cmd[1]

  if vim.fn.executable(server) == 1 then
    local version, err = command_version({ server, "--version" })
    if version then
      ok(string.format("%s: %s (%s)", server, version, vim.fn.exepath(server)))
    else
      local message = string.format(
      "%s is on PATH but `--version` failed: %s",
      server,
      err or "unknown error"
    )
    warn(message)
    end
  else
    error(server .. " was not found on PATH", {
      "Install with: curl -fsSL https://tynix.dev/install.sh | sh",
      "Or via Nix: nix profile install github:ubugeeei/tynix#tynix-lsp",
      'Or point `cmd` at the binary: require("tynix").setup({ cmd = { "/path/to/tynix-lsp" } })',
    })
  end

  if vim.fn.executable("tynix") == 1 then
    local version = command_version({ "tynix", "--version" })
    ok("tynix CLI: " .. (version or "available"))
  else
    info("tynix CLI not found on PATH (optional; used for `tynix check` / `tynix doctor`)")
  end
end

local function check_treesitter()
  start("Tree-sitter")
  local treesitter = require("tynix.treesitter")

  if treesitter.parser_available() then
    ok("tynix parser is installed")
  else
    warn("tynix parser is not installed (falling back to no tree-sitter highlighting)", {
      'With nvim-treesitter: call require("tynix").setup() and run :TSInstall tynix',
      'Or build editors/tree-sitter-tynix and pass setup({ treesitter = { parser_path = "/path/to/tynix.so" } })',
    })
  end

  local queries = vim.api.nvim_get_runtime_file("queries/tynix/highlights.scm", true)
  if #queries > 0 then
    ok("highlight queries: " .. queries[1])
  else
    error("queries/tynix/highlights.scm not found on 'runtimepath'", {
      "Make sure editors/neovim from the tynix repository is on 'runtimepath'.",
    })
  end

  if pcall(require, "nvim-treesitter") then
    ok("nvim-treesitter is available")
  else
    info("nvim-treesitter not found (optional; only needed for :TSInstall tynix)")
  end
end

local function check_clients()
  start("Language server")
  local get_clients = vim.lsp.get_clients or vim.lsp.get_active_clients
  local attached = {}
  for _, client in ipairs(get_clients()) do
    if client.name == "tynix" or client.name == "tynix-lsp" then
      local description = string.format(
        "%s (id %d, root %s)",
        client.name,
        client.id,
        client.config.root_dir or "?"
      )
      table.insert(attached, description)
    end
  end
  if #attached > 0 then
    for _, line in ipairs(attached) do
      ok("running: " .. line)
    end
  else
    info("no tynix-lsp client is running (open a .tynix file to start one)")
  end
end

function M.check()
  check_neovim()
  check_setup()
  check_executables()
  check_treesitter()
  check_clients()
end

return M
