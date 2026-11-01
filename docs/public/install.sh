#!/bin/sh
# tynix installer
#
#   curl -fsSL https://tynix.dev/install.sh | sh
#   curl -fsSL https://tynix.dev/install.sh | sh -s -- --version 0.5.0
#   curl -fsSL https://tynix.dev/install.sh | sh -s -- --uninstall
#
# Installs the prebuilt `tynix` CLI and `tynix-lsp` language server from the
# GitHub release archives. Every archive is verified against its published
# SHA-256 checksum before anything is installed.
#
# Environment variables:
#   TYNIX_VERSION        Release to install, e.g. 0.5.0 or v0.5.0 (default: latest)
#   TYNIX_INSTALL_DIR    Directory for the binaries (default: $HOME/.tynix/bin)
#   TYNIX_DOWNLOAD_BASE  Base URL of the release assets; files are fetched from
#                       <base>/<tag>/<file> (default: https://tynix.dev/download)
#   TYNIX_REPO           GitHub repository used to resolve the latest release
#                       (default: ubugeeei-prod/tynix)
#
# Supported targets: linux-x64, linux-arm64, macos-arm64, macos-x64.
# Everywhere else (and for NixOS users), install through the Nix flake:
#   nix profile install github:ubugeeei-prod/tynix

set -eu

TYNIX_REPO="${TYNIX_REPO:-ubugeeei-prod/tynix}"
TYNIX_DOWNLOAD_BASE="${TYNIX_DOWNLOAD_BASE:-https://tynix.dev/download}"
TYNIX_LATEST_URL="${TYNIX_LATEST_URL:-https://tynix.dev/latest}"
TYNIX_VERSION="${TYNIX_VERSION:-}"
TYNIX_INSTALL_DIR="${TYNIX_INSTALL_DIR:-}"
FLAKE_REF="github:ubugeeei-prod/tynix"
BINARIES="tynix tynix-lsp"

tmp_dir=""

say() {
  printf 'tynix-install: %s\n' "$*"
}

warn() {
  printf 'tynix-install: warning: %s\n' "$*" >&2
}

die() {
  printf 'tynix-install: error: %s\n' "$*" >&2
  exit 1
}

cleanup() {
  if [ -n "$tmp_dir" ] && [ -d "$tmp_dir" ]; then
    rm -rf "$tmp_dir"
  fi
}

usage() {
  cat <<EOF
tynix installer

Usage:
  curl -fsSL https://tynix.dev/install.sh | sh
  curl -fsSL https://tynix.dev/install.sh | sh -s -- [options]

Options:
  --version <version>   Install a specific release (e.g. 0.5.0); default: latest
  --dir <path>          Install directory (default: \$HOME/.tynix/bin)
  --uninstall           Remove tynix and tynix-lsp from the install directory
  -h, --help            Show this help

Environment:
  TYNIX_VERSION, TYNIX_INSTALL_DIR, TYNIX_DOWNLOAD_BASE, TYNIX_REPO

With Nix, prefer the flake instead:
  nix profile install $FLAKE_REF
EOF
}

has() {
  command -v "$1" >/dev/null 2>&1
}

suggest_flake() {
  cat >&2 <<EOF

Prebuilt tynix binaries are available for linux-x64, linux-arm64,
macos-arm64 and macos-x64. On other platforms, build from source with Nix:

  nix profile install $FLAKE_REF
  # or run without installing:
  nix run $FLAKE_REF -- check ./main.tynix

EOF
}

unsupported() {
  printf 'tynix-install: error: %s\n' "$*" >&2
  suggest_flake
  exit 1
}

# --- platform detection ------------------------------------------------------

detect_target() {
  # TYNIX_UNAME_S / TYNIX_UNAME_M exist so the installer can be tested for
  # other platforms; they are not meant for normal use.
  os="${TYNIX_UNAME_S:-$(uname -s)}"
  arch="${TYNIX_UNAME_M:-$(uname -m)}"

  case "$os" in
    Linux) os_part=linux ;;
    Darwin) os_part=macos ;;
    MINGW* | MSYS* | CYGWIN* | Windows_NT)
      unsupported "Windows is not supported natively. Use WSL2 and re-run this installer inside it."
      ;;
    *) unsupported "unsupported operating system: $os" ;;
  esac

  case "$arch" in
    x86_64 | amd64) arch_part=x64 ;;
    aarch64 | arm64) arch_part=arm64 ;;
    *) unsupported "unsupported CPU architecture: $arch ($os)" ;;
  esac

  # An x86_64 shell running under Rosetta 2 on Apple silicon should still get
  # the native arm64 build.
  if [ "$os_part" = macos ] && [ "$arch_part" = x64 ] && [ -z "${TYNIX_UNAME_M:-}" ]; then
    if [ "$(sysctl -n sysctl.proc_translated 2>/dev/null || echo 0)" = 1 ]; then
      arch_part=arm64
    fi
  fi

  printf '%s-%s\n' "$os_part" "$arch_part"
}

# --- downloads ---------------------------------------------------------------

download() {
  # download <url> <output-file>
  if has curl; then
    curl --proto '=https,file' --tlsv1.2 -fsSL --retry 3 -o "$2" "$1"
  elif has wget; then
    wget -q -O "$2" "$1"
  else
    die "either curl or wget is required"
  fi
}

download_stdout() {
  if has curl; then
    curl --proto '=https' --tlsv1.2 -fsSL --retry 3 "$1"
  elif has wget; then
    wget -q -O - "$1"
  else
    die "either curl or wget is required"
  fi
}

# Prints the final URL after following redirects.
effective_url() {
  if has curl; then
    curl --proto '=https' --tlsv1.2 -fsSL -o /dev/null -w '%{url_effective}' "$1"
  elif has wget; then
    wget -q -S -O /dev/null "$1" 2>&1 | sed -n 's/^ *[Ll]ocation: *//p' | tail -n 1 | tr -d '\r'
  else
    die "either curl or wget is required"
  fi
}

resolve_latest_tag() {
  tag=""
  api="https://api.github.com/repos/$TYNIX_REPO/releases/latest"
  if json="$(download_stdout "$api" 2>/dev/null)"; then
    tag="$(printf '%s\n' "$json" | sed -n 's/^[[:space:]]*"tag_name":[[:space:]]*"\([^"]*\)".*/\1/p' | head -n 1)"
  fi

  if [ -z "$tag" ]; then
    # Rate-limited or blocked API: follow https://tynix.dev/latest, which
    # redirects to .../releases/tag/<tag>.
    if url="$(effective_url "$TYNIX_LATEST_URL" 2>/dev/null)"; then
      case "$url" in
        */tag/*) tag="${url##*/tag/}" ;;
      esac
    fi
  fi

  [ -n "$tag" ] || die "could not determine the latest tynix release; set TYNIX_VERSION (e.g. TYNIX_VERSION=0.5.0)"
  printf '%s\n' "$tag"
}

normalize_tag() {
  case "$1" in
    v*) printf '%s\n' "$1" ;;
    *) printf 'v%s\n' "$1" ;;
  esac
}

sha256_of() {
  if has sha256sum; then
    sha256sum "$1" | awk '{print $1}'
  elif has shasum; then
    shasum -a 256 "$1" | awk '{print $1}'
  elif has openssl; then
    openssl dgst -sha256 "$1" | awk '{print $NF}'
  else
    die "no SHA-256 tool found (need sha256sum, shasum or openssl)"
  fi
}

read_expected_checksum() {
  expected="$(awk -v name="$2" '$2 == name {print $1}' "$1" | head -n 1)"
  if [ -z "$expected" ]; then
    expected="$(awk 'NR == 1 {print $1}' "$1")"
  fi
  expected="$(printf '%s' "$expected" | tr 'A-F' 'a-f')"
  case "$expected" in
    *[!0-9a-f]* | "") die "malformed checksum file $(basename "$1")" ;;
  esac
  [ "${#expected}" -eq 64 ] || die "malformed checksum file $(basename "$1")"
  printf '%s' "$expected"
}

verify_archive_checksum() {
  expected="$(read_expected_checksum "$2" "$(basename "$1")")"
  actual="$(sha256_of "$1")"
  if [ "$actual" != "$expected" ]; then
    die "checksum mismatch for $(basename "$1")
  expected: $expected
  actual:   $actual"
  fi
  say "checksum verified ($actual)"
}

# --- install / uninstall -----------------------------------------------------

path_hint() {
  case ":${PATH:-}:" in
    *":$1:"*) return 0 ;;
  esac

  shell_name="$(basename "${SHELL:-sh}")"
  case "$shell_name" in
    fish)
      line="fish_add_path \"$1\""
      rc="${HOME:-~}/.config/fish/config.fish"
      ;;
    zsh)
      line="export PATH=\"$1:\$PATH\""
      rc="${HOME:-~}/.zshrc"
      ;;
    bash)
      line="export PATH=\"$1:\$PATH\""
      rc="${HOME:-~}/.bashrc"
      ;;
    *)
      line="export PATH=\"$1:\$PATH\""
      rc="${HOME:-~}/.profile"
      ;;
  esac

  cat <<EOF

$1 is not on your PATH. Add it by appending this line to $rc:

  $line

then restart your shell.
EOF
}

do_uninstall() {
  removed=0
  for bin in $BINARIES; do
    if [ -e "$TYNIX_INSTALL_DIR/$bin" ] || [ -L "$TYNIX_INSTALL_DIR/$bin" ]; then
      rm -f "$TYNIX_INSTALL_DIR/$bin"
      say "removed $TYNIX_INSTALL_DIR/$bin"
      removed=1
    fi
  done

  # Clean up the default layout (~/.tynix/bin) if nothing else lives there.
  if [ "$TYNIX_INSTALL_DIR" = "$default_install_dir" ]; then
    rmdir "$TYNIX_INSTALL_DIR" 2>/dev/null || true
    rmdir "$(dirname "$TYNIX_INSTALL_DIR")" 2>/dev/null || true
  fi

  if [ "$removed" = 0 ]; then
    say "nothing to uninstall in $TYNIX_INSTALL_DIR"
  else
    say "tynix has been uninstalled"
  fi
}

do_install() {
  target="$(detect_target)"

  if [ -n "$TYNIX_VERSION" ]; then
    tag="$(normalize_tag "$TYNIX_VERSION")"
  else
    say "resolving the latest release"
    tag="$(normalize_tag "$(resolve_latest_tag)")"
  fi

  archive="tynix-$tag-$target.tar.gz"
  checksum="tynix-$tag-$target.sha256"
  base="${TYNIX_DOWNLOAD_BASE%/}/$tag"

  tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/tynix-install.XXXXXX")" || die "cannot create a temporary directory"
  trap cleanup EXIT
  trap 'cleanup; exit 130' INT
  trap 'cleanup; exit 143' TERM

  say "installing tynix $tag ($target)"
  say "downloading $base/$archive"
  download "$base/$archive" "$tmp_dir/$archive" ||
    die "failed to download $base/$archive (does release $tag exist for $target?)"
  download "$base/$checksum" "$tmp_dir/$checksum" ||
    die "failed to download $base/$checksum"

  verify_archive_checksum "$tmp_dir/$archive" "$tmp_dir/$checksum"

  mkdir -p "$tmp_dir/extract"
  tar -xzf "$tmp_dir/$archive" -C "$tmp_dir/extract" || die "failed to extract $archive"

  mkdir -p "$TYNIX_INSTALL_DIR" || die "cannot create $TYNIX_INSTALL_DIR"
  for bin in $BINARIES; do
    src="$(find "$tmp_dir/extract" -type f -path "*/bin/$bin" | head -n 1)"
    [ -n "$src" ] || die "$archive does not contain bin/$bin"
    # Install through a temporary name so a running tynix-lsp is replaced
    # atomically instead of being overwritten in place.
    cp "$src" "$TYNIX_INSTALL_DIR/.$bin.tmp.$$"
    chmod 0755 "$TYNIX_INSTALL_DIR/.$bin.tmp.$$"
    mv -f "$TYNIX_INSTALL_DIR/.$bin.tmp.$$" "$TYNIX_INSTALL_DIR/$bin"
    say "installed $TYNIX_INSTALL_DIR/$bin"
  done

  if version_output="$("$TYNIX_INSTALL_DIR/tynix" --version 2>&1)"; then
    say "$version_output"
  else
    warn "the installed tynix binary failed to run:"
    printf '%s\n' "$version_output" >&2
    suggest_flake
    exit 1
  fi

  path_hint "$TYNIX_INSTALL_DIR"
  say "done. Run 'tynix --help' to get started."
}

main() {
  action=install
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --uninstall) action=uninstall ;;
      --version)
        [ "$#" -ge 2 ] || die "--version requires a value"
        TYNIX_VERSION="$2"
        shift
        ;;
      --version=*) TYNIX_VERSION="${1#--version=}" ;;
      --dir)
        [ "$#" -ge 2 ] || die "--dir requires a value"
        TYNIX_INSTALL_DIR="$2"
        shift
        ;;
      --dir=*) TYNIX_INSTALL_DIR="${1#--dir=}" ;;
      -h | --help)
        usage
        exit 0
        ;;
      *) die "unknown option: $1 (see --help)" ;;
    esac
    shift
  done

  [ -n "${HOME:-}" ] || [ -n "$TYNIX_INSTALL_DIR" ] || die "HOME is not set; set TYNIX_INSTALL_DIR"
  default_install_dir="${HOME:-}/.tynix/bin"
  TYNIX_INSTALL_DIR="${TYNIX_INSTALL_DIR:-$default_install_dir}"

  case "$action" in
    install) do_install ;;
    uninstall) do_uninstall ;;
  esac
}

main "$@"
