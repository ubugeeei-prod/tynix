local M = {}

local uv = vim.uv or vim.loop

local default_cmd = { "tynix-lsp" }
local default_root_markers = {
  "flake.nix",
  "cabal.project",
  "pnpm-workspace.yaml",
  "tynix.config.tynix",
  ".git",
}
local default_filetypes = { "tynix", "nix" }

local function clone(list)
  return vim.deepcopy(list)
end

local function normalize_string(value)
  if type(value) ~= "string" then
    return nil
  end
  local trimmed = vim.trim(value)
  return trimmed ~= "" and trimmed or nil
end

local function normalize_list(values)
  local normalized = {}

  if type(values) ~= "table" then
    return normalized
  end

  for _, value in ipairs(values) do
    local trimmed = normalize_string(value)
    if trimmed then
      table.insert(normalized, trimmed)
    end
  end

  return normalized
end

---Normalize the configured tynix-lsp command into argv form.
---
---The helper accepts either a string or a list so users can configure the
---language server in the style that is most natural for their Neovim setup.
---@param cmd string|string[]|nil
---@return string[]
function M.normalize_cmd(cmd)
  local command = normalize_string(cmd)
  if command then
    return { command }
  end

  local normalized = normalize_list(cmd)
  if #normalized > 0 then
    return normalized
  end

  return clone(default_cmd)
end

---Resolve the project-root markers used for workspace discovery.
---@param markers string[]|nil
---@return string[]
function M.root_markers(markers)
  local normalized = normalize_list(markers)
  if #normalized > 0 then
    return normalized
  end

  return clone(default_root_markers)
end

---Resolve the filetypes the language server attaches to.
---
---Defaults to `{ "tynix", "nix" }` so tynix-lsp also provides ambient typing on
---plain Nix files. Pass `{ "tynix" }` to leave `.nix` to another server.
---@param filetypes string[]|nil
---@return string[]
function M.filetypes(filetypes)
  local normalized = normalize_list(filetypes)
  if #normalized > 0 then
    return normalized
  end

  return clone(default_filetypes)
end

local function source_path(source)
  if type(source) == "number" then
    local name = vim.api.nvim_buf_get_name(source == 0 and vim.api.nvim_get_current_buf() or source)
    return name ~= "" and name or nil
  end
  return source
end

---Find the closest ancestor directory containing one of `markers`.
---
---Uses `vim.fs.root` on Neovim 0.10+ and falls back to `vim.fs.find` on older
---releases.
---@param source integer|string
---@param markers string[]
---@return string|nil
local function find_root(source, markers)
  if vim.fs.root then
    return vim.fs.root(source, markers)
  end

  local path = source_path(source)
  if not path then
    return nil
  end

  local found = vim.fs.find(markers, { upward = true, path = vim.fs.dirname(path) })[1]
  return found and vim.fs.dirname(found) or nil
end

---Resolve the workspace root for the current buffer or path.
---
---An explicit string or callback wins over marker-based discovery so callers
---can integrate tynix into unusual repository layouts when needed.
---@param source integer|string
---@param opts table|nil
---@return string
function M.resolve_root_dir(source, opts)
  opts = opts or {}

  if type(opts.root_dir) == "function" then
    local resolved = opts.root_dir(source)
    if type(resolved) == "string" and resolved ~= "" then
      return resolved
    end
  end

  if type(opts.root_dir) == "string" and opts.root_dir ~= "" then
    return opts.root_dir
  end

  return find_root(source, M.root_markers(opts.root_markers)) or uv.cwd()
end

---Build the `vim.lsp.start` config used by the tynix plugin on Neovim < 0.11.
---@param source integer|string
---@param opts table|nil
---@return table
function M.server_config(source, opts)
  opts = opts or {}

  return {
    name = opts.name or "tynix-lsp",
    cmd = M.normalize_cmd(opts.cmd),
    cmd_env = type(opts.cmd_env) == "table" and opts.cmd_env or nil,
    root_dir = M.resolve_root_dir(source, opts),
    init_options = type(opts.init_options) == "table" and opts.init_options or nil,
    settings = type(opts.settings) == "table" and opts.settings or nil,
  }
end

---Build the `vim.lsp.config()` table used on Neovim 0.11+.
---
---`root_dir` is expressed as the 0.11 callback form so the same discovery
---rules (explicit root, callback, markers, cwd fallback) apply on both paths.
---@param opts table|nil
---@return table
function M.lsp_config(opts)
  opts = opts or {}

  return {
    cmd = M.normalize_cmd(opts.cmd),
    cmd_env = type(opts.cmd_env) == "table" and opts.cmd_env or nil,
    filetypes = M.filetypes(opts.filetypes),
    root_dir = function(bufnr, on_dir)
      on_dir(M.resolve_root_dir(bufnr, opts))
    end,
    init_options = type(opts.init_options) == "table" and opts.init_options or nil,
    settings = type(opts.settings) == "table" and opts.settings or nil,
  }
end

return M
