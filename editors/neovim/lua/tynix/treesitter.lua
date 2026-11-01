---tree-sitter integration for tynix.
---
---The grammar lives in this repository under `editors/tree-sitter-tynix` and the
---matching queries ship in this plugin's `queries/tynix/` runtime directory, so
---only the compiled parser needs to come from somewhere: either
---nvim-treesitter (`:TSInstall tynix` after `register()`), or an explicit
---`parser_path` pointing at a prebuilt `tynix.so`.
local M = {}

M.lang = "tynix"

M.install_info = {
  url = "https://github.com/ubugeeei/tynix",
  location = "editors/tree-sitter-tynix",
  files = { "src/parser.c", "src/scanner.c" },
  branch = "main",
  generate_requires_npm = false,
  requires_generate_from_grammar = false,
}

local function register_with_nvim_treesitter()
  local ok, parsers = pcall(require, "nvim-treesitter.parsers")
  if not ok or type(parsers) ~= "table" then
    return false
  end

  if type(parsers.get_parser_configs) == "function" then
    -- nvim-treesitter `master` branch.
    parsers.get_parser_configs()[M.lang] = {
      install_info = vim.deepcopy(M.install_info),
      filetype = "tynix",
      maintainers = { "@ubugeeei" },
    }
    return true
  end

  -- nvim-treesitter `main` branch: parser configs are a plain table that is
  -- rebuilt on `:TSUpdate`, so re-register from the `User TSUpdate` event too.
  local function add()
    local current = require("nvim-treesitter.parsers")
    current[M.lang] = {
      install_info = {
        url = M.install_info.url,
        location = M.install_info.location,
        branch = M.install_info.branch,
        generate = false,
      },
      tier = 3,
    }
  end
  add()
  vim.api.nvim_create_autocmd("User", {
    group = vim.api.nvim_create_augroup("tynix-treesitter-register", { clear = true }),
    pattern = "TSUpdate",
    callback = add,
  })
  return true
end

---Call `vim.treesitter.language.add` portably.
---
---0.9 returns nothing and throws on failure, 0.10+ returns `true` or
---`nil, err`, and 0.8 only has `require_language`.
---@param opts table|nil
---@return boolean ok
---@return string|nil err
local function add_language(opts)
  if vim.treesitter.language.add then
    local ok, result, err = pcall(vim.treesitter.language.add, M.lang, opts)
    if not ok then
      return false, tostring(result)
    end
    if vim.fn.has("nvim-0.10") == 1 and not result then
      return false, err or "parser not found"
    end
    return true
  end
  local ok, err = pcall(
    vim.treesitter.require_language,
    M.lang,
    opts and opts.path or nil,
    opts == nil
  )
  return ok, ok and nil or tostring(err)
end

---Load a prebuilt parser from `path` (a compiled `tynix.so` / `.dylib`).
---@param path string
---@return boolean ok
---@return string|nil err
function M.load_parser(path)
  return add_language({ path = path })
end

---Whether a tynix parser can be loaded right now.
---@return boolean
function M.parser_available()
  return (add_language(nil))
end

---Start tree-sitter highlighting in `bufnr` when the parser is available.
---@param bufnr integer
---@return boolean started
function M.start(bufnr)
  if not M.parser_available() then
    return false
  end
  return pcall(vim.treesitter.start, bufnr, M.lang)
end

---Register the tynix parser.
---@param opts table|nil `{ parser_path?: string, highlight?: boolean }`
---@return table status `{ nvim_treesitter: boolean, parser: boolean }`
function M.register(opts)
  opts = opts or {}

  if vim.treesitter.language.register then
    pcall(vim.treesitter.language.register, M.lang, { "tynix" })
  end

  local status = { nvim_treesitter = register_with_nvim_treesitter(), parser = false }

  if type(opts.parser_path) == "string" and opts.parser_path ~= "" then
    status.parser = M.load_parser(vim.fn.expand(opts.parser_path))
  else
    status.parser = M.parser_available()
  end

  if opts.highlight ~= false then
    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("tynix-treesitter-highlight", { clear = true }),
      pattern = "tynix",
      callback = function(ev)
        M.start(ev.buf)
      end,
    })
  end

  return status
end

return M
