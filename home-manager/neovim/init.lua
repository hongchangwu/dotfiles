-- LSP omnifunc setup
vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'sh', 'ocaml', 'python', 'rust' },
  callback = function()
    vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'
  end,
})

-- LSP keymaps
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

keymap('n', 'gd', vim.lsp.buf.definition, opts)
keymap('n', 'gD', vim.lsp.buf.declaration, opts)
keymap('n', 'K', vim.lsp.buf.hover, opts)
keymap('n', 'R', vim.lsp.buf.references, opts)
keymap('n', 'gs', vim.lsp.buf.signature_help, opts)
keymap('n', 'gi', vim.lsp.buf.implementation, opts)
keymap('n', 'gt', vim.lsp.buf.type_definition, opts)
keymap('n', 'g0', vim.lsp.buf.document_symbol, opts)
keymap('n', 'gW', vim.lsp.buf.workspace_symbol, opts)
keymap('n', '<F2>', vim.lsp.buf.rename, opts)
keymap('n', '<leader>rn', vim.lsp.buf.rename, opts)
keymap('n', 'gf', vim.lsp.buf.format, opts)

-- Load nvim-lspconfig package when using Neovim packages / Nix packages
vim.cmd('packadd nvim-lspconfig')

-- Neovim 0.11+ LSP configuration API
vim.lsp.config('ocamllsp', {
  root_markers = { 'dune-project', '.git' },
})

vim.lsp.enable({
  'bashls',
  'dockerls',
  'ocamllsp',
  'pyright',
  'rust_analyzer',
})
