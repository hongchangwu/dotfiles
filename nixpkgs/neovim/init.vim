autocmd Filetype sh setlocal omnifunc=v:lua.vim.lsp.omnifunc
autocmd Filetype ocaml setlocal omnifunc=v:lua.vim.lsp.omnifunc
autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc
autocmd Filetype rust setlocal omnifunc=v:lua.vim.lsp.omnifunc

nnoremap <silent> gd         <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gD         <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gd         <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K          <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> R          <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gs         <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> gi         <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gt         <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> g0         <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW         <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> <F2>       <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> <leader>rn <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> gf         <cmd>lua vim.lsp.buf.formatting()<CR>
lua <<EOF
vim.cmd('packadd nvim-lspconfig')
local nvim_lsp = require'lspconfig'
nvim_lsp.bashls.setup {}
nvim_lsp.dockerls.setup {}
nvim_lsp.ocamllsp.setup {
  root_dir = nvim_lsp.util.root_pattern('dune-project', '.git');
}
nvim_lsp.pylsp.setup {}
nvim_lsp.rls.setup {}
vim.cmd('packadd nvim-treesitter')
require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",
  highlight = {
    enable = true,
    disable = { "python" },
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  refactor = {
    highlight_definitions = {
      enable = true,
    },
    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = "grr",
      },
    },
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
  },
}
EOF
