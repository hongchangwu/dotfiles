execute pathogen#infect()

syntax on
set guifont=Inconsolata:h14
colorscheme grb256

" Show line number
set number

" Customize backspaces
set backspace=indent,eol,start

" Formatting options
set autoindent
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set shiftround
set tw=78

" Allow % to bounce between angles too
set matchpairs+=<:>

" Enable filetype detection, plugin and indent
filetype plugin indent on

" Omni completion
set ofu=syntaxcomplete#Complete

" Taglist mapping
nnoremap <silent> <F2> :TlistToggle<CR>

" NERD tree
nnoremap <silent> <F3> :NERDTreeToggle<CR>
