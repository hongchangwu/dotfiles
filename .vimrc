execute pathogen#infect()

syntax on
set guifont=Inconsolata:h14
set encoding=utf-8
colorscheme grb256

" Show line numbers
set number

" Show partial commands in the last line of the screen
set showcmd

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

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
