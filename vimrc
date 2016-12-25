
" Normally we use vim-extensions. If you want true vi-compatibility
" remove change the following statements
set nocompatible	" Use Vim defaults instead of 100% vi compatibility
set backspace=2		" more powerful backspacing

" Don't write backup file if vim is being called by "crontab -e"
au BufWrite /private/tmp/crontab.* set nowritebackup nobackup
" Don't write backup file if vim is being called by "chpass"
au BufWrite /private/etc/pw.* set nowritebackup nobackup

"--------- add for Vundle.vim -----------
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" tools
Plugin 'L9'

" browser enhencement
Plugin 'scrooloose/nerdtree'
"Plugin 'Shougo/vimproc.vim' " use zsh

" UI enhencement
Plugin 'bling/vim-airline'
Plugin 'drmikehenry/vim-fontsize'

" language support
Plugin 'kchmck/vim-coffee-script'
Plugin 'pangloss/vim-javascript'
Plugin 'plasticboy/vim-markdown'

"+coding enhencement
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/syntastic'
Plugin 'godlygeek/tabular'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-surround'
Plugin 'nathanaelkane/vim-indent-guides'
"Plugin 'Shougo/unite.vim' Use ctrlp
Plugin 'Shougo/neocomplete.vim'
"|--git
Plugin 'tpope/vim-fugitive'
" colorscheme
Plugin 'flazz/vim-colorschemes'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'

"
Plugin 'easymotion/vim-easymotion'
Plugin 'vim-scripts/sjump.vim'
Plugin 'PProvost/vim-markdown-jekyll'
Plugin 'vivien/vim-linux-coding-style'

Plugin 'jceb/vim-orgmode'

Plugin 'airblade/vim-gitgutter'

Plugin 'ctrlpvim/ctrlp.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Put your non-Plugin stuff after this line

" format
set nu
"set ts=4
"set sw=4
"set sts=4
"set noexpandtab
set display=lastline
set laststatus=2

" encoding
set encoding=utf-8
setglobal fileencoding=utf-8
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,latin1,cp936,iso8859
set termencoding=utf-8
" indent
set wrap
set autoindent
set smartindent
set smarttab
set cindent
set linebreak
set shiftround
set expandtab
"set showbreak=>\ \ \

" search
set hlsearch
set incsearch
set ignorecase
set showcmd

" backups
set nobackup
set nowritebackup

set ruler
set cursorline

set fileformat=unix
set fileformats=unix,dos,mac

set mouse=a

syntax enable
set foldmethod=syntax
"set nofoldenable
"set foldlevel=1
set foldlevelstart=20

colorscheme molokai
set cc=80

set list
set listchars=tab:▸\ ,eol:¬


" Gif config
"
" Require tpope/vim-repeat to enable dot repeat support
" Jump to anywhere with only `s{char}{target}`
" `s<CR>` repeat last find motion.
nmap <space> <Plug>(easymotion-s)
" Bidirectional & within line 't' motion
"omap t <Plug>(easymotion-bd-tl)
" Use uppercase target labels and type as a lower case
let g:EasyMotion_use_upper = 1
 " type `l` and match `l`&`L`
 let g:EasyMotion_smartcase = 1
 " Smartsign (type `3` and match `3`&`#`)
 let g:EasyMotion_use_smartsign_us = 1

let g:syntastic_c_include_dirs = ['/home/dmk/kernels/linux-4.5.2/include/']
let g:syntastic_c_check_header = 1
set bg=light

colorscheme CandyPaperLight
"colorscheme 0x7A69_dark

" ctrl_p
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_switch_buffer = 'et'

set wildignore+=*/tmp/*,*.so,*.swp,*.zip

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
"let g:ctrlp_custom_ignore = {
"                        \ 'dir': '\v[\/]\.(git|hg|svn)$',
"                        \ 'file': '\v\.(exe|so|dll)$',
"                        \ 'link': '',
"                        \ }
let g:ctrlp_user_command = 'find %s -type f'
"let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']

if executable('ag')
        " set for built-in grep
        set grepprg=ag\ --nogroup\ --nocolor
        " set for ctrlp
        let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
        " disable ctrlp cache (ag is faster enough)
        let g:ctrlp_user_caching = 0
endif

" bind \ to grep (ag) shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap \ :Ag<SPACE>

hi CursorLine ctermbg=8
