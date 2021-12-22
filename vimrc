" Configuration file for vim
language en_US.utf-8

" Normally we use vim-extensions. If you want true vi-compatibility
" remove change the following statements
set nocompatible	" Use Vim defaults instead of 100% vi compatibility
set backspace=2		" more powerful backspacing

" Don't write backup file if vim is being called by "crontab -e"
au BufWrite /private/tmp/crontab.* set nowritebackup nobackup
" Don't write backup file if vim is being called by "chpass"
au BufWrite /private/etc/pw.* set nowritebackup nobackup

set nocompatible              " be iMproved, required

"--------- add for vim-plug -----------

" vim-plug doesn't need to set rtp
"set rtp+=~/.vim/plugged
" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" tools
Plug 'vim-scripts/L9'

" browser enhencement
" Use builtin netrw
Plug 'scrooloose/nerdtree'
"Plugin 'Xuyuanp/nerdtree-git-plugin'
"Plugin 'Shougo/vimproc.vim' " use zsh

" UI enhencement
Plug 'bling/vim-airline'
" Plug 'itchyny/lightline.vim'
Plug 'drmikehenry/vim-fontsize'

" language support
Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'plasticboy/vim-markdown'
Plug 'dzeban/vim-log-syntax'

"+coding enhencement
Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'nathanaelkane/vim-indent-guides'
"Plugin 'Shougo/unite.vim' Use ctrlp
Plug 'Shougo/neocomplete.vim'
"|--git
Plug 'tpope/vim-fugitive'
" colorscheme
Plug 'flazz/vim-colorschemes'
Plug 'altercation/vim-colors-solarized'
Plug 'tomasr/molokai'
Plug 'morhetz/gruvbox'

Plug 'Siphalor/vim-atomified'
"
Plug 'easymotion/vim-easymotion'
Plug 'vim-scripts/sjump.vim'
Plug 'PProvost/vim-markdown-jekyll'
Plug 'mkdong/vim-linux-coding-style'

Plug 'jceb/vim-orgmode'

Plug 'airblade/vim-gitgutter'
Plug 'vim-scripts/taglist.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-scripts/rainbow_parentheses.vim'
Plug 'osyo-manga/vim-over'
Plug 'crater2150/vim-theme-chroma'
Plug 'vim-syntastic/syntastic'

Plug 'yosssi/vim-ace'
Plug 'fatih/vim-go'
Plug 'mileszs/ack.vim'

"Plug 'yuttie/comfortable-motion.vim'
Plug 'terryma/vim-smooth-scroll'

Plug 'axvr/org.vim'

"Plugin 'vim-scripts/Smart-Tabs'
"
" Nvim does not evaluate folds during insert-mo
" Plug 'Konfekt/FastFold' "foldmethod=syntax is slow for large file, use this!

Plug 'dstein64/vim-startuptime' " visualize startuptime

" All of your Plugins must be added before the following line
"
" Initialize plugin system
call plug#end()

" Put your non-Plugin stuff after this line
if executable('rg')
  let g:ackprg = 'rg --vimgrep --smart-case --follow'
  cnoreabbrev ag Ack
  cnoreabbrev aG Ack
  cnoreabbrev Ag Ack
  cnoreabbrev AG Ack
endif


"set autoindent
"set copyindent
"set preserveindent

" format
set nu
set ts=4
set sw=4
set sts=4
set expandtab
au FileType javascript setl sw=2 sts=2 ts=2
au FileType html setl sw=2 sts=2 ts=2 expandtab
au FileType cpp setl sw=4 sts=4 ts=4 noexpandtab
au FileType c setl sw=8 sts=8 ts=8 noexpandtab
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
"set expandtab
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

" disable this for large file loading
let g:airline#extensions#tagbar#enabled = 0

" prefix with s: for local script-only functions
function! s:setFolding()
  let s:fz = line2byte('$') + len(getline('$'))
  if s:fz < 1024 * 1024 "1M
      set foldmethod=syntax
  else
      echo "Large file, optimize for speed."
      set foldmethod=indent
  endif
endfunction

augroup folding
   au!
   au BufReadPost * :call s:setFolding()
augroup end

syntax enable
"set foldmethod=syntax
set foldmethod=manual
"set nofoldenable
"set foldlevel=1
set foldlevelstart=20

colorscheme molokai
colorscheme atomified
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

"syntastic config
let g:syntastic_c_include_dirs = ['/lib/modules/$(shell uname -r)/build/include', '/lib/modules/4.5.2/build/include', '/usr/src/linux-4.12.8/include/']
let g:syntastic_c_check_header = 1
let g:syntastic_cpp_compiler_options = ' -std=c++11 '

" React jsx
let g:jsx_ext_required = 0
"colorscheme CandyPaperLight
"colorscheme 0x7A69_dark
colorscheme chroma
colorscheme PaperColor
set bg=dark

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
"au VimEnter * RainbowParenthesesToggle
"au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
"au Syntax * RainbowParenthesesLoadBraces

"" No Plugins
" FINDING FILES
set path+=**
set wildmenu

" File broswing
""" let g:netrw_banner=0
""" let g:netrw_browser_split=4
""" let g:netrw_altv=1
""" let g:netrw_liststyle=3 "tree view
"let g:netrw_list_hide=netrw_gitignore#Hide()
"let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

"let g:linuxsty_patterns = []
"set cinoptions=N-st100
set cindent
set cinoptions=:0,l1,t0,g0,(0,N-s


"""""" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0

"let b:syntastic_mode = "passive"

set tags+=tags;/
"set autochdir

let g:go_version_warning = 0


fun s:c()
    syntax match message_trace  ".*TRACE:.*"
    "hi default link message_trace PmenuSel
    "hi message_trace ctermbg=LightGreen  ctermfg=LightGreen
    hi message_trace ctermfg=Blue
endfun

augroup ft_log
  autocmd!
  autocmd syntax log call s:c()
augroup end


let g:airline#extensions#whitespace#checks = [ 'trailing', 'long' ]
let g:airline_mode_map = {
      \ '__'     : '-',
      \ 'c'      : 'CMD',
      \ 'i'      : 'I',
      \ 'n'      : 'N',
      \ 'R'      : 'R',
      \ 'Rv'     : 'V-R',
      \ 's'      : 'SEL',
      \ 't'      : 'TERM',
      \ 'v'      : 'VIS',
      \ }
