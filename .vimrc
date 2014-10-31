" -------------------------------------------------------------------------
" MAPPING SUMMARY
" =========================================================================
" <F1>  :help [vim default]
" <F2>  :NERDTreeToggle<CR>
" <F3>  :pastetoggle
" <F4>  :w !diff % -
" <F5>  purge CtrlP cache
" <F6>  toggle line number display
" <F7>  :TagbarToggle<CR>

" -------------------------------------------------------------------------
" SETTINGS
" =========================================================================

if &compatible                  " only if not set before
    set nocompatible            " use vim-defaults instead of vi-defaults (more user friendly)
endif
set ttyfast
set confirm                     " get a dialog when :q, :w or :wq fails


" -------------------------------------------------------------------------
" PLUG-IN
" =========================================================================
"
" Use pathogen to easily modify the runtime path
" and include all plugins under ~/.vim/bundle directory
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" -------------------------------------------------------------------------
" HISTORY
" =========================================================================

set hidden                      " remember undo after exit
set history=1000
set undolevels=1000
set nobackup                    " never let vim write a backup file, they did that in the 70's


" -------------------------------------------------------------------------
" SYNTAX HIGHLIGHTING
" =========================================================================

syntax on
set t_Co=256
"colorscheme 256-grayvim
"colorscheme monokai
"colorscheme torte


" -------------------------------------------------------------------------
" SEARCHING
" =========================================================================

set incsearch                   " do incremental searching, the matches shown while typing
set hlsearch                    " highlight search pattern matches
                                " clear search buffer with ,/
nmap <silent> ,/ :nohlsearch<CR>
set ignorecase                  " case-insensitive searching
set smartcase                   " ignore case in searching unless an upppercase letter is used


" -------------------------------------------------------------------------
" DISPLAYING
" =========================================================================

set showmode                    " show mode in status bar
set showcmd                     " display typed command in status bar
set ruler                       " show cursor position in status bar
set sm                          " show matching bracket
set nowrap                      " no text wrapping
"set laststatus=2                " use 2 lines for the status bar
"set matchtime=2                 " show matching bracket for 0.2 seconds
"set matchpairs+=<:>             " specially for html

" toggle line number display
map <F6> :set invnumber<CR>

" create a new tab
noremap <C-t> <Esc>:tabnew<CR>

" turns a split window into it's own tab
" noremap <S-T> <Esc><C-w>T

" move left/right between tabs
" noremap <C-h> <Esc>gT<CR>
" noremap <C-l> <Esc>gt<CR>
" nmap <C-T> <Esc>gT<CR> "not sure why this doesn't work with noremap <C-t> ...

" close tab - conflict with C-ww
" map <C-W> :q<CR>


" -------------------------------------------------------------------------
" INDENTATION
" =========================================================================

set autoindent                  " auto-indentation
set pastetoggle=<F3>            " toggle on/off paste mode. Turn on paste-mode when pasting already-indented text
set tabstop=4
"set softtabstop=4              " indicate the number of spaces to insert instead of tab when pressing Tab (in Insert mode)
"set expandtab                  " turn a tab into spaces
"set lazyredraw                 " no redraw in macros
set shiftwidth=4                " nuber of spaces to use for auto-indenting when using << and >>
set smarttab                    " insert tabs on the start of lines according to shiftwidth, not tabstop


" -------------------------------------------------------------------------
" EDITING
" =========================================================================

set backspace=indent,eol,start  " allow backspacing over everything in insert mode


" -------------------------------------------------------------------------
" MOUSE/SCROLLING
" =========================================================================
"
" Scrolling with mouse
" Note : it will affect cpoy/paste with mouse i.e. being changed to visual mode
"set mouse=a " - to be enabled when needed
map <ScrollWheelUp> 3<C-Y>
map <scrollWheelDown> 3<C-E>

" Smooth scrolling with Ctrl-U and Ctrl-D
map <C-U> <C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y>
map <C-D> <C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E>

" Scroll horizontally
set sidescroll=1                " scrolling horizontally by 1 each time the cursor move
set sidescrolloff=4             " minimum distance between cursor and terminal's right-border

                                " scroll 10 characters to the right
map <C-L> 10zl
                                " scroll 10 characters to the left
map <C-H> 10zh

" Scroll vertically
set scrolloff=2                 " 2 lines above/below cursor when scrolling up/down


" -------------------------------------------------------------------------
" DIFF
" =========================================================================

" Showing diff between last saved version and current unsaved version
nmap <F4> :w !diff % - <CR>


" -------------------------------------------------------------------------
" MISCELLANEOUS
" =========================================================================
"
" Ctrl-Backspace to delete the previous word
" Note : work in gvim, may not work in terminal vim
imap <C-BS> <C-W>

set visualbell                  " don't beep, I don't have a speaker
set noerrorbells                " don't beep too, my PC is mute

" you guess what it does >:)
nnoremap ; :


" -------------------------------------------------------------------------
" PLUG-IN
" =========================================================================

" NERDTree
map <F2> :NERDTreeToggle<CR>

" CtrlP
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_max_files = 0
let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_show_hidden = 1
map <C-P><C-M> :CtrlPMixed<CR>
map <C-P><C-M><C-U> :CtrlPMRU<CR>

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*        " Linux/MacOSX

" CtrlP-filetype
let g:ctrlp_extensions = ['filetype']
silent! nnoremap <unique> <silent> <Leader>f :CtrlPFiletype<CR>
" --> this means \f = :CtrlPFiletype<CR>

" TComment
map <c-/> :TComment
"this is similar to --> map <c-/><c-/> :TComment
map // :TComment<CR>

map <c-/>b :TCommentBlock
map <c-/>t :TCommentRight

" Tagbar
nmap <F7> :TagbarToggle<CR>
let g:tagbar_left = 0
let g:tagbar_autofocus = 0
let g:tagbar_show_visibility = 1
let g:tagbar_autopreview = 0
" autocmd VimEnter * nested :call tagbar#autoopen(1)
