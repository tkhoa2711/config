" ~/.vimrc

" -------------------------------------------------------------------------
" TIPS
" =========================================================================
"
" ___________________ SEARCH/REPLACE/DELETE
"
" Ctrl-n Ctrl-p     : complete word (insert mode)
" *                 : search current word under cursor and move to next matched
" * ` `             : search and return to current word
" #                 : previous word under cursor
" /word             : search 'word' from top
" ?word             : search 'word' from bottom
" %s/old/new/g[w]   : replace all occurences [with confirmation]
" 5,$s/old/new/g    : replace all occurences from line 5 to EOF
" .,+5s/old/new/g   : replace all occurences from current line and 5 lines after that
" .,9[g|v]/foo/d    : delete any lines [containing|not containing] 'foo' from
"                     the current line through the next 9 lines
" %g/foo/m$         : move all lines containing 'foo' to the end of file
" g[d|D]            : jump to the local/global declaration of a variable
" cw                : delete current word
"
" ___________________ FORMAT
"
" V[u|U]            : lower|upper-case whole line
" g~~               : invert case of whole line
" %!fmt             : align all lines
" retab             : change all existing tab characters to match current tab settings
" gq                : format the current line
" gqap              : format the current paragraph
" gg=G              : indent the whole file
"
" ___________________ MOVING SCREEN/CURSOR
"
" [n]h|l            : move [n] character(s) left/right
" [n]j|k            : move [n] row(s) down/up
" [n]w|b            : move to beginning of [n] next/previous word(s)
" 0                 : move to beginning of line
" $                 : move to end of line
" ^                 : first non-blank character of line
" [gg|G]            : move to first/last line
" [H|M|L]           : move to top/middle/bottom of screen
" Ctrl-[Y|E]        : move the screen up/down by 1 row
" Ctrl-[f|b]        : move forward/backward 1 screen i.e. page down/up
" Ctrl-[d|u]        : move forward/backward 1/2 screen
" z[h|l]            : move the screen to left/right
" %                 : jump to the matching open/close brace
"
" ___________________ INSERTING/APPENDING TEXT - INSERT MODE
"
" I                 : enter insert mode at beginning of the line
" a                 : append after the cursor
" A                 : append at the end of the line
" ea                : append at end of word
" [o|O]             : open new line below/above the current line and enter insert mode
"
" ___________________ WINDOW/TAB
"
" edit              : open new file in the current tab/window
" tabedit           : open new file in a new tab
" :[sp|vs]          : open new file in a horizontal/vertical split
" Ctrl-ww           : switch between split planes
" Ctrl-wq           : close the current pane
" g[t|T]            : move to next/previous tab
"
" ___________________ MARK/REGISTER
"
" :marks            : list all marks
" m[a-zA-Z]         : set mark [a-zA-Z] at current location
" ['|`]x            : move to [beginning of line/exact location] of mark x
" [d|y]x            : cut/copy the from mark x to the next mark
" [d|y]["|']x       : cut/copy from the current line/cursor position to that of mark x
" {|}               : move to the beginning/end of current paragraph
" { [d|y]}          : cut/copy a paragraph at current cursor
" y}                : copy paragraph if already at the first line of the paragraph
" y{                : copy paragraph if already at the last line of the paragraph
" "ay/foo           : yank a copy of text from current cursor to the next line
"                     containing string 'foo' to register 'a'
" "aP               : paste a copy from 'a' register before the current line
"
"____________________ SPELL CHECKING
"
" ]s                : move to the next mispelled word
" [s                : move to the previous mispelled word
" zg                : add a word to the dictionary
" zug               : undo the addition of a word to the dictionary
" z=                : view spelling suggestions for a mispelled word
"
"____________________ MISC
"
" :w !diff % -      : show diff of last saved version with current unsaved version
" :colorscheme      : show current colorscheme
" :so $MYVIMRC      : reload (source) vim config
" :so %             : source current buffer
" :set <op>?        : display the option value
" make              : built-in make
" X                 : encrypt current buffer with prompted password
" u                 : undo
" Ctrl-R            : redo
" .                 : repeat last change
" @:                : repeat last command
" J                 : join current line with the line below
" q[a-z]            : start recording macro into register [a-z]
" [N]@[a-z]         : repeat the action of macro stored inside register [a-z] N times
" @@                : repeat the previous macro
"
"____________________ vim-trailing-whitespace
"
" :FixWhitespace    : fix trailing white space error
"
"____________________ NERDTree
"
" gt|gT             : navigate forward|backward between tabs
" Ctrl-ww           : switch between windows
" s                 : open in vertical split window
" t|T               : open in new[silent] tab
" ?                 : show help
" m                 : show menu for creating/editing files and directories
"
"____________________ CtrlP
"
" Ctrl-d            : switch to search by filename only instead of full path
" <F5>              : purge the cache
" Ctrl-r            : switch to regexp mode
" Ctrl-z            : mark/unmark files
" Ctrl-o            : open marked files
" Ctrl-t|v|x        : open selected entry in new tab|split
" Ctrl-w            : switch between windows
"
"____________________ Lawrencium
"
" :Hgvdiff          : diff in vertical split windows
" :Hgstatus
"
"____________________ Tagbar
"
" p                 : jump to the tag under cursor, stay in Tagbar window
" P                 : open the tag in a preview window
" <Space>           : display the prototype of the current tag in command line
" q                 : close Tagbar window
"
" -------------------------------------------------------------------------
" MAPPING SUMMARY
" =========================================================================
" <F1>  :help [vim default]
" <F2>  :NERDTreeToggle<CR>
" <F3>  :pastetoggle
" <F4>  :w !diff % -
" <F5>  purge CtrlP cache
" <F6>
" <F7>  :TagbarToggle<CR>
" <F8>  list buffers and select one
" <F9>  :make
" ________________________________________________________________________


" -------------------------------------------------------------------------
" SETTINGS
" =========================================================================

if &compatible                  " only if not set before
    set nocompatible            " use vim-defaults instead of vi-defaults (more user friendly)
endif
set ttyfast
set confirm                     " get a dialog when :q, :w or :wq fails

let mapleader = "\<Space>"      " Power!!!


" -------------------------------------------------------------------------
" PLUG-IN
" =========================================================================
"
" Use pathogen to easily modify the runtime path
" and include all plugins under ~/.vim/bundle directory
"call pathogen#runtime_append_all_bundles() ; looks like this is obsolete
" execute pathogen#infect()
" call pathogen#helptags()        " generate helptags for everything in 'runtimepath'

" setup Vundle
filetype off                    " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'VundleVim/Vundle.vim'

" plugin list
Plugin 'bronson/vim-trailing-whitespace'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'endel/ctrlp-filetype.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'lambdatoast/elm.vim'
Plugin 'lilydjwg/colorizer'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
Plugin 'terryma/vim-expand-region'
Plugin 'thoughtbot/vim-rspec'
Plugin 'tkhoa2711/vim-togglenumber'
Plugin 'tomtom/tcomment_vim'
Plugin 'townk/vim-autoclose'
Plugin 'pangloss/vim-javascript'
Plugin 'vim-scripts/closetag.vim'

" All of your Plugins must be added before the following line
call vundle#end()               " required
filetype plugin indent on       " required


" -------------------------------------------------------------------------
" HISTORY
" =========================================================================

set hidden                      " it hides buffers instead of closing them
                                " you can have unwritten changes to a file
                                " and be able to open new file with :e
                                " undo buffers and marks are preserved while the buffer is opened
set history=10000

" TODO organize functions into one place
function! MkDir(dir)
    let dir=a:dir
    if has('win32')
        silent call system('mkdir ' . dir)
    else
        silent call system('mkdir -p ' . dir)
    endif
endfunction

if has('persistent_undo')       " version >= 703
    set undodir=~/.vim/undodir//
    call MkDir(&undodir)
    set undofile
    set undolevels=1000
    set undoreload=10000        " maximum number lines to save for undo on a buffer reload
endif

"set noswapfile                 " enable noswapfile will allow editing file in multiple instances of vim
                                " and if vim crashes, we won't have backup
set nobackup                    " never let vim write a backup file, they did that in the 70's. Well..
set backupdir=~/.vim/backup//
call MkDir(&backupdir)
set directory=~/.vim/swp//
call MkDir(&directory)

" autosave on losing focus
" :au FocusLost * silent! wa


" -------------------------------------------------------------------------
" SYNTAX HIGHLIGHTING
" =========================================================================

syntax on
set t_Co=256
try
    colorscheme 256-grayvim
catch
    " install it later
endtry

" highlight tab characters, trailing whitespaces and invisible spaces visually
" use # at end of line to mark lines that extend off-screen
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.
autocmd filetype javascript,html,xml set listchars-=tab:>.      " not showing tabs for these filetypes

" toggle displaying of whitespace character
nnoremap <Leader><Leader> :set list!<CR>

" force syntax highlighting for these files
au BufRead,BufNewFile .bash_* set filetype=sh
au BufRead,BufNewFile *.asd set filetype=lisp
au BufRead,BufNewFile *.lsp set filetype=newlisp

" configure spacing for indentation
au filetype javascript,json,html,css setl sw=2 sts=2 et

" -------------------------------------------------------------------------
" SEARCHING
" =========================================================================

set incsearch                   " do incremental searching, the matches shown while typing
set hlsearch                    " highlight search pattern matches
                                " clear search buffer with ,/
nmap <silent> ,/ :nohlsearch<CR>
set ignorecase                  " case-insensitive searching
set smartcase                   " ignore case in searching unless an upppercase letter is used

" center the cursor after jumping to the next/previous match
nnoremap n nzz
nnoremap N Nzz


" -------------------------------------------------------------------------
" DISPLAYING
" =========================================================================

set showmode                    " show mode in status bar
set showcmd                     " display typed command in status bar
set ruler                       " show cursor position in status bar
set sm                          " show matching bracket
set nowrap                      " no text wrapping
"set matchtime=2                 " show matching bracket for 0.2 seconds
"set matchpairs+=<:>             " specially for html

" toggle spell checking
nnoremap <Leader>s :set spell!<CR>

" turn on word completion
set complete+=kspell


" -------------------------------------------------------------------------
" WINDOWS
" =========================================================================

" create a new tab with Ctrl-T
noremap <C-t> <Esc>:tabnew<CR>

" turns a split window into it's own tab
" noremap <S-T> <Esc><C-w>T

" move left/right between tabs
" noremap <C-h> <Esc>gT<CR>
" noremap <C-l> <Esc>gt<CR>
" nmap <C-T> <Esc>gT<CR> "not sure why this doesn't work with noremap <C-t> ...

" close tab - conflict with C-ww
" map <C-W> :q<CR>

" faster way to open a vertical buffer and switch to it
nnoremap <Leader>w <C-w>v<C-w>l

" open new split pane in a more natural way
set splitbelow
set splitright

" display a list of buffers for selection
nnoremap <F8> :buffers<CR>:buffer<Space>

" set the listing style for NetRW window
let g:netrw_liststyle=3

" close the buffer without closing the window
nmap <Leader>d :b#<bar>bd#<CR>


" -------------------------------------------------------------------------
" INDENTATION
" =========================================================================

set autoindent                  " auto-indentation
set pastetoggle=<F3>            " toggle on/off paste mode. Turn on paste-mode when pasting already-indented text
set tabstop=4
"set softtabstop=4              " indicate the number of spaces to insert instead of tab when pressing Tab (in Insert mode)
set expandtab                   " turn a tab into spaces
                                " use Ctrl-V <tab> to insert real tab character
"set lazyredraw                 " no redraw in macros
set shiftwidth=4                " number of spaces to use for auto-indenting when using << and >>
set smarttab                    " insert tabs on the start of lines according to shiftwidth, not tabstop

if has("autocmd")
    " enable filetype-based indentation
    " use indentation scripts locate in the 'indent' directory
    filetype plugin indent on

    au filetype html setl ts=2 sts=2 sw=2 et
    au filetype xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
endif

" toggle on/off paste mode. Turn on paste-mode when pasting already-indented text
set pastetoggle=<F3>

" easy paste-toggling - <F3> doesn't seem to work correctly in tmux
nnoremap <Leader>p :set invpaste paste?<CR>


" -------------------------------------------------------------------------
" EDITING
" =========================================================================

set backspace=indent,eol,start  " allow backspacing over everything in insert mode

" use Q for formatting the current paragraph (or selection in visual mode)
vmap Q gq
nmap Q gqap


" -------------------------------------------------------------------------
" MOUSE/SCROLLING
" =========================================================================
"
" Scrolling with mouse
" Note : it will affect copy/paste with mouse i.e. being changed to visual mode
"set mouse=a " - to be enabled when needed
map <MiddleMouse> <Nop>
map <ScrollWheelUp> 3<C-Y>
map <scrollWheelDown> 3<C-E>

" Smooth scrolling with Ctrl-U and Ctrl-D
map <C-U> <C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y>
map <C-D> <C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E>

" Scroll horizontally
set sidescroll=1                " scrolling horizontally by 1 each time the cursor move
set sidescrolloff=4             " minimum distance between cursor and terminal's right-border

" Scroll 10 characters to the right
map <C-L> 10zl

" Scroll 10 characters to the left
map <C-H> 10zh

" Scroll vertically
set scrolloff=2                 " 2 lines above/below cursor when scrolling up/down

" move cursor to the next row in the editor, not the next line, when text wrapping is enabled
nnoremap j gj
nnoremap k gk


" -------------------------------------------------------------------------
" DIFF
" =========================================================================
"
" Showing diff between last saved version and current unsaved version
nmap <F4> :w !diff % - <CR>


" -------------------------------------------------------------------------
" TAGS
" =========================================================================
"
"set tags+=$HOME/.ctags/build_tags
"set tags+=$HOME/.ctags/tags


" -------------------------------------------------------------------------
" COMPILATION
" =========================================================================
"
set makeprg=make\ UNDER_VIM=1\ -j5
map <silent> <F9> :make<cr>

" quickfix ----------------------------
" navigate through the errors in quickfix window easily
map <C-j> :cn<cr>
map <C-k> :cp<cr>


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

" ignore unnecessary warning with these 2 frequently-used keys
map :W :w
map :Q :q

" use w!! to edit a file that need root privileges after already opened it
cmap w!! w !sudo tee % >/dev/null

" statusline --------------------------
set laststatus=2                " use 2 lines for the status bar

set statusline=                 " clear the statusline when vimrc is reloaded
set statusline+=%f              " file path
set statusline+=%m%r%h%w        " modified/readonly/help/? flag
set statusline+=\ %y\           " filetype
set statusline+=enc:%{&enc}     " encoding
set statusline+=%=              " split point for left/right justification
set statusline+=line:\ %4.l     " current line number, width is at least 4 chars
set statusline+=/%L             " total number of lines in buffer
set statusline+=\
set statusline+=col:%2c         " current column number
set statusline+=\
set statusline+=ch:%3.b         " current character

" highlight color for statusline
hi StatusLine term=reverse ctermfg=236 ctermbg=158
if version >= 700
    au InsertEnter * hi StatusLine term=reverse ctermfg=236 ctermbg=209
    au InsertLeave * hi StatusLine term=reverse ctermfg=236 ctermbg=158
endif

if has("autocmd")
    " Put these in an autocmd group, so that we can delete them easily.
    augroup vimrcEx
    au!

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    " Also don't do it when the mark is in the first line, that is the default
    " position when opening a file.
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif

    " source the vimrc file after saving it
    autocmd BufWritePost .vimrc source $MYVIMRC

    augroup END

    " hightlight TODO and alike keywords
    augroup HighlightTODO
        autocmd!
        autocmd WinEnter,VimEnter * :silent! call matchadd('Todo', 'TODO\|FIXME\|NOTE\|HACK\|XXX', -1)
    augroup END
else
    set autoindent
endif " has("autocmd")


" -------------------------------------------------------------------------
" PLUG-IN
" =========================================================================
"
" NERDTree ----------------------------
map <F2> :NERDTreeToggle<CR>
let NERDTreeWinSize     = 50
let NERDTreeQuitOnOpen  = 0     " close the tree window after opening a file
let NERDTreeDirArrows   = 0     " use +/~ chars when displaying directories instead of arrows
let NERDTreeShowHidden  = 1     " show hidden files

" CtrlP -------------------------------
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_max_files = 0
let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_show_hidden = 1
map <C-P><C-M> :CtrlPMixed<CR>
map <C-P><C-M><C-U> :CtrlPMRU<CR>

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*        " Linux/MacOSX

" CtrlP-filetype ----------------------
let g:ctrlp_extensions = ['filetype']
silent! nnoremap <unique> <silent> <Leader>f :CtrlPFiletype<CR>
" --> this means \f = :CtrlPFiletype<CR>

" TComment ----------------------------
" TODO make this work in insert mode
map <c-/> :TComment
"this is similar to --> map <c-/><c-/> :TComment
map // :TComment<CR>

map <c-/>b :TCommentBlock
map <c-/>t :TCommentRight

" Tagbar ------------------------------
nmap <F7> :TagbarToggle<CR>
let g:tagbar_left = 0
let g:tagbar_autofocus = 0
let g:tagbar_show_visibility = 1
let g:tagbar_autopreview = 0
" autocmd VimEnter * nested :call tagbar#autoopen(1)

" vim-expand-region -------------------
" region expanding with just 'v' and 'C-v'
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" vim-javascript ----------------------
let g:javascript_plugin_jsdoc = 1   " enable syntax highlighting for JSDocs
let g:javascript_plugin_ngdoc = 1   " additional syntax highlighting for NGDocs
"
