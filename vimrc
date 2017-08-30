
set mouse=a

"буфер иксов
if has('unnamedplus')
    set clipboard=unnamedplus
else
    set clipboard=unnamed
endif

set completeopt=menu

set cindent shiftwidth=4

set wrap
set linebreak
set tw=100
set expandtab
set sw=4

set modeline

set hidden

set laststatus=2

set hlsearch            " Highlight
set incsearch		" Incremental search

set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.

"virtualedit in VISUAL BLOCK mode
set virtualedit=block

if v:version >= 703
    set undofile
if has('nvim')
    set undodir=~/.undodir_nvim
else
    set undodir=~/.undodir
endif

    set colorcolumn=+1
endif

set langmap=йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ;qwertyuiop[]asdfghjkl\;\'zxcvbnm\\,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>

"------------------------------------------------------------------------
" Detection
"------------------------------------------------------------------------

"fish
if &shell =~# 'fish$'
    set shell=sh
endif

"neovim
if has('nvim')
    let s:editor_root=expand("~/.config/nvim/")
else
    let s:editor_root=expand("~/.vim")
endif

"------------------------------------------------------------------------
" Vundle
"------------------------------------------------------------------------
set nocompatible
filetype off
let s:vundle_path = s:editor_root . '/bundle/Vundle.vim'
let &rtp = &rtp . ',' . s:vundle_path
call vundle#begin(s:editor_root . '/bundle')

Plugin 'VundleVim/Vundle.vim'

Plugin 'Gundo'
Plugin 'octave.vim--'
Plugin 'vcscommand.vim'

"Clojure
"Plugin 'tpope/vim-leiningen'
"Plugin 'tpope/vim-classpath'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-dispatch'
Plugin 'guns/vim-clojure-static'
Plugin 'guns/vim-clojure-highlight'
Plugin 'paredit.vim'

Plugin 'Shougo/vimproc'
Plugin 'Shougo/neocomplete'
Plugin 'Shougo/unite.vim'
Plugin 'bling/vim-airline'
Plugin 'bling/vim-bufferline'
Plugin 'int3/vim-taglist-plus'
Plugin 'scrooloose/syntastic'
Plugin 'scrooloose/nerdtree'
Plugin 'mhinz/vim-startify.git'
Plugin 'Raimondi/delimitMate'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'freitass/todo.txt-vim'
Plugin 'kshenoy/vim-signature'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'

Plugin 'derekwyatt/vim-scala'
"Plugin 'ensime/ensime-vim'
Plugin 'dscleaver/sbt-quickfix'
Plugin 'solarnz/thrift.vim'

Plugin 'pangloss/vim-javascript'
Plugin 'leafgarland/typescript-vim'

"cemetery
"Plugin 'mileszs/ack.vim'
"Plugin 'jimenezrick/vimerl'
"Plugin 'Shougo/neosnippet'
"Plugin 'kien/ctrlp.vim'
"Plugin 'Lokaltog/vim-easymotion'
"Plugin 'neilagabriel/vim-geeknote'
"Plugin 'YankRing.vim'
"Plugin 'ReplaceWithRegister'
"Plugin 'perl-support.vim'

"coloschemes
Plugin 'larssmit/vim-getafe'
if has('nvim')
Plugin 'frankier/neovim-colors-solarized-truecolor-only'
else
Plugin 'altercation/vim-colors-solarized'
endif
Plugin 'tomasr/molokai'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'djjcast/mirodark'
Plugin 'bcat/abbott.vim'
Plugin 'sjl/badwolf'
Plugin 'baycomb'
Plugin 'morhetz/gruvbox'


call vundle#end()

filetype plugin indent on
syntax on

"------------------------------------------------------------------------
"  syntastic
"------------------------------------------------------------------------

"let g:syntastic_perl_checkers = ['perl', 'perlcritic', 'podchecker']
let g:syntastic_perl_checkers = ['perl', 'podchecker']
let g:syntastic_enable_perl_checker = 1

let g:syntastic_scala_checkers = ['fsc']

"------------------------------------------------------------------------
"  clojure
"------------------------------------------------------------------------

let g:paredit_mode = 1

"------------------------------------------------------------------------
"  delimitMate
"------------------------------------------------------------------------

au FileType clojure let b:loaded_delimitMate = 1
let delimitMate_expand_cr = 1

"------------------------------------------------------------------------
"  NERDTree
"------------------------------------------------------------------------

map <tab> :NERDTreeToggle<CR>

"------------------------------------------------------------------------
"  Unite
"------------------------------------------------------------------------
call unite#filters#matcher_default#use(['matcher_fuzzy'])

let g:unite_data_directory='~/.unite_cache'
"let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable=1

if executable('ag')
    let g:unite_source_grep_command='ag'
    let g:unite_source_grep_default_opts='--nocolor --nogroup -S -C4'
    let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
    let g:unite_source_grep_command='ack'
    let g:unite_source_grep_default_opts='--no-heading --no-color -k'
    let g:unite_source_grep_recursive_opt=''
endif

"nmap <space> [unite]
nmap , [unite]
nnoremap [unite] <nop>

nnoremap <silent> <C-t> :<C-u>Unite -toggle -auto-resize -buffer-name=files -start-insert file_rec/async<cr>
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<cr>
nnoremap <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line -start-insert line<cr>
nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers -quick-match buffer<cr>
"nnoremap <silent> [unite]m :<C-u>Unite -auto-resize -buffer-name=buffers -quick-match file_mru<cr>
nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>

"------------------------------------------------------------------------
"  Neocomplete
"------------------------------------------------------------------------
let g:neocomplete#enable_at_startup = 1

let g:neocomplete#force_overwrite_completefunc = 1

let g:neocomplete#sources#syntax#min_keyword_length = 3

let g:EclimCompletionMethod = "omnifunc"

if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_omni_input_patterns.java = '\k\.\k*'
let g:neocomplete#force_omni_input_patterns.scala = '\k\.\k*'

"------------------------------------------------------------------------
"  Colors & Appearance
"------------------------------------------------------------------------

set background=dark
"let g:lucius_contrast="high"

"256 цветов
set t_Co=256

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"colorscheme lucius
"colorscheme solarized
let g:molokai_original = 1
colorscheme molokai

"курсор не мигает
let &guicursor = &guicursor . ",a:blinkon0"

"для статусной строки
set ttimeoutlen=50

"remove scrollbar from gvim
set guioptions+=RLlrb
set guioptions-=RLlrb

" Remove menu bar
set guioptions-=m

" Remove toolbar
set guioptions-=T

"------------------------------------------------------------------------
" Autocmd
"------------------------------------------------------------------------
"при открытии файла прыгает к последнему месту
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif

"расширения -> типы файлов
au BufNewFile,BufRead *.exp         setfiletype tcl
au BufNewFile,BufRead Make*         setfiletype make
au BufNewFile,BufRead *.ocaml       setfiletype ocaml
au BufNewFile,BufRead *.octave,*.m  setfiletype octave
au BufNewFile,BufRead *.txt         setfiletype wiki

au FileType c set omnifunc=ccomplete#Complete
au FileType html set omnifunc=htmlcomplete#CompleteTags
au FileType javascript set omnifunc=javascriptcomplete#CompleteJS

"------------------------------------------------------------------------
"  Maps
"------------------------------------------------------------------------

"выход по q
noremap Q q
noremap q :q<CR>

cmap ц w
cmap й q

if bufwinnr(1)
  map + <C-w>+
  map - <C-w>-
endif

imap <C-d> <DEL>

"Screen movement
map j gj
map k gk

nnoremap Y y$

"repeat last command for each line in visual block (Tip#84)
vnoremap . :normal .<CR>

"make <c-l> clear the highlight as well as redraw
nnoremap <silent> <Esc><Esc> :nohlsearch<CR><Esc>

inoremap <C-J> <esc>

"-----------------------------------------------------------------------
"  F-keys
"------------------------------------------------------------------------

"<F1> - помощь
"<F2> - регистры
"<F5> - Gundo
"<F6> - открыть ссылку в браузере
"<F7> - проверка орфографии
"<F8> - кодировка
"<F9> - компиляция/интерпретация

map  <F1> :he <C-r><C-w><CR>
map  <F2> :reg<CR>

map  <F5> :GundoToggle<CR>
map  <F6> :!urlview %<cr>
map  <F7> :call ChangeSpellLang()<CR>

set wildmenu
set wcm=<Tab>
menu Encoding.utf-8 :e ++enc=utf8 <CR>
menu Encoding.windows-1251 :e ++enc=cp1251<CR>
menu Encoding.koi8-r :e ++enc=koi8-r<CR>
menu Encoding.cp866 :e ++enc=cp866<CR>
map <F8> :emenu Encoding.<TAB>

"-----------------------------------------------------------------------
"  Leader
"------------------------------------------------------------------------
"
nmap ,1  :b1<cr>
nmap ,2  :b2<cr>
nmap ,3  :b3<cr>
nmap ,4  :b4<cr>
nmap ,5  :b5<cr>
nmap ,6  :b6<cr>
nmap ,7  :b7<cr>
nmap ,8  :b8<cr>
nmap ,9  :b9<cr>
nmap ,n  :bn<cr>
nmap ,p  :bp<cr>

set pastetoggle=<leader>p

"-----------------------------------------------------------------------
"  Launchers
"------------------------------------------------------------------------

au FileType c map <leader>rr :!gcc -lm % && ./a.out<CR>
au FileType octave map <leader>rr :!octave -q %<CR>
au FileType octave vmap <leader>rr y:!octave -q --eval "<c-r>0"<CR>

au FileType scheme map <leader>rr :!guile %<CR>

au FileType tex map <leader>rr :!pdflatex %<CR>
au FileType tex map <leader>rs :!okular %<.pdf<CR>

"au FileType perl map <leader>rr :!scp % root@172.16.4.60:~; ssh root@172.16.4.60 perl %<CR>
au FileType perl map <leader>rr :!perl %<CR>

"-----------------------------------------------------------------------
"  sbt-quickfix
"------------------------------------------------------------------------

au FileType scala set errorformat=%E\ %#[error]\ %#%f:%l:\ %m,%-Z\ %#[error]\ %p^,%-C\ %#[error]\ %m
au FileType scala set errorformat+=,%W\ %#[warn]\ %#%f:%l:\ %m,%-Z\ %#[warn]\ %p^,%-C\ %#[warn]\ %m
au FileType scala set errorformat+=,%-G%.%#

"noremap <silent> <Leader>ff :cf target/quickfix/sbt.quickfix<CR>
"noremap <silent> <Leader>fn :cn<CR>
