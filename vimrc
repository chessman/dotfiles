
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

set ignorecase
set smartcase

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
" vim-plug
"------------------------------------------------------------------------
"
""
" Install vim-plug if not found
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

call plug#begin()

"Clojure
"Plug 'tpope/vim-leiningen'
"Plug 'tpope/vim-classpath'
"Plug 'tpope/vim-fireplace'
"Plug 'tpope/vim-dispatch'
"Plug 'guns/vim-clojure-static'
"Plug 'guns/vim-clojure-highlight'
"Plug 'vim-scripts/paredit.vim'
"
Plug 'bling/vim-airline'
"Plug 'bling/vim-bufferline'
"Plug 'int3/vim-taglist-plus'
"Plug 'scrooloose/syntastic'
"Plug 'scrooloose/nerdtree'
Plug 'mhinz/vim-startify'
"Plug 'Raimondi/delimitMate'
"Plug 'ntpeters/vim-better-whitespace'
"Plug 'freitass/todo.txt-vim'
"Plug 'kshenoy/vim-signature'
"Plug 'tpope/vim-repeat'
"Plug 'tpope/vim-surround'
"
"Plug 'derekwyatt/vim-scala'
"Plug 'dscleaver/sbt-quickfix'
"Plug 'solarnz/thrift.vim'
"
"Plug 'pangloss/vim-javascript'
"Plug 'leafgarland/typescript-vim'

"cemetery
"Plug 'Gundo'
"Plug 'vcscommand.vim'
"Plug 'mileszs/ack.vim'
"Plug 'jimenezrick/vimerl'
"Plug 'Shougo/neosnippet'
"Plug 'kien/ctrlp.vim'
"Plug 'Lokaltog/vim-easymotion'
"Plug 'neilagabriel/vim-geeknote'
"Plug 'YankRing.vim'
"Plug 'ReplaceWithRegister'
"Plug 'perl-support.vim'

"coloschemes
"Plug 'larssmit/vim-getafe'
"if has('nvim')
"Plug 'frankier/neovim-colors-solarized-truecolor-only'
"else
"Plug 'altercation/vim-colors-solarized'
"endif
Plug 'tomasr/molokai'
"Plug 'jonathanfilip/vim-lucius'
"Plug 'djjcast/mirodark'
"Plug 'bcat/abbott.vim'
"Plug 'sjl/badwolf'
"Plug 'morhetz/gruvbox'
"
Plug 'neoclide/coc.nvim'
Plug 'towolf/vim-helm'

call plug#end()

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

"-----------------------------------------------------------------------
"  coc.nvim
"------------------------------------------------------------------------

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> <M-.> <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nmap <M-,> <C-o>

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <C-d> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <C-d> <Plug>(coc-range-select)
xmap <silent> <C-d> <Plug>(coc-range-select)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
