" Section: Options {{{1
set nocompatible

let s:iswin = has('win32') || has('win64')
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc('~/.vim/bundle')
" Bundle/Vundle {{{2
Bundle 'git://github.com/tyru/operator-camelize.vim.git'
Bundle 'https://github.com/teramako/jscomplete-vim.git'
Bundle 'git://github.com/myusuf3/numbers.vim.git'
Bundle 'https://github.com/Lokaltog/vim-easymotion.git'
Bundle 'https://github.com/nono/vim-handlebars.git'
Bundle 'https://github.com/sjl/vitality.vim.git'
Bundle 'https://github.com/benmills/vimux.git'
Bundle 'https://github.com/pgr0ss/vimux-ruby-test.git'
Bundle 'https://github.com/kana/vim-tabpagecd.git'
Bundle 'https://github.com/Shougo/neocomplcache.git'
Bundle 'https://github.com/michaeljsmith/vim-indent-object.git'
Bundle 'https://github.com/bbommarito/vim-slim.git'
Bundle 'https://github.com/the-isz/MinYankRing.vim.git', {'v': 'events'}
Bundle "https://github.com/mattn/vdbi-vim"
Bundle 'git@github.com:travisjeffery/vim-rails-find-partial.git'
Bundle 'https://github.com/kikijump/tslime.vim.git'
Bundle 'https://github.com/kana/vim-smartinput.git'
Bundle 'https://github.com/ecomba/vim-ruby-refactoring.git'
Bundle 'https://github.com/sunaku/vim-ruby-shoulda-context.git'
Bundle 'https://github.com/tangledhelix/vim-octopress.git'
Bundle 'https://github.com/Raimondi/vim_search_objects.git'
Bundle 'https://github.com/kana/vim-gf-diff.git'
Bundle 'https://github.com/kana/vim-gf-user.git'
Bundle 'https://github.com/kana/vim-textobj-line.git'
Bundle 'https://github.com/groenewege/vim-less.git'
Bundle 'https://github.com/tpope/vim-eunuch.git'
Bundle 'https://github.com/tyru/current-func-info.vim.git'
Bundle 'https://github.com/digitaltoad/vim-jade.git'
Bundle 'https://github.com/juvenn/mustache.vim.git'
Bundle 'https://github.com/tpope/vim-markdown.git'
Bundle 'https://github.com/mattn/sonictemplate-vim.git'
Bundle 'https://github.com/tyru/open-browser.vim.git'
Bundle 'https://github.com/mattn/webapi-vim.git'
Bundle 'https://github.com/gregsexton/MatchTag.git'
Bundle 'https://github.com/mattn/lisper-vim.git'
Bundle 'https://github.com/tpope/vim-rvm.git'
Bundle 'https://github.com/thinca/vim-ref.git'
Bundle 'https://github.com/ujihisa/quicklearn.git'
Bundle 'https://github.com/vim-scripts/ReplaceWithRegister.git'
Bundle 'https://github.com/timcharper/textile.vim.git'
Bundle 'https://github.com/vim-scripts/AnsiEsc.vim.git'
Bundle 'https://github.com/Shougo/unite-build.git'
Bundle 'https://github.com/h1mesuke/unite-outline.git'
Bundle 'https://github.com/tsukkee/unite-tag.git'
Bundle 'https://github.com/majutsushi/tagbar.git'
Bundle 'https://github.com/ujihisa/vital.vim.git'
Bundle 'git@github.com:travisjeffery/vim-extradite.git'
Bundle 'https://github.com/kchmck/vim-coffee-script.git'
Bundle 'git://github.com/Shougo/vim-vcs.git'
Bundle 'https://github.com/vim-scripts/VimClojure.git'
Bundle 'https://github.com/vim-scripts/Align.git'
Bundle 'https://github.com/vim-scripts/SQLUtilities.git'
Bundle 'https://github.com/thinca/vim-github.git'
Bundle 'https://github.com/lukaszb/vim-web-indent.git'
Bundle 'https://github.com/pangloss/vim-javascript.git'
Bundle 'https://github.com/thinca/vim-qfreplace.git'
Bundle 'https://github.com/h1mesuke/vim-alignta.git'
Bundle 'https://github.com/kien/ctrlp.vim.git'
Bundle 'https://github.com/gmarik/vundle.git'
Bundle 'https://github.com/kana/vim-altr.git'
Bundle 'git@github.com:travisjeffery/IndexedSearch.git'
Bundle 'https://github.com/mileszs/ack.vim.git'
Bundle 'https://github.com/vim-scripts/bufkill.vim.git'
Bundle 'https://github.com/duff/vim-bufonly.git'
Bundle 'git://git.wincent.com/command-t.git '
Bundle 'https://github.com/tpope/vim-fugitive.git'
Bundle 'https://github.com/sjl/gundo.vim.git'
Bundle 'https://github.com/travisjeffery/vim-help'
Bundle 'https://github.com/tpope/vim-ragtag.git'
Bundle 'git@github.com:travisjeffery/vim-rails.git'
Bundle 'https://github.com/tpope/vim-repeat.git'
Bundle 'https://github.com/csexton/rvm.vim.git'
Bundle 'https://github.com/tpope/vim-surround.git'
Bundle 'https://github.com/kana/vim-textobj-entire.git'
Bundle 'https://github.com/kana/vim-textobj-function.git'
Bundle 'https://github.com/thinca/vim-textobj-comment.git'
Bundle 'https://github.com/kana/vim-textobj-diff.git'
Bundle 'https://github.com/kana/vim-textobj-lastpat.git'
Bundle 'https://github.com/nelstrom/vim-textobj-rubyblock.git'
Bundle 'https://github.com/kana/vim-textobj-user.git'
Bundle 'https://github.com/kana/vim-smartword.git'
Bundle 'git@github.com:travisjeffery/vim-unimpaired.git'
Bundle 'http://github.com/mattn/zencoding-vim/'
Bundle 'https://github.com/vim-scripts/ZoomWin.git'
Bundle 'https://github.com/gregsexton/gitv.git'
Bundle 'https://github.com/jgdavey/vim-blockle.git'
Bundle 'https://github.com/travisjeffery/vim-colors.git'
Bundle 'https://github.com/kana/vim-grex.git'
Bundle 'https://github.com/kana/vim-operator-user.git'
Bundle 'https://github.com/kana/vim-operator-replace.git'
Bundle 'https://github.com/tpope/vim-commentary.git'
Bundle 'https://github.com/Shougo/unite.vim.git'
Bundle 'https://github.com/vim-ruby/vim-ruby.git'
Bundle 'https://github.com/tpope/vim-rake.git'
Bundle 'https://github.com/tpope/vim-bundler.git'
Bundle 'https://github.com/altercation/vim-colors-solarized.git'
Bundle 'https://github.com/thinca/vim-quickrun.git'
Bundle 'https://github.com/kana/vim-exjumplist.git'
Bundle 'https://github.com/thinca/vim-unite-history.git'
Bundle 'https://github.com/Shougo/echodoc.git'
Bundle 'https://github.com/kana/vim-altercmd.git'
Bundle 'https://github.com/mattn/gist-vim.git'
Bundle 'https://github.com/kana/vim-scratch.git'
Bundle 'https://github.com/scrooloose/syntastic.git'
Bundle 'https://github.com/Valloric/YouCompleteMe.git'

"}}}

filetype plugin indent on

augroup MyAutoCmd
  autocmd!
augroup END

filetype on
syntax on
set lazyredraw
set autowrite
set autowriteall
set cmdheight=2
set backspace=indent,eol,start
set history=1000
set nostartofline
set splitbelow
set splitright
set previewheight=10
set helpheight=12
set noequalalways
set pumheight=20
set showfulltag
set cscopetag
if has('quickfix')
  set cscopequickfix=s-,c-,d-,i-,t-,e-
endif
set cscopeverbose
set cmdwinheight=5
set ruler
set showcmd
set showmode
set incsearch
set mousemodel=popup
set smartcase
set hlsearch
set nowrap
set linebreak
if exists('+fuoptions')
  set fuoptions=maxhorz,maxvert
endif
if exists('+guicursor')
  set guicursor&
  set guicursor=a:blinkwait4000-blinkon1500-blinkoff500
endif
if exists('+macmeta')
  set macmeta
endif
" set winwidth=84
" set winheight=5
" set winminheight=5
" set winheight=999
set undodir=~/.tmp
set undofile
set undolevels=1000
set undoreload=10000
if !isdirectory($HOME . "/.tmp")
  call mkdir($HOME . "/.tmp", 'p')
endif
set backupdir=~/.tmp
set directory=~/.tmp
set backup
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
set wildmode=list:longest
set wildmenu
set wildignore=*.o,*.obj,*~,_site,.git,.svn,*.xcodeproj,*.pyc,tmp
set virtualedit=block
set matchpairs+=<:>
set autoread
set iskeyword+=-
set iskeyword+=_
if v:version >= 600
  set foldenable
  set foldlevelstart=1
  set foldlevel=1
  set foldmethod=marker
  set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo
  set printoptions=paper:letter
  set sidescrolloff=5
  set sidescroll=1
  set mouse=nvi
endif
set nolist
if (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && version >= 700
  let &listchars = "tab:\u21e5\u00b7,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
else
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<
endif
set formatoptions-=o
" tabline{{{2
set showtabline=2
set tabline=%!MyTabLine()
function! MyTabLine()
  let s = ''
  for t in range(tabpagenr('$'))
    if t + 1 == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif
    let s .= ' '
    let s .= '%' . (t + 1) . 'T'
    let s .= t + 1 . ' '
    let n = ''
    let m = 0
    let bc = len(tabpagebuflist(t + 1))
    for b in tabpagebuflist(t + 1)
      if getbufvar( b, "&buftype" ) == 'help'
        let n .= '[H]' . fnamemodify( bufname(b), ':t:s/.txt$//' )
      elseif getbufvar( b, "&buftype" ) == 'quickfix'
        let n .= '[Q]'
      else
        let n .= pathshorten(bufname(b))
      endif
      if getbufvar( b, "&modified" )
        let m += 1
      endif
      if bc > 1
        let n .= ' '
      endif
      let bc -= 1
    endfor
    if m > 0
      let s.= '+ '
    endif
    if n == ''
      let s .= '[No Name]'
    else
      let s .= n
    endif
    let s .= ' '
  endfor
  let s .= '%#TabLineFill#%T'
  if tabpagenr('$') > 1
    let s .= '%=%#TabLine#%999XX'
  endif
  return s
endfunction"}}}2
" scroll "{{{2
set scroll=5
set scrolloff=0
let g:scrolloff = 15 
autocmd MyAutoCmd CursorMoved * call s:reinventing_scrolloff()

let s:last_lnum = -1
function! s:reinventing_scrolloff()
  if g:scrolloff ==# 0 || s:last_lnum > 0 && line('.') ==# s:last_lnum
    return
  endif
  let s:last_lnum = line('.')
  let winline     = winline()
  let winheight   = winheight(0)
  let middle      = winheight / 2
  let upside      = (winheight / winline) >= 2
  " If upside is true, add winlines to above the cursor.
  " If upside is false, add winlines to under the cursor.
  if upside
    let up_num = g:scrolloff - winline + 1
    let up_num = winline + up_num > middle ? middle - winline : up_num
    if up_num > 0
      execute 'normal!' up_num."\<C-y>"
    endif
  else
    let down_num = g:scrolloff - (winheight - winline)
    let down_num = winline - down_num < middle ? winline - middle : down_num
    if down_num > 0
      execute 'normal!' down_num."\<C-e>"
    endif
  endif
endfunction
"}}}2
" Automatically mkdir when writing file in non-existant directory{{{2
augroup vimrc-auto-mkdir
  autocmd!
  autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
  function! s:auto_mkdir(dir, force)
    if !isdirectory(a:dir)
          \   && (a:force
          \       || input("'" . a:dir . "' does not exist. Create? [y/N]") =~? '^y\%[es]$')
      call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
    endif
  endfunction
augroup END"}}}2
set mouse=a
set ttymouse=xterm2
set t_Co=256
set hidden
set textwidth=79
set ignorecase
set dictionary-=/usr/share/dict/words dictionary+=/usr/share/dict/words
set complete-=k
set complete-=i
set fileformats=unix,dos,mac
set cursorline
set relativenumber
au BufReadPost * set relativenumber
set timeoutlen=1000
set ttimeoutlen=50
set laststatus=2
set diffopt=filler
set diffopt+=iwhite
set switchbuf=useopen
set noswapfile
set updatecount=0
set updatetime=50
set showmatch
" let loaded_matchparen = 1
set matchtime=1
if has('unix')
  set nofsync
  set swapsync=
endif
set guioptions-=T
set guioptions-=L
set guioptions-=r
set guioptions+=c
set noantialias
if has('balloon_eval')
  autocmd FileType ruby,eruby set noballooneval
  set noballooneval
endif
set norestorescreen
set t_ti=
set t_te=
set shortmess=filtIoOA
set report=0
set grepprg=ack
if has("mac")
  set clipboard=unnamed
  let g:gist_clip_command = 'pbcopy'
else
  let g:gist_clip_command = 'xclip -selection clipboard'
endif
if has("gui") && has("mac")
  set fuopt+=maxhorz
  set macmeta
  set antialias
  set guifont=Inconsolata:h14
else
  set guifont=Inconsolata\ 12
endif
if &term =~ "^screen"
  augroup MyAutoCmd
    autocmd VimLeave * :set mouse=
  augroup END

  " workaround for freeze when using mouse on GNU screen.
  set ttymouse=xterm2
endif
call cfi#load()
set statusline=%f
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}
set statusline+=%{exists('g:loaded_rvm')?rvm#statusline():''}
set statusline+=%#warningmsg#
set statusline+=%{(cfi#get_func_name()!='')?'['.cfi#get_func_name().']':''}
set statusline+=%{&ff!='unix'?'['.&ff.']':''}
set statusline+=%*
set statusline+=%#warningmsg#
set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']':''}
set statusline+=%*
set statusline+=%h
set statusline+=%y
set statusline+=%r
set statusline+=%m
set statusline+=%#error#
set statusline+=%*
set statusline+=%#warningmsg#
set statusline+=%{exists('g:loaded_syntastic_plugin')?SyntasticStatuslineFlag():''}
set statusline+=%*
set statusline+=%#error#
set statusline+=%{&paste?'[paste]':''}
set statusline+=%*
set statusline+=%=
set statusline+=%c,
set statusline+=%l/%L
set statusline+=\ %{GetDocumentPosition()}
function! GetDocumentPosition()
  return float2nr(str2float(line('.')) / str2float(line('$')) * 100) . "%"
endfunction

autocmd MyAutoCmd FileType * setl formatoptions-=ro | setl formatoptions+=mM
autocmd MyAutoCmd BufWritePost vimrc source $MYVIMRC
autocmd MyAutoCmd BufWritePost .vimrc source $MYVIMRC

function! AddInstanceVariablesForArguments()
  let @a = ""
  execute 'normal! "ayib'
  if @a == ""
    " handle method definitions without parenthsis
    execute 'normal! ^2wv$h"ay'
  end

  let argsstr = @a
  let argsstr = substitute(argsstr, "^def \k*", "", "g")
  let argsstr = substitute(argsstr, " ", "", "g")

  let argslist = split(argsstr, ",")

  let l = line('.')

  let argslist = map(argslist, '"@" . argslist[v:key] . " = " . argslist[v:key]')

  call append(l, argslist)  

  execute "normal jV" . len(argslist) . "j="
endfunction

autocmd FileType ruby,eruby 
      \ nnoremap <leader>rg :<c-u>call AddInstanceVariablesForArguments()<cr>


function! Buffers()
  let tablist = []
  for i in range(tabpagenr('$'))
    call extend(tablist, tabpagebuflist(i + 1))
  endfor
  return tablist
endfunction

function! BufferNames()
  let buffers = []
  for i in range(tabpagenr('$'))
    call extend(buffers, tabpagebuflist(i + 1))
  endfor

  let buffer_names = []
  for i in buffers
    let buffer_names = buffer_names + [bufname(i)]
  endfor
  return buffer_names
endfunction

function! GrepBuffers(pattern)
  let last_bufno = bufnr("$")

  let i = 1
  let filenames = ""

  while i <= last_bufno
    if bufexists(i) && buflisted(i)
      let filenames = filenames . " " . bufname(i)
    endif
    let i = i + 1
  endwhile

  " No buffers
  if filenames == ""
    return
  endif

  execute "vimgrep /" . a:pattern . "/ "  . filenames . " | cw"
endfunction
command! -nargs=1 GrepBuffers call GrepBuffers(<q-args>)

" Close help and git window by pressing q.
autocmd FileType fugitiveblame,help,git-status,git-log,qf,gitcommit,quickrun,qfreplace,ref,simpletap-summary,vcs-commit,vcs-status
      \ nnoremap <buffer><silent> q :<C-u>call <sid>smart_close()<CR>
autocmd FileType * if (&readonly || !&modifiable) && !hasmapto('q', 'n')
      \ | nnoremap <buffer><silent> q :<C-u>call <sid>smart_close()<CR>| endif
function! s:smart_close()
  if winnr('$') != 1
    close
  endif
endfunction
augroup MyAutoCmd
  autocmd FileType vim nnoremap <silent><buffer> [Space]so :write <Bar> :source % <Bar> echo "source " . bufname('%')<CR>

  if v:version >= 700 && isdirectory(expand("~/.trash"))
    autocmd BufWritePre,BufWritePost * if exists("s:backupdir") | set backupext=~ | let &backupdir = s:backupdir | unlet s:backupdir | endif
    autocmd BufWritePre ~/*
          \ let s:path = expand("~/.trash").strpart(expand("<afile>:p:~:h"),1) |
          \ if !isdirectory(s:path) | call mkdir(s:path,"p") | endif |
        \ let s:backupdir = &backupdir |
        \ let &backupdir = escape(s:path,'\,').','.&backupdir |
        \ let &backupext = strftime(".%Y%m%d%H%M%S~",getftime(expand("<afile>:p")))
  endif

  autocmd CursorHold,BufWritePost,BufReadPost,BufLeave *
        \ if isdirectory(expand("<amatch>:h")) | let &swapfile = &modified | endif

  autocmd FileType vim  setlocal ai et sta sw=2 sts=2 keywordprg=:help
augroup END
" Make a scratch buffer when unnamed buffer{{{2
augroup vimrc-scratch-buffer
  autocmd!
  autocmd BufEnter * call s:scratch_buffer()
  autocmd FileType qfreplace autocmd! vimrc-scratch * <buffer>

  function! s:scratch_buffer()
    if exists('b:scratch_buffer') || bufname('%') != '' || &l:buftype != ''
      return
    endif
    let b:scratch_buffer = 1
    setlocal buftype=nofile nobuflisted noswapfile bufhidden=hide
    augroup vimrc-scratch
      autocmd! * <buffer>
      autocmd BufWriteCmd <buffer> call s:scratch_on_BufWriteCmd()
    augroup END
  endfunction
  function! s:scratch_on_BufWriteCmd()
    silent! setl buftype< buflisted< swapfile< bufhidden< nomodified
    autocmd! vimrc-scratch * <buffer>
    unlet! b:scratch_buffer
    execute 'saveas' . (v:cmdbang ? '!' : '') ' <afile>'
    filetype detect
  endfunction
augroup END"}}}2
"}}}1
" Section: Plugins {{{1
" EasyMotion {{{1
let g:EasyMotion_leader_key = '<Space><Space>'
"}}}
" powerline {{{2
let g:Powerline_symbols = 'fancy'
"}}}
" open-browser{{{2
let g:netrw_nogx = 1 " disable netrw's gx mapping.

let g:netrw_liststyle = 3
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)
"}}}
" Solarized{{{2
let g:solarized_termtrans=1
let g:solarized_degrade=0
let g:solarized_bold=1
let g:solarized_underline=1
let g:solarized_italic=1
let g:solarized_diffmode="normal"
let g:solarized_hitrail=0
let g:solarized_termcolor=256 
let g:solarized_visibility="high" 
let g:solarized_contrast="high" 
set background=light
colorscheme Tomorrow
" colorscheme solarized
"}}}2
" netrw {{{2
let g:netrw_list_hide= '*.swp'
" nnoremap <silent> <BS> :<C-u>Explore<CR>
" Change default directory.
set browsedir=current
if executable('wget')
  let g:netrw_http_cmd = 'wget'
endif
let g:netrw_list_hide = '^\.,^tags$'
"}}}2
" rails {{{2
let g:rails_menu=0
let g:rails_installed_menu=0
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_rails = 1
autocmd User Rails/app/presenters/*.*
    \ let b:rails_alternate = substitute(rails#buffer().path(), 'app/','test/unit/','')
autocmd User Rails/app/assets/javascripts/*.*
    \ let b:rails_alternate = fnamemodify(rails#buffer().path(),
    \ ":s?app/assets/?spec/?:r"). "_spec." . 
    \ fnamemodify(rails#buffer().path(), ":e")
autocmd User Rails/spec/javascripts/*.*
    \ let b:rails_alternate = fnamemodify(rails#buffer().path(),
    \ ":s?spec/?app/assets/?:s?_spec??") 
"}}}2
" Syntastic {{{2
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=0
let g:syntastic_auto_loc_list=1
let g:syntastic_quiet_warnings=1
let g:syntastic_javascript_checker = 'jshint'
" let g:syntastic_javascript_jslint_conf = "--white --lastsemic --eqeqeq --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars --es5=false"
"}}}2
" QuickRun {{{2
let g:quickrun_direction = 'rightbelow horizontal'
let g:quickrun_config = {}
" let g:quickrun_config['ruby'] = {'command': 'testdrb', 'exec': '%c -Itest %s'}
let g:quickrun_config['*'] = {
      \ 'runner/vimproc/updatetime' : 100,
      \ 'split': 'vertical rightbelow',
      \ 'targets' : ['quickfix', 'buffer'],
      \ 'outputter' : 'buffer',
      \ 'runner' : 'vimproc',
      \ 'into' : 0,
      \ 'runmode' : 'async:remote:vimproc'
      \}
"}}}2
" CtrlP {{{2
let g:ctrlp_working_path_mode = 0
let g:ctrlp_persistent_input = 0
let g:ctrlp_open_new_file = 0
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_match_window_bottom = 1
let g:ctrlp_jump_to_buffer = 1
let g:ctrlp_use_caching = 1
let g:ctrlp_mruf_max = 10000
let g:ctrlp_max_files = 0 
let g:ctrlp_extensions = ['tag', 'quickfix', 'dir', 'buffertag', 'line', 'change', 'undo']
let g:ctrlp_split_window = 0
" let g:ctrlp_user_command = {
"     \ 'types': {
"       \ 1: ['.git/', 'cd %s && git ls-files'],
"       \ 2: ['.hg/', 'hg --cwd %s locate -I .'],
"       \ },
"     \ 'fallback': 'find %s -type f'
"     \ }
"}}}2
"Fugitive & Gitv {{{2
nnoremap [Space]gd :<C-u>Gdiff<CR>
nnoremap [Space]gs :<C-u>Gstatus<CR>
nnoremap [Space]gl :<C-u>Glog<CR>
nnoremap [Space]gc :<C-u>Gcommit<CR>
nnoremap [Space]gC :<C-u>Git commit -- amend<CR>
nnoremap [Space]gb :<C-u>Gblame<CR>
nnoremap [Space]gr :<C-u>Gread<CR>
nnoremap [Space]gw :<C-u>Gwrite<CR>
nnoremap [Space]gv :<C-u>Gitv<CR>
"}}}2
" Testee {{{2
" noremap <leader>rt <Plug>(testee-test-case)
" noremap <leader>rf <Plug>(testee-test-file)
" noremap <leader>rl <Plug>(testee-test-last)
"}}}2
" grex {{{2
nmap gD  <SID>(command-line-enter)<C-u>Gred<CR>
nmap gy  <SID>(command-line-enter)<C-u>Grey<CR>
"}}}2
" qfreplace.vim {{{2
autocmd MyAutoCmd FileType qf nnoremap <buffer> r :<C-u>Qfreplace<CR>
" Misc {{{2
let g:snips_author = "Travis Jeffery"
let g:user_zen_leader_key = '<c-e>'
let g:use_zen_complete_tag = 1
let g:ragtag_global_maps = 1
" let g:user_zen_settings = {
"       \  'indentation' : '  ',
"       \   'markdown' : { 'extends' : 'html' }
"       \}
let g:user_zen_settings = {
\  'indentation' : ' '
\}
" Unite {{{2
let g:unite_source_history_yank_enable = 1
let g:unite_enable_split_vertically = 1
let g:unite_kind_file_cd_command = 'TabpageCD'
let g:unite_kind_file_lcd_command = 'TabpageCD'
let g:unite_update_time = 50
let g:unite_winheight = 20
let g:unite_enable_start_insert = 1
let g:unite_cursor_line_highlight = 'TabLineSel'
let g:unite_abbr_highlight = 'TabLine'
let g:unite_source_file_mru_time_format = ''
let g:unite_source_file_mru_filename_format = ':~:.'
let g:unite_source_file_mru_limit = 300
let g:unite_source_directory_mru_time_format = ''
let g:unite_source_directory_mru_limit = 300
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#converter_default#use(['converter_relative_abbr'])
call unite#custom_filters('file,buffer,file_rec,file_rec/async,outline',
      \ ['converter_relative_abbr', 'converter_relative_word', 'matcher_fuzzy', 'sorter_default', 'converter_default'])
" nnoremap <C-B> :<C-u>Unite buffer<CR>
" nnoremap <C-B> :<C-u>CommandTBuffer<CR>
nnoremap <C-B> :<C-u>CtrlPBuffer<CR>
" nnoremap <C-B> :<C-u>CtrlPBuffer<CR>
nnoremap <silent> [Space]:  :<C-u>Unite history/command command<CR>
"}}}2
" echodoc {{{2
let g:echodoc_enable_at_startup = 1
"}}}2
" delimitmate {{{2
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1
let delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_balance_matchpairs = 1
let delimitMate_smart_quotes = 1
let delimitMate_smart_matchpairs = 1
let delimitMate_autoclose = 1
"}}}2
" YankRing {{{2
let g:yankring_max_history = 1000
let g:yankring_max_display = 100
let g:yankring_persist = 1
let g:yankring_share_between_instances = 1
let g:yankring_window_use_separate = 1
let g:yankring_window_use_horiz = 1
let g:yankring_window_auto_close = 1
let g:yankring_window_width = 30
let g:yankring_window_use_right = 1
let g:yankring_window_increment = 15
let g:yankring_history_dir = '$HOME/.tmp'
"}}}2
" surround {{{2
let g:surround_{char2nr('-')} = "<% \r %>"
let g:surround_{char2nr('=')} = "<%= \r %>"
let g:surround_{char2nr('8')} = "/* \r */"
let g:surround_{char2nr('s')} = " \r"
let g:surround_{char2nr('^')} = "/^\r$/"
let g:surround_indent = 1
"}}}2
" vimfiler {{{2
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_enable_clipboard = 0
" vimshell.vim"{{{
"let g:vimshell_user_prompt = "3\ngetcwd()"
"let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
"let g:vimshell_user_prompt = 'printf("%s  %50s", fnamemodify(getcwd(), ":~"), vimshell#vcs#info("(%s)-[%b]"))'
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
let g:vimshell_right_prompt = 'vcs#info("(%s)-[%b]%p", "(%s)-[%b|%a]%p")'
let g:vimshell_prompt = '% '
"let g:vimshell_environment_term = 'xterm'
" let g:vimshell_cd_command = 'TabpageCD'
let g:vimshell_split_command = ''

if s:iswin
  " Display user name on Windows.
  "let g:vimshell_prompt = $USERNAME."% "

  " Use ckw.
  let g:vimshell_use_terminal_command = 'ckw -e'
else
  " Display user name on Linux.
  "let g:vimshell_prompt = $USER."% "

  " Use zsh history.
  let g:vimshell_external_history_path = expand('~/.zsh-history')

  " call vimshell#set_execute_file('bmp,jpg,png,gif', 'gexe eog')
  " call vimshell#set_execute_file('mp3,m4a,ogg', 'gexe amarok')
  " let g:vimshell_execute_file_list['zip'] = 'zipinfo'
  " call vimshell#set_execute_file('tgz,gz', 'gzcat')
  " call vimshell#set_execute_file('tbz,bz2', 'bzcat')

  " Use gnome-terminal.
  let g:vimshell_use_terminal_command = 'gnome-terminal -e'
endif

" Initialize execute file list.
let g:vimshell_execute_file_list = {}
" call vimshell#set_execute_file('txt,vim,c,h,cpp,d,xml,java', 'vim')
let g:vimshell_execute_file_list['rb'] = 'ruby'
let g:vimshell_execute_file_list['pl'] = 'perl'
let g:vimshell_execute_file_list['py'] = 'python'
" call vimshell#set_execute_file('html,xhtml', 'gexe firefox')


autocmd MyAutoCmd FileType vimshell call s:vimshell_settings()
function! s:vimshell_settings()
  imap <buffer><silent> &  <C-o>:call vimshell#mappings#push_and_execute('cd ..')<CR>

  inoremap <buffer><expr>'  pumvisible() ? "\<C-y>" : "'"
  imap <buffer><BS>  <Plug>(vimshell_another_delete_backward_char)
  imap <buffer><C-h>  <Plug>(vimshell_another_delete_backward_char)
  inoremap <silent><expr><buffer><C-r>  unite#sources#vimshell_history#start_complete(!0)

  call vimshell#altercmd#define('g', 'git')
  call vimshell#altercmd#define('i', 'iexe')
  call vimshell#altercmd#define('t', 'texe')
  call vimshell#set_alias('l.', 'ls -d .*')
  call vimshell#set_galias('L', 'ls -l')
  call vimshell#hook#add('chpwd', 'my_chpwd', s:vimshell_hooks.chpwd)
  call vimshell#hook#add('emptycmd', 'my_emptycmd', s:vimshell_hooks.emptycmd)
  call vimshell#hook#add('preprompt', 'my_preprompt', s:vimshell_hooks.preprompt)
  call vimshell#hook#add('preexec', 'my_preexec', s:vimshell_hooks.preexec)
  " call vimshell#hook#set('preexec', [s:SID_PREFIX() . 'vimshell_hooks_preexec'])
endfunction

autocmd MyAutoCmd FileType int-* call s:interactive_settings()
function! s:interactive_settings()
  call vimshell#hook#set('input', [s:vimshell_hooks.input])
endfunction

autocmd MyAutoCmd FileType term-* call s:terminal_settings()
function! s:terminal_settings()
  inoremap <silent><buffer><expr> <Plug>(vimshell_term_send_semicolon) vimshell#term_mappings#send_key(';')
  inoremap <silent><buffer><expr> j<Space> vimshell#term_mappings#send_key('j')
  "inoremap <silent><buffer><expr> <Up> vimshell#term_mappings#send_keys("\<ESC>[A")

  " Sticky key.
  imap <buffer><expr> ;  <SID>texe_sticky_func()

  " Escape key.
  iunmap <buffer> <ESC><ESC>
  imap <buffer> <ESC>         <Plug>(vimshell_term_send_escape)
endfunction
function! s:texe_sticky_func()
  let sticky_table = {
        \',' : '<', '.' : '>', '/' : '?',
        \'1' : '!', '2' : '@', '3' : '#', '4' : '$', '5' : '%',
        \'6' : '^', '7' : '&', '8' : '*', '9' : '(', '0' : ')', '-' : '_', '=' : '+',
        \';' : ':', '[' : '{', ']' : '}', '`' : '~', "'" : "\"", '\' : '|',
        \}
  let special_table = {
        \"\<ESC>" : "\<ESC>", "\<Space>" : "\<Plug>(vimshell_term_send_semicolon)", "\<CR>" : ";\<CR>"
        \}

  if mode() !~# '^c'
    echo 'Input sticky key: '
  endif
  let char = ''

  while char == ''
    let char = nr2char(getchar())
  endwhile

  if char =~ '\l'
    return toupper(char)
  elseif has_key(sticky_table, char)
    return sticky_table[char]
  elseif has_key(special_table, char)
    return special_table[char]
  else
    return ''
  endif
endfunction

let s:vimshell_hooks = {}
function! s:vimshell_hooks.chpwd(args, context)
  if len(split(glob('*'), '\n')) < 100
    call vimshell#execute('ls')
  endif
endfunction
function! s:vimshell_hooks.emptycmd(cmdline, context)
  call vimshell#set_prompt_command('ls')
  return 'ls'
endfunction
function! s:vimshell_hooks.preprompt(args, context)
  " call vimshell#execute('echo "preprompt"')
endfunction
function! s:vimshell_hooks.preexec(cmdline, context)
  " call vimshell#execute('echo "preexec"')

  let args = vimproc#parser#split_args(a:cmdline)
  if len(args) > 0 && args[0] ==# 'diff'
    call vimshell#set_syntax('diff')
  endif

  return a:cmdline
endfunction
function! s:vimshell_hooks.input(input, context)
  " echomsg 'input'
  return a:input
endfunction
command! -nargs=0 VF :VimFiler
nnoremap <silent>   [Space]v   :<C-u>VimFiler<CR>
nmap    [Space]h   :<C-u>edit %:h<CR>
"}}}2
" SuperTab {{{2
let g:SuperTabLongestEnhanced = 1
let g:SuperTabDefaultCompletionType = "context"
"}}}2
" CommandT {{{2
let g:CommandTMatchWindowReverse  = 1
let g:CommandTMaxHeight           = 10
let g:CommandTMaxFiles            = 30000
let g:CommandTScanDotDirectories  = 1
let g:CommandTMatchWindowAtTop = 0
  let g:CommandTCancelMap=['<C-x>', '<C-c>']
"}}}2
" neocomplcache{{{2
let g:neocomplcache_enable_at_startup = 0
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 4
let g:neocomplcache_manual_completion_start_length = 0
let g:neocomplcache_min_keyword_length = 4
let g:neocomplcache_enable_auto_select = 1
let g:neocomplcache_enable_auto_delimiter = 1
let g:neocomplcache_disable_auto_select_buffer_name_pattern = '\[Command Line\]'
let g:neocomplcache_cursor_hold_i_time = 50
let g:neocomplcache_auto_completion_start_length = 4
let g:neocomplcache_max_list = 100
let g:neocomplcache_force_overwrite_completefunc = 1
let g:neocomplcache_enable_cursor_hold_i = v:version > 703 ||
      \ v:version == 703 && has('patch289')
let g:neocomplcache_dictionary_filetype_lists = {
      \ 'default' : '',
      \ 'ruby' : expand('$DOTVIM/dict/ruby.dict'),
      \ }
let g:neocomplcache_omni_functions = {
      \ 'python' : 'pythoncomplete#Complete',
      \ 'ruby' : 'rubycomplete#Complete',
      \ }
" let g:neocomplcache_cursor_hold_i_time = 300
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
let g:neocomplcache_snippets_dir = $HOME . '/snippets'
" if !exists('g:neocomplcache_omni_patterns')
"     let g:neocomplcache_omni_patterns = {}
" endif
" let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*<Bar>\h\w*::'
" if !exists('g:neocomplcache_same_filetype_lists')
"   let g:neocomplcache_same_filetype_lists = {}
" endif
let g:neocomplcache_source_look_dictionary_path = ''
let g:neocomplcache_vim_completefuncs = {
      \ 'Ref' : 'ref#complete',
      \ 'Unite' : 'unite#complete_source',
      \ 'VimShellExecute' : 'vimshell#complete#vimshell_execute_complete#completefunc',
      \ 'VimShellInteractive' : 'vimshell#complete#vimshell_execute_complete#completefunc',
      \ 'VimShellTerminal' : 'vimshell#complete#vimshell_execute_complete#completefunc',
      \ 'VimFiler' : 'vimfiler#complete',
      \}
if !exists('g:neocomplcache_plugin_completion_length')
  let g:neocomplcache_plugin_completion_length = {
        \ 'look' : 4,
        \ }
endif
"}}}2
" Clojure {{{2
let g:vimclojure#HighlightBuiltins=1
let vimclojure#FuzzyIndent = 1
"}}}2
let g:template_vim_template_dir = $HOME.'.vim/template'
let g:AutoPairs = {'(':')', '[':']', '{':'}','"':'"'}
let g:indexed_search_shortmess=1
let g:HammerQuiet=1
let g:CSApprox_verbose_level=0
if filereadable("/usr/local/bin/ctags")
  let g:tagbar_ctags_bin="/usr/local/bin/ctags"
else
  let g:tagbar_ctags_bin="/usr/bin/ctags"
endif
if filereadable($HOME . "/.githubrc")
  source $HOME/.githubrc
endif
if filereadable($HOME . "/.passwords")
  source $HOME/.passwords
endif
"}}}1
" Section: Commands {{{1
call altercmd#load()
command! -nargs=* -bang W :w<bang> <args>
command! -nargs=0 E :Explore
command! -nargs=0 DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis
command! -bar -nargs=0 SudoW   :setl nomod|silent exe 'write !sudo tee % >/dev/null'|let &mod = v:shell_error
" Rename {{{2
command! -bar -nargs=* -bang -complete=file Rename :
      \ let v:errmsg = ""|
      \ saveas<bang> <args>|
      \ if v:errmsg == ""|
      \   call delete(expand("#"))|
      \ endif
"}}}2
" Remove {{{2
command! -bar -nargs=0 -bang -complete=file Remove :
      \ let v:errmsg = ''|
      \ let s:removable = expand('%:p')|
      \ bdelete<bang>|
      \ if v:errmsg == ''|
      \   call delete(s:removable)|
      \ endif|
      \ unlet s:removable
"}}}2
" CD - alternative :cd with more user-friendly completion  "{{{2

command! -complete=customlist,s:complete_cdpath -nargs=+ CD  TabpageCD <args>
function! s:complete_cdpath(arglead, cmdline, cursorpos)
  return split(globpath(&cdpath,
        \                     join(split(a:cmdline, '\s')[1:], ' ') . '*/'),
        \            "\n")
endfunction

AlterCommand cd  CD

" TabpageCD - wrapper of :cd to keep cwd for each tabpage  "{{{2

command! -nargs=? TabpageCD
      \   execute 'cd' fnameescape(<q-args>)
      \ | let t:cwd = getcwd()
"}}}1
" Section: Autocommands {{{1
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType javascript
  \ :setl omnifunc=jscomplete#CompleteJS
let g:jscomplete_use = ['dom']
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType vim set omnifunc=syntaxcomplete#Complete
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType ruby,eruby 
      \ set omnifunc=rubycomplete#Complete |
      \ set foldmethod=expr | 
      \ set foldexpr=getline(v:lnum)=~'^\\s*#' 
      " \ exe "normal zM``" 
      " \ set foldlevel=1 | 
autocmd BufNewFile,BufRead *.coffee set ft=coffee sw=2 sts=2
autocmd BufNewFile,BufRead *.txt set ft=markdown
autocmd BufNewFile,BufRead *.css set ft=css sw=2 sts=2
autocmd BufNewFile,BufRead *.json set ft=javascript sw=2 sts=2
autocmd BufNewFile,BufRead *.rb set ft=ruby sw=2 sts=2
autocmd BufNewFile,BufRead Podfile set ft=ruby sw=2 sts=2
autocmd FileType vimperator set ft=vim
autocmd filetype svn,*commit*,markdown set spell
autocmd FileType cucumber compiler cucumber | setl makeprg=cucumber\ \"%:p\"
autocmd FileType python
      \   setl makeprg=python\ \"%:p\" 
autocmd FileType clojure
      \   setl makeprg=clj\ \"%:p\" 
autocmd User Bundler
      \ if &makeprg !~ 'bundle' | setl makeprg^=bundle\ exec\  | endif
autocmd FileType ruby
      \ compiler rubyunit | setl makeprg=ruby\ -Itest\ \"%:p\" 
" autocmd User Bundler
"       \ if &makeprg !~ 'bundle' | setl makeprg^=bundle\ exec\  | endif

autocmd BufReadPost fugitive://* set bufhidden=delete
let g:do_filetype = 0
au GUIEnter,BufAdd * if expand('<afile>') == "" | let g:do_filetype = 1 | endif
au BufEnter * if g:do_filetype | setf markdown | let g:do_filetype = 0 | endif
au BufNewFile,BufRead,BufWinEnter * if search("diff --git", 'cp', 1) | set filetype=git | endif

command! OpenChangedFiles :call OpenChangedFiles()
function! OpenChangedFiles()
  only
  let status = system('git status -s | grep "^ \?\(M\|A\)" | cut -d " " -f 3')
  let filenames = split(status, "\n")
  exec "edit " . filenames[0]
  for filename in filenames[1:]
    exec "sp " . filename
  endfor
endfunction

autocmd BufReadPost * call SetCursorPosition()
function! SetCursorPosition()
  if &filetype !~ 'svn\|commit\c'
    if line("'\"") > 0 && line("'\"") <= line("$")
      exe "normal! g`\""
      normal! zz
    endif
  end
endfunction

function! InlineVariable()
  normal "ayiw
  normal 4diw
  normal "bd$
  normal dd
  normal k$
  exec '/\<' . @a . '\>'
  exec ':.s/\<' . @a . '\>/' . @b
endfunction

function! HtmlEscape()
  silent s/&/\&amp;/eg
  silent s/</\&lt;/eg
  silent s/>/\&gt;/eg
endfunction

function! HtmlUnEscape()
  silent s/&lt;/</eg
  silent s/&gt;/>/eg
  silent s/&amp;/\&/eg
endfunction

command! -nargs=0 JSBeautify call JSBeautify()<CR>
function! JSBeautify()
  silent! exe "%! jsbeautifier --stdin"
endfunction
"}}}1
" Section: Abbreviations{{{1
cabbrev E e
cabbrev ack grep
cabbrev BUndle Bundle
cabbrev gg; Ggrep -P

" rails cabbrevs
cabbrev rm; Rmodel
cabbrev rc; Rcontroller
cabbrev rv; Rview 
cabbrev ru; Runittest
cabbrev rf; Rfunctional
cabbrev rs; Rschema

command! Ctags
      \ execute "!ctags --extra=+f --exclude=.git --exclude=log -R *"

cabbrev RModel Rmodel

autocmd FileType ruby,eruby
      \ iabbrev <buffer> rw; attr_accessor| 
      \ iabbrev <buffer> rr; attr_reader|
      \ iabbrev <buffer> ww; attr_writer

" cabbrev git Git
cabbrev ~? ~/
iabbrev Utitlies Utilities
iabbrev utitlies utilities
iabbrev init; initialize
iabbrev innit init
iabbrev innitialize initialize
iabbrev seperate separate
iabbrev teh the
iabbrev ;; =>
iabbrev shuold should
iabbrev shulod should
iabbrev PgTOols PgTools
iabbrev PgTOOls PgTools
if has('quickfix')
  cnoreabbrev <expr> csa
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs add'  : 'csa')
  cnoreabbrev <expr> csf
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs find' : 'csf')
  cnoreabbrev <expr> csk
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs kill' : 'csk')
  cnoreabbrev <expr> csr
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs reset' : 'csr')
  cnoreabbrev <expr> css
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs show' : 'css')
  cnoreabbrev <expr> csh
        \ ((getcmdtype() == ':' && getcmdpos() <= 4)? 'cs help' : 'csh')
endif
abbrev shuold should
"}}}1
" Section: Mappings {{{1
inoremap kj <Esc>
let mapleader=","
nmap  <Space>   [Space]
xmap  <Space>   [Space]
nnoremap  [Space]   <Nop>
xnoremap  [Space]   <Nop>

nnoremap ; <Nop>
xnoremap ;  <Nop>
nnoremap : <Nop>
xnoremap :  <Nop>

nnoremap <SID>(command-line-enter) :
xnoremap <SID>(command-line-enter) :
nmap ; <SID>(command-line-enter)
xmap ; <SID>(command-line-enter)
nmap : <SID>(command-line-enter)
xmap : <SID>(command-line-enter)

nnoremap ' <Nop>
nnoremap ' ;

nnoremap <silent> [Space]; :<C-u>normal!<Space>;<CR>
nnoremap <silent> [Space], :<C-u>normal!<Space>,<CR>

nnoremap <silent> [Space]yh :<C-u>Unite history/yank<CR>
nnoremap <silent> [Space]mk :<C-u>marks<CR>
nnoremap <silent> [Space]re :<C-u>registers<CR>
nnoremap <silent> [Space]ya :<C-u>1,$y<CR><Bar>:<C-u>1,$y+<CR>
nnoremap <silent> [Space]y% :<C-u>1,$y<CR><Bar>:<C-u>1,$y+<CR>

function! s:SID() "{{{
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfunction "}}}
function! s:SNR(map) "{{{
    return printf("<SNR>%d_%s", s:SID(), a:map)
endfunction "}}}

if (exists("g:indexed_search_plugin"))
  nnoremap <silent>n :let v:errmsg=''<CR>:silent! norm! nzv<CR>:<C-u>ShowSearchIndex<CR>
  nnoremap <silent>N :let v:errmsg=''<CR>:silent! norm! Nzv<CR>:<C-u>ShowSearchIndex<CR>
  nnoremap <silent>* :let v:errmsg=''<CR>:silent! norm! *zv<CR>:<C-u>ShowSearchIndex<CR>
  nnoremap <silent># :let v:errmsg=''<CR>:silent! norm! #zv<CR>:<C-u>ShowSearchIndex<CR>
endif

nmap <leader>b <SID>(command-line-enter)<C-u>CtrlPBufTagAll<CR>
nmap <leader>d <SID>(command-line-enter)<C-u>CtrlPDir<CR>
nmap <leader>q <SID>(command-line-enter)<C-u>CtrlPQuickfix<CR>
nmap <leader>l <SID>(command-line-enter)<C-u>CtrlPLine<CR>

" nmap : q:
" nmap / q/
" nmap ? q?

nmap [Space]r <SID>(command-line-enter)<C-u>QuickRun<CR>

nnoremap <expr> s* ':%substitute/\<' . expand('<cword>') . '\>/'

" Ruby block to yo face
nmap <silent> [Space]b <Plug>BlockToggle

if exists('g:loaded_delimitMate') && g:loaded_delimitMate == 1
  autocmd FileType * imap <C-b> <Plug>delimitMateC-Left
  autocmd FileType * imap <C-f> <Plug>delimitMateC-Right

  imap <C-b> <Plug>delimitMateC-Left
  imap <C-f> <Plug>delimitMateC-Right
else
  inoremap <silent> <C-B> <C-R>=getline('.')=~'^\s*$'&&col('.')>strlen(getline('.'))?"0\<Lt>C-D>\<Lt>Esc>kJs":"\<Lt>Left>"<CR>
  inoremap <silent> <C-F> <C-R>=col('.')>strlen(getline('.'))?"\<Lt>C-F>":"\<Lt>Right>"<CR>
endif

map [Space]c <Plug>(operator-camelize-toggle)

" Copy current buffer's path to clipboard{{{2
nnoremap <Leader>% :<C-u>call <SID>copy_ruby_path()<CR>
function! s:copy_ruby_path()
  let @*="ruby -Ilib:test ".expand('%')
  let @"="ruby -Ilib:test ".expand('%')
  let @+="ruby -Ilib:test ".expand('%')
endfunction"}}}2

" Copy current buffer's path to clipboard{{{2
nnoremap [Space]% :<C-u>call <SID>copy_path()<CR>
function! s:copy_path()
  let @*=expand('%')
  let @"=expand('%')
  let @+=expand('%')
endfunction"}}}2
" Altr {{{2
autocmd MyAutoCmd FileType vim call altr#define('autoload/%.vim', 'doc/%.txt', 'doc/%.jax', 'plugin/%.vim')
autocmd MyAutoCmd FileType clojure call altr#define('src/*/%.clj', 'test/*/%.clj', 'test/*/test/%.clj')
autocmd FileType ruby call altr#define('lib/%/*.rb', 'lib/%.rb', 'test/%_*.rb', 'test/%.rb')
autocmd FileType coffee call altr#define('src/%/*.coffee', 'src/%.coffee', 'test/%_*.coffee', 'test/%.coffee')
autocmd FileType * command! -buffer A call altr#forward()
"}}}2
" To keep legacy surround mapping{{{2
let g:surround_no_mappings=1
nmap ds  <Plug>Dsurround
nmap cs  <Plug>Csurround
nmap ys  <Plug>Ysurround
nmap yS  <Plug>YSurround
nmap yss <Plug>Yssurround
nmap ySs <Plug>YSsurround
nmap ySS <Plug>YSsurround
xmap S   <Plug>VSurround
xmap gS  <Plug>VgSurround
if !hasmapto("<Plug>Isurround","i") && "" == mapcheck("<C-S>","i")
  imap    <C-S> <Plug>Isurround
endif
imap      <C-G>s <Plug>Isurround
imap      <C-G>S <Plug>ISurround
vmap s <Plug>VSurround
xmap s <Plug>VSurround
" neocomplcache {{{2
" inoremap <expr> <C-x><C-f> neocomplcache#manual_filename_complete()
" nnoremap <silent> <expr> <CR> &bt == "" ? ":nohlsearch<CR>" : "\<CR>" 
" imap <expr> <silent> <Cr> <SID>CrInInsertModeBetterWay()

" function! s:CrInInsertModeBetterWay()
"   return pumvisible() ? "\<C-r>=neocomplcache#cancel_popup()"  : "\<Cr>"
" endfunction

" inoremap <expr><C-b>  pumvisible() ? "\<PageUp>"   :
"     \neocomplcache#sources#completefunc_complete#call_completefunc('googlesuggest_complete#completefunc')
" inoremap <expr><C-y>  pumvisible() ? neocomplcache#close_popup() :  "\<C-r>"
" inoremap <expr><C-e>  pumvisible() ? neocomplcache#cancel_popup() : "\<End>"
" imap <C-k>  <Plug>(neocomplcache_start_unite_complete)
" inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
" inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr> <C-l> pumvisible() ? "\<C-p>" : "\<C-p>"
" inoremap <expr><C-l>  pumvisible() ? neocomplcache#close_popup():  "\<C-p>"
" inoremap <expr><Space>  pumvisible() ? neocomplcache#cancel_popup()."\<Space>" :  "\<Space>"

" inoremap <expr><C-n>  pumvisible() ? "\<C-n>" : "\<C-x>\<C-u>\<C-p>\<Down>"
inoremap <expr><C-p>  pumvisible() ? "\<C-p>" : "\<C-p>\<C-n>"
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : "\<C-x>\<C-u>\<C-p>\<Down>"
" inoremap <expr><C-f>  pumvisible() ? "\<PageDown>" : "\<Right>"
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : "\<C-x>\<C-u>\<C-p>\<Down>"
" inoremap <expr><C-f>  pumvisible() ? "\<PageDown>" : "\<Right>"

"{{{2

" Useful save mappings{{{2
nnoremap <silent> [Space]w  :<C-u>update<CR>
nnoremap <silent> [Space]fw  :<C-u>write!<CR>
nnoremap <silent> [Space]q  :<C-u>quit<CR>
nnoremap <silent> [Space]aq  :<C-u>quitall<CR>
nnoremap <silent> [Space]fq  :<C-u>quitall!<CR>
nnoremap <Leader><Leader> :<C-u>update<CR>
"}}}2
" Change current directory{{{2
nnoremap <silent> [Space]cd :<C-u>call <SID>cd_buffer_dir()<CR>
function! s:cd_buffer_dir()"{{{
  let l:filetype = getbufvar(bufnr('%'), '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:dir = getbufvar(bufnr('%'), 'vimfiler').current_dir
  elseif l:filetype ==# 'vimshell'
    let l:dir = getbufvar(bufnr('%'), 'vimshell').save_dir
  else
    let l:dir = isdirectory(bufname('%')) ? bufname('%') : fnamemodify(bufname('%'), ':p:h')
  endif

  TabpageCD `=l:dir`
endfunction"}}}2
nnoremap <ESC><ESC> :redraw!<Bar>nohlsearch<CR>

nnoremap <silent> <expr> <CR> &bt == "" ? "/": "\<CR>" 
nmap R <Nop>
nmap R <SID>(command-line-enter)%s//
nnoremap <C-y> 5<C-y>
nnoremap <C-e> 5<C-e>

" Fast search pair.
nnoremap [Space]p %
xnoremap [Space]p %

" Fast screen move.
nnoremap [Space]j z<CR><C-f>z.
xnoremap [Space]j z<CR><C-f>z.
nnoremap [Space]k z-<C-b>z.
xnoremap [Space]k z-<C-b>z.

nnoremap [Space]ev :<C-u>edit $MYVIMRC<CR>
nnoremap [Space]sv :<C-u>source $MYVIMRC<CR>

" q commands "{{{2
nnoremap Q  gq
vnoremap Q  gq
"}}}
" Tags {{{2
nnoremap [Space]tt <C-]>
nnoremap [Space]tn :<C-u>tn<CR>
nnoremap [Space]tp :<C-u>tp<CR>
nnoremap [Space]tl :<C-u>tags<CR>
nnoremap [Space]ts :<C-u>ts<CR>
nnoremap [Space]tP :<C-u>tf<CR>
nnoremap [Space]tN :<C-u>tl<CR>
nnoremap [Space]tk :<C-u>tp<CR>
nnoremap [Space]tsn :<C-u>split<CR><Bar>:<C-u>tn<CR>
nnoremap [Space]tsp :<C-u>split<CR><Bar>:<C-u>tp<CR>
nnoremap [Space]tsP :<C-u>split<CR><Bar>:<C-u>tf<CR>
nnoremap [Space]tsN :<C-u>split<CR><Bar>:<C-u>tl<CR>
"}}}2
" q: Quickfix  "{{{

" The prefix key.
nnoremap [Quickfix]   <Nop>
" nmap    q  [Quickfix]
" Disable Ex-mode.

" For quickfix list  "{{{
nnoremap <silent> [Quickfix]n  :<C-u>cnext<CR>
nnoremap <silent> [Quickfix]p  :<C-u>cprevious<CR>
nnoremap <silent> [Quickfix]r  :<C-u>crewind<CR>
nnoremap <silent> [Quickfix]N  :<C-u>cfirst<CR>
nnoremap <silent> [Quickfix]P  :<C-u>clast<CR>
nnoremap <silent> [Quickfix]fn :<C-u>cnfile<CR>
nnoremap <silent> [Quickfix]fp :<C-u>cpfile<CR>
nnoremap <silent> [Quickfix]l  :<C-u>clist<CR>
nnoremap <silent> [Quickfix]q  :<C-u>cc<CR>
nnoremap <silent> [Quickfix]o  :<C-u>copen<CR>
nnoremap <silent> [Quickfix]c  :<C-u>cclose<CR>
nnoremap <silent> [Quickfix]en :<C-u>cnewer<CR>
nnoremap <silent> [Quickfix]ep :<C-u>colder<CR>
nnoremap <silent> [Quickfix]m  :<C-u>make<CR>
nnoremap [Quickfix]M  q:make<Space>
nnoremap [Quickfix]g  q:grep<Space>
" Toggle quickfix window.
nnoremap <silent> [Quickfix]<Space> :<C-u>call <SID>toggle_quickfix_window()<CR>
function! s:toggle_quickfix_window()
  let _ = winnr('$')
  cclose
  if _ == winnr('$')
    copen
    setlocal nowrap
    setlocal whichwrap=b,s
  endif
endfunction
"}}}

" For location list (mnemonic: Quickfix list for the current Window)  "{{{
nnoremap <silent> [Quickfix]wn  :<C-u>lnext<CR>
nnoremap <silent> [Quickfix]wp  :<C-u>lprevious<CR>
nnoremap <silent> [Quickfix]wr  :<C-u>lrewind<CR>
nnoremap <silent> [Quickfix]wP  :<C-u>lfirst<CR>
nnoremap <silent> [Quickfix]wN  :<C-u>llast<CR>
nnoremap <silent> [Quickfix]wfn :<C-u>lnfile<CR>
nnoremap <silent> [Quickfix]wfp :<C-u>lpfile<CR>
nnoremap <silent> [Quickfix]wl  :<C-u>llist<CR>
nnoremap <silent> [Quickfix]wq  :<C-u>ll<CR>
nnoremap <silent> [Quickfix]wo  :<C-u>lopen<CR>
nnoremap <silent> [Quickfix]wc  :<C-u>lclose<CR>
nnoremap <silent> [Quickfix]wep :<C-u>lolder<CR>
nnoremap <silent> [Quickfix]wen :<C-u>lnewer<CR>
nnoremap <silent> [Quickfix]wm  :<C-u>lmake<CR>
nnoremap [Quickfix]wM  q:lmake<Space>
nnoremap [Quickfix]w<Space>  q:lmake<Space>
nnoremap [Quickfix]wg  q:lgrep<Space>
"}}}
"}}}2
" s: Windows and buffers(High priority) "{{{2
" The prefix key.
nnoremap    [Window]   <Nop>
nmap    s [Window]
nnoremap <silent> [Window]p  :<C-u>call <SID>split_nicely()<CR>
nnoremap <silent> [Window]v  :<C-u>vsplit<CR>
nnoremap <silent> [Window]c  :<C-u>call <sid>smart_close()<CR>
" nnoremap <silent> -  :<C-u>call <sid>smart_close()<CR>
nnoremap <silent> [Window]o  :<C-u>only<CR>
nnoremap <silent> [Window]<Space>  :<C-u>call <SID>ToggleSplit()<CR>

nnoremap <silent> [Window]s  <Plug>(golden_ratio_resize)
function! s:MovePreviousWindow()
  let l:prev_name = winnr()
  silent! wincmd p
  if l:prev_name == winnr()
    silent! wincmd w
  endif
endfunction
" If window isn't splited, split buffer.
function! s:ToggleSplit()
  let l:prev_name = winnr()
  silent! wincmd w
  if l:prev_name == winnr()
    split
  else
    call s:smart_close()
  endif
endfunction
"}}}2
" Move search word and fold open"{{{2
" nnoremap N  Nzv
" nnoremap n  nzv
nnoremap g*  g*zv
nnoremap g#  g#zv
"}}}2
" smartword.vim"{{{2
" Replace w and others with smartword-mappings
nmap w  <Plug>(smartword-w)
nmap b  <Plug>(smartword-b)
nmap e  <Plug>(smartword-e)
nmap ge  <Plug>(smartword-ge)
xmap w  <Plug>(smartword-w)
xmap b  <Plug>(smartword-b)
" Operator pending mode.
omap <Leader>w  <Plug>(smartword-w)
omap <Leader>b  <Plug>(smartword-b)
omap <Leader>ge  <Plug>(smartword-ge)
"}}}2
" Command line buffer {{{2
nnoremap <sid>(command-buffer-enter) q:
xnoremap <sid>(command-buffer-enter) q:
nnoremap <sid>(command-buffer-norange) q:<C-u>
" nmap :  <sid>(command-buffer-enter)
" xmap :  <sid>(command-buffer-enter)
" nmap ;  <sid>(command-buffer-enter)
" xmap ;  <sid>(command-buffer-enter)
autocmd MyAutoCmd CmdwinEnter * call s:init_cmdwin()
function! s:init_cmdwin()
  nnoremap <buffer><silent> q :<C-u>quit<CR>
  inoremap <buffer><expr><TAB>  pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : "\<C-p>"
  startinsert!
endfunction
"}}}
function! s:check_back_space()"{{{
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction"}}}
"}}}2
" camlcasemotion.vim"{{{2
nmap <silent> W <Plug>CamelCaseMotion_w
xmap <silent> W <Plug>CamelCaseMotion_w
nmap <silent> B <Plug>CamelCaseMotion_b
xmap <silent> W <Plug>CamelCaseMotion_b
"}}}2

vnoremap <silent> [Space]a :Alignta =>/=<Cr>
xnoremap <silent> [Space]a  :Alignta =>\=<CR>

nnoremap <Leader>m :TagbarToggle<CR>
" nnoremap <Leader>d :TagbarOpenAutoClose<CR>
nmap <silent> <Leader>z <SID>(command-line-enter)<C-u>ZoomWin<CR>
nnoremap <Leader>v :<C-u>vsplit<CR><C-w><C-w>
nnoremap <Leader>s :<C-u>split<CR><C-w><C-w>

nnoremap <Leader>\ :%!fmt -w 79<CR>
xnoremap <Leader>\ :!fmt -w 79<CR>
vnoremap <Leader>\ :!fmt -w 79<CR>

nnoremap <Leader>` :<C-u>%s/\s*$//ge<CR>
nnoremap <Leader>cd :<C-u>cd %:p:h<CR>
nnoremap <silent> <Leader>y :YRShow<CR>
nnoremap <silent> <Leader>Y :GundoToggle<CR>
imap <M-o>       <Esc>o
imap <C-j>       <Down>
nmap <Leader>' ""yls<c-r>={'"': "'", "'": '"'}[@"]<CR><esc>
vmap <silent> g/ y/<C-R>=substitute(escape(@", '\\/.*$^~[]'), '\n', '\\n', 'g')<CR><CR>
nnoremap VV 0v$h
nnoremap <Leader>X <c-w><c-h>:set winwidth=80<CR><c-w><c-l>:set winwidth=31<CR><c-w><c-h>
nnoremap gff <C-w>gf
" imap (( ()
" inoremap <C-Space> <Right>
nmap <Leader>- i<space><esc>vs-2lxi
nmap <Leader>+ :<C-u>cd %:p:h<bar>new<Space>
imap <C-e> <esc>$a
imap <C-a> <esc>0i
" imap <C-b> <esc>ha
" imap <C-f> <esc>la
imap <C-d> <right><bs>
imap <silent> <C-BS> <esc>bvec
cmap <C-BS> <c-w>
nmap [2 :diffget //2<CR>
nmap ]3 :diffget //3<CR>
vmap <Leader>rv :call ExtractVariable()<CR>
nmap <Leader>ri :call InlineVariable()<CR>
nmap <Leader>/# /^ *#<CR>
nmap <Leader>/f /^ *def\><CR>
nmap <Leader>/c /^ *class\><CR>
nmap <Leader>/i /^ *if\><CR>

nmap \# /^ *#<CR>
nnoremap gs <Nop>
nmap gs :vimgrep /^ *\(context\<Bar>test\<Bar>def\<Bar>should\<Bar>class\)/ %<CR><Bar>:cw<CR>
nmap \/ /\<\><Left><Left>

nnoremap gqq vapgq

" nmap <Leader>a= :Tabularize /=<CR>
" vmap <Leader>a= :Tabularize /=<CR>
" nmap <Leader>a: :Tabularize /:\zs<CR>
" vmap <Leader>a: :Tabularize /:\zs<CR>
" nmap <Leader>a :Tabularize /
" vmap <Leader>a :Tabularize /

command! -nargs=? -range Align <line1>,<line2>call AlignSection('<args>')
vnoremap <silent> <Leader>a :Align<CR>
function! AlignSection(regex) range
  let extra = 1
  let sep = empty(a:regex) ? '=' : a:regex
  let maxpos = 0
  let section = getline(a:firstline, a:lastline)
  for line in section
    let pos = match(line, ' *'.sep)
    if maxpos < pos
      let maxpos = pos
    endif
  endfor
  call map(section, 'AlignLine(v:val, sep, maxpos, extra)')
  call setline(a:firstline, section)
endfunction

function! AlignLine(line, sep, maxpos, extra)
  let m = matchlist(a:line, '\(.\{-}\) \{-}\('.a:sep.'.*\)')
  if empty(m)
    return a:line
  endif
  let spaces = repeat(' ', a:maxpos - strlen(m[1]) + a:extra)
  return m[1] . spaces . m[2]
endfunction

nnoremap =p m`=ap``
nmap <Leader>= maG=gg`a

nmap <Leader>h :help <c-r>=expand("<cword>")<CR><CR>
vmap <Leader>h "ry:help<space><c-r>r<CR>
nmap <C-cr> <esc>yyp
imap <S-cr> <esc>$o
nnoremap <C-S-cr> d$O<esc>p0x
" Split movement{{{2
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
"}}}2
inoremap <C-]> <Space>=><Space>
cnoremap %% <C-R>=expand('%:h').'/'<CR>
cnoremap %& <C-R>=expand('%:p')<CR>
" Search slashes easily (too lazy to prefix backslashes to slashes)
cnoremap <expr> /  getcmdtype() == '/' ? '\/' : '/'
nnoremap <expr> gc  <SID>keys_to_select_the_last_changed_text()
nnoremap gV `[v`]
nmap \v /\v
nmap <S-right> g,
nmap <S-left> g;
" nnoremap / /\v
" vnoremap / /\v
" nmap <left>  <Plug>(jump-x2-to-previous)
" nmap <right>  <Plug>(jump-x2-to-next)
cnoremap <c-x> <c-r>=<SID>PasteEscaped()<cr>
" cnoremap <expr> <Tab> "\<Up>"

function! s:PasteEscaped()
  echo "\\".getcmdline()."\""
  let char = getchar()
  if char == "\<esc>"
    return ''
  else
    let register_content = getreg(nr2char(char))
    let escaped_register = escape(register_content, '\'.getcmdtype())
    return substitute(escaped_register, '\n', '\\n', 'g')
  endif
endfunction

function! s:jump_section_n(pattern)
  let pattern = a:pattern[1:]
  let forward_p = a:pattern[0] == '/'
  let flags = forward_p ? 'W' : 'Wb'

  mark '
  let i = 0
  while i < v:count1
    if search(pattern, flags) == 0
      if forward_p
        normal! G
      else
        normal! gg
      endif
      break
    endif
    let i = i + 1
  endwhile
endfunction

" for visual mode.  a:motion is '[[', '[]', ']]' or ']['.
function! s:jump_section_v(motion)
  execute 'normal!' "gv\<Esc>"
  execute 'normal' v:count1 . a:motion
  let line = line('.')
  let col = col('.')

  normal! gv
  call cursor(line, col)
endfunction

" for operator-pending mode.  a:motion is '[[', '[]', ']]' or ']['.
function! s:jump_section_o(motion)
  execute 'normal' v:count1 . a:motion
endfunction

xmap <C-\>  <Plug>Commentary
nmap <C-\> <Plug>CommentaryLine

if hasmapto('s', 'v')
  vunmap s
endif
vmap s S
xmap s S

" nnoremap <silent> <Tab> :call <SID>NextWindow()<CR>
" nnoremap <silent> <S-Tab> :call <SID>PreviousWindowOrTab()<CR>

function! s:keys_to_select_the_last_changed_text()
  " It is not possible to determine whether the last operation to change text
  " is linewise or not, so guess the wise of the last operation from the range
  " of '[ and '], like wise of a register content set by setreg() without
  " {option}.

  let col_begin = col("'[")
  let col_end = col("']")
  let length_end = len(getline("']"))

  let maybe_linewise_p = (col_begin == 1
        \                       && (col_end == length_end
        \                           || (length_end == 0 && col_end == 1)))
  return '`[' . (maybe_linewise_p ? 'V' : 'v') . '`]'
endfunction

function! s:NextWindow()
  if winnr('$') == 1
    call s:split_nicely()
  else
    wincmd w
  endif
endfunction

function! s:NextWindowOrTab()
  if tabpagenr('$') == 1 && winnr('$') == 1
    call s:split_nicely()
  elseif winnr() < winnr("$")
    wincmd w
  else
    tabnext
    1wincmd w
  endif
endfunction

function! s:PreviousWindowOrTab()
  if winnr() > 1
    wincmd W
  else
    tabprevious
    execute winnr("$") . "wincmd w"
  endif
endfunction

" Split nicely"{{{2
command! SplitNicely call s:split_nicely()
function! s:split_nicely()
  " Split nicely.
  if winwidth(0) > 2 * &winwidth
    vsplit
  else
    split
  endif
  wincmd p
endfunction
"}}}2
" Delete current buffer."{{{
nnoremap <silent> [Window]d  :<C-u>call <SID>CustomBufferDelete(0)<CR>
" Force delete current buffer.
nnoremap <silent> [Window]D  :<C-u>call <SID>CustomBufferDelete(1)<CR>
function! s:CustomBufferDelete(is_force)
  let current = bufnr('%')

  call s:CustomAlternateBuffer()

  if a:is_force
    silent! execute 'bdelete! ' . current
  else
    silent! execute 'bdelete ' . current
  endif
endfunction
"}}}
" Buffer move.
" Fast buffer switch."{{{
function! s:CustomAlternateBuffer()
  if bufnr('%') != bufnr('#') && buflisted(bufnr('#'))
    buffer #
  else
    let l:cnt = 0
    let l:pos = 1
    let l:current = 0
    while l:pos <= bufnr('$')
      if buflisted(l:pos)
        if l:pos == bufnr('%')
          let l:current = l:cnt
        endif

        let l:cnt += 1
      endif

      let l:pos += 1
    endwhile

    if l:current > l:cnt / 2
      bprevious
    else
      bnext
    endif
  endif
endfunction
"}}}
" Edit"{{{
nnoremap <silent> [Window]en  :<C-u>new<CR>
nnoremap <silent> [Window]ee  :<C-u>JunkFile<CR>
"}}}

" Scroll other window.
" nnoremap <silent> <C-y> :<C-u>call <SID>ScrollOtherWindow(1)<CR>
" inoremap <silent> <A-y> <C-o>:<C-u>call <SID>ScrollOtherWindow(1)<CR>
" nnoremap <silent> <C-u> :<C-u>call <SID>ScrollOtherWindow(0)<CR>
" inoremap <silent> <A-u> <C-o>:<C-u>call <SID>ScrollOtherWindow(0)<CR>

function! s:ScrollOtherWindow(direction)
  execute 'wincmd' (winnr('#') == 0 ? 'w' : 'p')
  execute (a:direction ? "normal! \<C-d>" : "normal! \<C-u>")
  wincmd p
endfunction
"}}}

" Smart }"{{{2
nnoremap <silent> } :<C-u>call ForwardParagraph()<CR>
onoremap <silent> } :<C-u>call ForwardParagraph()<CR>
xnoremap <silent> } <Esc>:<C-u>call ForwardParagraph()<CR>mzgv`z
function! ForwardParagraph()
  let cnt = v:count ? v:count : 1
  let i = 0
  while i < cnt
    if !search('^\s*\n.*\S','W')
      normal! G$
      return
    endif
    let i = i + 1
  endwhile
endfunction
"}}}
nnoremap <silent> [Space]<Space> :<C-u>buffer #<CR>
nnoremap <silent> 0 ^
nnoremap <silent> ^ 0
xnoremap <silent> 0 ^
xnoremap <silent> ^ 0
vnoremap <silent> 0 ^
vnoremap <silent> ^ 0

xmap p <Plug>(operator-replace)
xmap P <Plug>(operator-replace)

" Move to top/center/bottom.
noremap <expr> zz (winline() == (winheight(0)+1)/ 2) ? 'zt' : (winline() == 1)? 'zb' : 'zz'

" Auto escape / substitute.
xnoremap [Space]s y:%s/<C-r>=substitute(@0, '/', '\\/', 'g')<Return>//g<Left><Left>

if exists('g:loaded_poslist')
  nmap <Left> <Plug>(poslist-prev-buf)
  nmap <Right> <Plug>(poslist-next-buf)
  nmap [Space][ <Plug>(poslist-prev-buf)
  nmap [Space]]  <Plug>(poslist-next-buf)
else
  nmap [Space][ <Plug>(exjumplist-previous-buffer)
  nmap [Space]] <Plug>(exjumplist-next-buffer)
endif

iabbrev em; —

function! YRRunAfterMaps()
  nnoremap Y   :<C-U>YRYankCount 'y$'<CR>
endfunction

nnoremap Y y$
nnoremap . .`[

function! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfunction
vmap * :<C-u>call <SID>VSetSearch()<CR>//<CR>
vmap # :<C-u>call <SID>VSetSearch()<CR>??<CR>

nmap <Leader>gr :topleft :split config/routes.rb<CR>
function! ShowRoutes()
  topleft 100 :split __Routes__
  set buftype=nofile
  normal 1GdG
  0r! rake -s routes
  exec ":normal " . line("$") . "_ "
  normal 1GG
  normal dd
endfunction
nnoremap <Leader>gR :call ShowRoutes()<CR>

nnoremap <silent> <Leader>gv :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT app/views<CR>
nnoremap <silent> <Leader>gc :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT app/controllers<CR>
nnoremap <silent> <Leader>gu :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT test/unit<CR>
nnoremap <silent> <Leader>gF :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT test/functional<CR>
nnoremap <silent> <Leader>gt :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT test<CR>
nnoremap <silent> <Leader>gm :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT app/models<CR>
nnoremap <silent> <Leader>gh :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT app/helpers<CR>
nnoremap <silent> <Leader>gl :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT lib<CR>
nnoremap <silent> <Leader>gp :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT public<CR>
nnoremap <silent> <Leader>gs :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT public/stylesheets<CR>
nnoremap <silent> <Leader>gS :<C-u>CommandTFlush<CR><Bar>:<C-u>CommandT spec<CR>
nnoremap <silent> <Leader>gg :<C-u>topleft 100 :split Gemfile<CR>
nnoremap <silent> <Leader>gf :<C-u>CommandT %%/<CR>
" nnoremap <silent> <Leader>, :<C-u>CommandT<CR>
nnoremap <silent> <Leader>! :<C-u>CommandTFlush<CR>
nnoremap <silent> <Leader>, :<C-u>CommandTFlush<CR>:<C-u>CommandT<CR>
nnoremap <silent> <Leader> :<C-u>CommandTFlush<CR>:<C-u>CommandTBuffer<CR>
" Delete the content of the current line (not the line itself).
nnoremap dl  0d$

command! -nargs=+ Allmap
      \   execute 'map' <q-args>
      \ | execute 'map!' <q-args>

command! -nargs=+ Allnoremap
      \   execute 'noremap' <q-args>
      \ | execute 'noremap!' <q-args>

command! -nargs=+ Allunmap
      \   execute 'unmap' <q-args>
      \ | execute 'unmap!' <q-args>

command! -bang -nargs=* Cmap  call s:cmd_Cmap('', '<bang>', [<f-args>])
command! -nargs=* Ccmap  call s:cmd_Cmap('c', '', [<f-args>])
command! -nargs=* Cimap  call s:cmd_Cmap('i', '', [<f-args>])
command! -nargs=* Clmap  call s:cmd_Cmap('l', '', [<f-args>])
command! -nargs=* Cnmap  call s:cmd_Cmap('n', '', [<f-args>])
command! -nargs=* Comap  call s:cmd_Cmap('o', '', [<f-args>])
command! -nargs=* Csmap  call s:cmd_Cmap('s', '', [<f-args>])
command! -nargs=* Cvmap  call s:cmd_Cmap('v', '', [<f-args>])
command! -nargs=* Cxmap  call s:cmd_Cmap('x', '', [<f-args>])
command! -nargs=* Callmap  call s:cmd_Cmap('All', '', [<f-args>])
command! -nargs=* Cobjmap  call s:cmd_Cmap('Obj', '', [<f-args>])

function! s:separate_list(list, regexp)
  let i = 0
  while i < len(a:list) && a:list[i] =~# a:regexp
    let i += 1
  endwhile
  return [(0 < i ? a:list[:i-1] : []), a:list[(i):]]
endfunction

function! s:contains_p(list, regexp)
  for item in a:list
    if item =~# a:regexp
      return s:TRUE
    endif
  endfor
  return s:FALSE
endfunction

function! s:cmd_Cmap(prefix, suffix, args)
  " FIXME: This parsing may not be compatible with the original one.
  let [options, rest] = s:separate_list(a:args,
        \ '^\c<\(buffer\|expr\|script\|silent\|special\|unique\|count\|noexec\)>$')
  if len(rest) < 2
    throw 'Insufficient number of arglineuments: ' . string(rest)
  endif
  let lhs = rest[0]
  let script = rest[1:]
  let count_p = s:contains_p(options, '^\c<count>$')
  let noexec_p = s:contains_p(options, '^\c<noexec>$')
  call filter(options, 'v:val !~# ''^\c<\(count\|noexec\)>$''')

  execute a:prefix.'noremap'.a:suffix join(options) lhs
        \ ':'.(count_p ? '' : '<C-u>') . join(script) . (noexec_p ? '' : '<Return>')
endfunction

let s:FALSE = 0
let s:TRUE = !s:FALSE

command! -bang -nargs=* Fmap  call s:cmd_Fmap('', '<bang>', [<f-args>])
command! -nargs=* Fcmap  call s:cmd_Fmap('c', '', [<f-args>])
command! -nargs=* Fimap  call s:cmd_Fmap('i', '', [<f-args>])
command! -nargs=* Flmap  call s:cmd_Fmap('l', '', [<f-args>])
command! -nargs=* Fnmap  call s:cmd_Fmap('n', '', [<f-args>])
command! -nargs=* Fomap  call s:cmd_Fmap('o', '', [<f-args>])
command! -nargs=* Fsmap  call s:cmd_Fmap('s', '', [<f-args>])
command! -nargs=* Fvmap  call s:cmd_Fmap('v', '', [<f-args>])
command! -nargs=* Fxmap  call s:cmd_Fmap('x', '', [<f-args>])
command! -nargs=* Fallmap  call s:cmd_Fmap('All', '', [<f-args>])
command! -nargs=* Fobjmap  call s:cmd_Fmap('Obj', '', [<f-args>])

function! RunTests(filename)
    " Write the file and run tests for the given filename
    :w
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    if match(a:filename, '\.feature$') != -1
        exec ":!script/features " . a:filename
    else
        if filereadable("script/test")
            exec ":!script/test " . a:filename
        elseif filereadable("Gemfile")
            exec ":!bundle exec rspec --color " . a:filename
        else
            exec ":!rspec --color " . a:filename
        end
    end
endfunction

function! SetTestFile()
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@%
endfunction

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Run the tests for the previously-marked file.
    let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\)$') != -1
    if in_test_file
        call SetTestFile()
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file . command_suffix)
endfunction

function! RunNearestTest()
    let spec_line_number = line('.')
    call RunTestFile(":" . spec_line_number . " -b")
endfunction

nnoremap <leader>T :RunAllRubyTests<CR>
" nnoremap <leader>T :RunRubyFocusedTest<CR>
" nnoremap <leader>t RunRubyFocusedContext :call s:RunRubyFocusedContext()

" map <leader>t :call RunTestFile()<cr>
" map <leader>T :call RunNearestTest()<cr>
" map <leader>a :call RunTests('')<cr>
map <leader>c :w\|:!script/features<cr>
map <leader>w :w\|:!script/features --profile wip<cr>

Allnoremap <C-z>  <Nop>
Cnmap <C-z>  SuspendWithAutomticCD

if !exists('s:GNU_SCREEN_AVAILABLE_P')
  if has('gui_running')
    " In GUI, $WINDOW is not reliable, because GUI process is independent from
    " GNU screen process.  Check availability of executable instead.
    let s:GNU_SCREEN_AVAILABLE_P = executable('screen')
  else
    " In CUI, availability of executable is not reliable, because Vim may be
    " invoked with "screen ssh example.com vim" and GNU screen may be
    " available at example.com.  Check $WINDOW instead.
    let s:GNU_SCREEN_AVAILABLE_P = len($WINDOW) != 0
  endif
endif

command! -bar -nargs=0 SuspendWithAutomticCD
      \ call s:cmd_SuspendWithAutomticCD()
function! s:cmd_SuspendWithAutomticCD()
  if s:GNU_SCREEN_AVAILABLE_P
    call s:activate_terminal()

    " \015 = <C-m>
    " To avoid adding the cd script into the command-line history,
    " there are extra leading whitespaces in the cd script.
    silent execute '!screen -X eval'
          \              '''select another'''
          \              '''stuff "  cd \"'.getcwd().'\"  '
    " \#\#,vim-auto-cd\015"'''
    redraw!
    let s:GNU_SCREEN_AVAILABLE_P = (v:shell_error == 0)
  endif

  if !s:GNU_SCREEN_AVAILABLE_P
    suspend
  endif
endfunction

function! s:activate_terminal()  "{{{2
  if !has('gui_running')
    return
  endif

  if has('macunix')
    " There is alternative way to activate, but it's slow:
    " !osascript -e 'tell application "Terminal" to activate the front window'
    silent !open -a Terminal
  else
    " This platform is not supported.
  endif
endfunction

" Show the lines which match to the last search pattern.
" Cnmap <count> g/  global//print
" Cvmap <count> g/  global//print

" Ditto for quickfix

Cnmap <count> g/  :<C-U>vimgrep /<C-R>//j %<CR><Bar>:cw<CR>
Cvmap <count> g/  :<C-U>vimgrep /<C-R>//j %<CR><Bar>:cw<CR>

nmap go <Nop>
nmap go <SID>(command-line-enter)<C-u>Unite outline<CR>

function! s:cmd_Fmap(prefix, suffix, args)
  " FIXME: This parsing may not be compatible with the original one.
  let [options, rest] = s:separate_list(a:args,
        \ '^\c<\(buffer\|expr\|script\|silent\|special\|unique\)>$')
  if len(rest) < 2
    throw 'Insufficient number of arguments: ' . string(rest)
  endif
  let lhs = rest[0]
  let rhs = rest[1:]

  execute a:prefix.'noremap'.a:suffix join(options) lhs
        \ ':<C-u>call' join(rhs) '<Return>'
endfunction


" Objmap - :map for text objects  "{{{3
"
" Keys for text objects should be mapped in Visual mode and Operator-pending
" mode.  The following commands are just wrappers to avoid DRY violation.

command! -nargs=+ Objmap
      \   execute 'omap' <q-args>
      \ | execute 'vmap' <q-args>

command! -nargs=+ Objnoremap
      \   execute 'onoremap' <q-args>
      \ | execute 'vnoremap' <q-args>

command! -nargs=+ Objunmap
      \   execute 'ounmap' <q-args>
      \ | execute 'vunmap' <q-args>


" Operatormap - :map for oeprators  "{{{3
"
" Keys for operators should be mapped in Normal mode and Visual mode.  The
" following commands are just wrappers to avoid DRY violation.
"
" FIXME: How about mapping to g@ in Operator-pending mode
"        to use {operator}{operator} pattern?

command! -nargs=+ Operatormap
      \   execute 'nmap' <q-args>
      \ | execute 'vmap' <q-args>

command! -nargs=+ Operatornoremap
      \   execute 'nnoremap' <q-args>
      \ | execute 'vnoremap' <q-args>

command! -nargs=+ Operatorunmap
      \   execute 'nunmap' <q-args>
      \ | execute 'vunmap' <q-args>

Fvmap *  <SID>search_the_selected_text_literaly('n')
Fvmap #  <SID>search_the_selected_text_literaly('N')

Fvmap <silent> ]]  <SID>jump_section_v(']]')
Fvmap <silent> ][  <SID>jump_section_v('][')
Fvmap <silent> [[  <SID>jump_section_v('[[')
Fvmap <silent> []  <SID>jump_section_v('[]')
Fomap <silent> ]]  <SID>jump_section_o(']]')
Fomap <silent> ][  <SID>jump_section_o('][')
Fomap <silent> [[  <SID>jump_section_o('[[')
Fomap <silent> []  <SID>jump_section_o('[]')


function! s:search_the_selected_text_literaly(search_command)
  let reg_0 = [@0, getregtype('0')]
  let reg_u = [@", getregtype('"')]

  normal! gvy
  let @/ = @0
  call histadd('/', '\V' . escape(@0, '\'))
  execute 'normal!' a:search_command
  let v:searchforward = a:search_command ==# 'n'

  call setreg('0', reg_0[0], reg_0[1])
  call setreg('"', reg_u[0], reg_u[1])
endfunction


function! Preserve(command)
  let _s=@/
  let l = line(".")
  let c = col(".")
  execute a:command
  let @/=_s
  call cursor(l, c)
endfunction

nmap _$ :call Preserve("%s/\\s\\+$//e")<CR>
nmap _= :call Preserve("normal gg=G")<CR>

function! Titlecase()
  if !executable("toTitleCase")
    echo "Missing toTitleCase excutable."
    return
  endif
  normal gv"xy
  let @x = system('echo "'.@x.'"| toTitleCase | tr -d "\n"')
  let @x = substitute(@x, '^\s*\(.\{-}\)\s*$', '\1', '')
  normal gv
  normal "xp
endfunction
vmap <Leader>t :call Titlecase()<CR>

inoremap <C-O> <C-X><C-O>
nmap <Leader>x <SID>(command-line-enter)<C-u>sp <C-R>=expand("%:h")<CR>/

inoremap <Tab> <C-r>=InsertTabWrapper()<CR>
inoremap <S-Tab> <c-n>
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
"}}}1
" Platform depends:"{{{1
" For Linux"{{{
if exists('$WINDIR')
  " Cygwin.

  " Use bash.
  set shell=bash
else
  " Use zsh.
  set shell=sh
endif

" Set path.
let $PATH = expand('~/bin').':/usr/local/bin/:'.$PATH

" For non GVim.
if !has('gui_running')
  " Enable 256 color terminal.
  if !exists('$TMUX')
    set t_Co=256

    " For screen."{{{
    if &term =~ '^screen'
      augroup MyAutoCmd
        " Show filename on screen statusline.
        " But invalid 'another' screen buffer.
        autocmd BufEnter * if $WINDOW != 0 &&  bufname("") !~ "[A-Za-z0-9\]*://"
              \ | silent! exe '!echo -n "kv:%:t\\"' | endif
        " When 'mouse' isn't empty, Vim will freeze. Why?
        autocmd VimLeave * :set mouse=
      augroup END

      " For Vim inside screen.
      set ttymouse=xterm2
    endif

    " For prevent bug.
    autocmd MyAutoCmd VimLeave * set term=screen
    "}}}
  endif

  if has('gui')
    " Use CSApprox.vim

    " Convert colorscheme in Konsole.
    let g:CSApprox_konsole = 1
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
  else
    " Disable error messages.
    let g:CSApprox_verbose_level = 0
  endif

  " Change cursor shape.
  " if &term =~ "xterm"
  "   let &t_SI = "\<Esc>]12;lightgreen\x7"
  "   let &t_EI = "\<Esc>]12;white\x7"
  " endif
endif
"}}}1


set secure

" vim: foldmethod=marker