mkdir -p ~/.vimbundles
cd ~/.vimbundles

get_bundle() {
  (
  if [ -d "$2" ]; then
    echo "Updating $1's $2"
    cd "$2"
    git pull --rebase
  else
    git clone "git://github.com/$1/$2.git"
  fi
  )
}

#get_bundle adamlowe vim-slurper
#get_bundle duff vim-bufonly
#get_bundle godlygeek tabular
#get_bundle kchmck vim-coffee-script
#get_bundle leshill vim-json
#get_bundle mileszs ack.vim
#get_bundle pangloss vim-javascript
#get_bundle scrooloose nerdcommenter
#get_bundle therubymug vim-pyte
#get_bundle tpope vim-abolish
#get_bundle tpope vim-cucumber
#get_bundle tpope vim-endwise
#get_bundle tpope vim-fugitive
#get_bundle tpope vim-git
#get_bundle tpope vim-haml
#get_bundle tpope vim-markdown
#get_bundle tpope vim-pathogen
#get_bundle tpope vim-rake
#get_bundle tpope vim-ragtag
#get_bundle tpope vim-rails
#get_bundle tpope vim-repeat
#get_bundle tpope vim-speeddating
#get_bundle tpope vim-surround
#get_bundle tpope vim-unimpaired
#get_bundle tpope vim-vividchalk
#get_bundle vim-ruby vim-ruby
#get_bundle wgibbs vim-irblack
#get_bundle vim-scripts bufkill.vim
#get_bundle jgdavey vim-blockle
#get_bundle jgdavey vim-railscasts
#get_bundle gregsexton gitv

get_bundle scrooloose nerdtree
get_bundle thinca vim-quickrun
get_bundle msanders cocoa.vim
get_bundle mileszs ack.vim
get_bundle vim-scripts bufexplorer.zip
get_bundle vim-scripts camelcasemotion
get_bundle vim-scripts cheat.vim
get_bundle scrooloose nerdcommenter
get_bundle tpope vim-fugitive
get_bundle tpope vim-endwise
get_bundle mattn gist-vim
get_bundle vim-scripts grep.vim
get_bundle vim-scripts IndexedSearch
get_bundle vim-scripts matchit.zip
get_bundle fholgado minibufexpl.vim
get_bundle tpope vim-pastie
get_bundle tpope vim-ragtag
get_bundle tpope vim-rails
get_bundle msanders snipmate.vim
get_bundle tpope vim-surround
get_bundle scrooloose syntastic
get_bundle godlygeek tabular
get_bundle robgleeson vim-markdown-preview
get_bundle vim-scripts ZoomWin
get_bundle scrooloose snipmate-snippets
get_bundle tpope vim-pathogen
get_bundle travisjeffery taglist.vim
get_bundle sjl gundo.vim
get_bundle wlangstroth vim-racket
get_bundle travisjeffery vim-help
get_bundle mattn zencoding-vim/
get_bundle altercation vim-colors-solarized
get_bundle vim-scripts netrw.vim
get_bundle majutsushi tagbar
get_bundle travisjeffery vim-colors
get_bundle Townk vim-autoclose

vim -c 'call pathogen#helptags()|q'
