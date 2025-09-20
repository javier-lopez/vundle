## About

[Vundle](https://github.com/javier-lopez/vundle) is short for _Vim bundle_ and is a [Vim](http://vim.org) plugin manager.

This is a custom version, merging [Vundle](https://github.com/javier-lopez/vundle) & [Plug](https://github.com/junegunn/vim-plug).

<p align="center">
<img src="https://raw.githubusercontent.com/junegunn/i/master/vim-plug/installer.gif" alt="vundle-installer"/>
</p>

## Quick start

1. Setup:

     ```
     $ git clone https://github.com/javier-lopez/vundle.git ~/.vim/bundle/vundle
     ```

2. Configuration:

     Sample `.vimrc`:

     ```vim
     set rtp+=~/.vim/bundle/vundle/
     call vundle#rc()

     " let Vundle manage Vundle, required!
     Bundle 'javier-lopez/vundle'

     " github repos
     Bundle 'Lokaltog/vim-easymotion'
     Bundle 'msanders/snipmate.vim'     , { 'on': 'insert' }
     Bundle 'paradigm/TextObjectify'    , { 'on': 'delay' }
     Bundle 'scrooloose/nerdtree'       , { 'on': 'NERDTreeToggle' }
     Bundle 'javier-lopez/TaskList.vim' , { 'on': '<Plug>TaskList' }
     Bundle 'kien/ctrlp.vim'            , { 'on': ['CtrlP', 'CtrlPBuffer'] }
     Bundle 'javier-lopez/vim-markdown' , { 'for': 'mkd' }
     Bundle 'rstacruz/sparkup'          , { 'rtp': 'vim/'}

     Bundle 'junegunn/fzf', 'develop' " use the develop branch instead of master

     " vim-scripts repos
     Bundle 'surround.vim' , { 'on': ['insert', 'delay', ] }

     " non github repos
     Bundle 'git://git.wincent.com/command-t.git'
     " git repos on your local machine (ie. when working on your own plugin)
     Bundle 'file:///Users/admin/path/to/plugin'
     " ...

     " Brief help
     " :BundleInstall       - install bundles
     " :BundleUpdate        - update bundles
     " :BundleStatus        - list configured bundles
     " :BundleClean         - remove unused bundles
     "
     " see :h vundle for more details
     " NOTE: comments after Bundle command are not allowed..

     ```

3. Deploy:

     Launch `vim`, run `:BundleInstall`
     (or `vim +BundleInstall +qall` for CLI lovers)

     Installing requires Git and triggers [Git clone](http://gitref.org/creating/#clone) for each configured repo to `~/.vim/bundle/`.

## Differences

- Use "Bundle" to define bundles/plugins
- Remove the need to end() vundle definitions, go back to rc() init method
- Define 'insert' (on Insert mode) and 'delay' (on time, 10 sec by default) triggers
- Fix case when it's initialized hands-free

## Why

There are glorified vundle plugins who do it better but break compatibility, I wanted a drop-in replacement who had better features (specially lazy loading) but don't force me to edit my vimrc file (except for replacing vundle with the new thing). This revision currently comply with all my requirements.
