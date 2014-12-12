## About

[Vundle](https://github.com/chilicuil/vundle) is short for _Vim bundle_ and is a [Vim](http://vim.org) plugin manager. This is a custom version, merging [Vundle](https://github.com/chilicuil/vundle) & [Plug](https://github.com/junegunn/vim-plug).

![Vundle-installer](https://raw.githubusercontent.com/junegunn/i/master/vim-plug/installer.gif)

## Quick start

1. Setup:

     ```
     $ git clone https://github.com/chilicuil/vundle.git ~/.vim/bundle/vundle
     ```

2. Configuration:

     Sample `.vimrc`:

     ```vim
     set nocompatible               " be iMproved
     filetype off                   " required!

     set rtp+=~/.vim/bundle/vundle/
     call vundle#rc()

     " let Vundle manage Vundle
     " required!
     Bundle 'chilicuil/vundle'

     " My Bundles here:
     "
     " original repos on github
     Bundle 'tpope/vim-fugitive'
     Bundle 'Lokaltog/vim-easymotion'
     Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
     Bundle 'tpope/vim-rails.git'
     " vim-scripts repos
     Bundle 'L9'
     Bundle 'FuzzyFinder'
     " non github repos
     Bundle 'git://git.wincent.com/command-t.git'
     " git repos on your local machine (ie. when working on your own plugin)
     Bundle 'file:///Users/chilicuil/path/to/plugin'
     " ...

     filetype plugin indent on     " required!
     "
     " Brief help
     " :BundleList          - list configured bundles
     " :BundleInstall(!)    - install(update) bundles
     " :BundleSearch(!) foo - search(or refresh cache first) for foo
     " :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
     "
     " see :h vundle for more details
     " NOTE: comments after Bundle command are not allowed..
     s``

3. Deploy:

     Launch `vim`, run `:BundleInstall`
     (or `vim +BundleInstall +qall` for CLI lovers)

     Installing requires Git and triggers [Git clone](http://gitref.org/creating/#clone) for each configured repo to `~/.vim/bundle/`.

## Why Vundle

Vundle allows to:

- keep track and configure your scripts right in `.vimrc`
- install configured scripts (aka bundle)
- update configured scripts
- clean unused scripts up
- regenerates helptag automatically

## Docs

see [`:h vundle`](vundle/blob/master/doc/vundle.txt#L1) vimdoc for more details.

## Inspiration and ideas from

* [pathogen](https://github.com/tpope/vim-pathogen)
* [bundler](http://bundler.io/)
* [Scott Bronson](http://github.com/bronson)
