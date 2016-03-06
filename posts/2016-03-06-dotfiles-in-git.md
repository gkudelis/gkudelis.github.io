---
title: Dotfiles in git
tags: git
published: true
---

TL;DR: I put my dotfiles on GitHub and wrote a `Makefile` that installs it. You
can find it all in [gkudelis/dotfiles](https://github.com/gkudelis/dotfiles).

I recently started working for a new company. They're using [Vagrant](https://www.vagrantup.com/) to set up
development environments and one of my first tasks was to sort out some package
management issues. Long story short, I found myself starting a bunch of new
machines and next week I'll be starting many more.

Until now my process for getting my dotfiles onto a new machine involved a USB
stick, but it's definitely not the way to go if you need to do it more than
once a month. I decided to put my dotfiles on [GitHub](https://github.com/) and sort out some sort
of installation script to make it all easier. Ideally I'd just clone the
repository, `cd` into it, run something like `make install` and have all
my stuff there.

Currently I only care about my dotfiles for `vim`, `tmux`, `zsh` and `git`. I
could just symlink the `tmux`, `zsh` and `git` files, but `vim` would need something
that could download all the packages. After a quick look I decided it's time
to switch to using [Vundle](https://github.com/VundleVim/Vundle.vim) as you just have to list all the packages you need
in your `.vimrc` and Vundle will install/update them for you. The process becomes

- symlink `.vimrc`,
- `git clone` Vundle inside `.vim/bundle`,
- run `vim +PluginInstall +qall` to make Vundle download and install all the plugins.

After doing that I noticed that the last step was making `vim` error because of
a missing colorscheme (which was about to be installed). Changing `colorscheme solarized` to
`silent! colorscheme solarized` fixed that and I ended up with a non-interactive
installation process.

After sorting out installation I realised that because I'm dealing with a number
of different machines (OSX on my laptop, Linux on the work Vagrant machines) the
dotfiles need to be slightly different (mostly differences in paths). My
solution was to keep a branch for each of the different environments and have a
master branch where changes to be applied to all branches would go. The
specific branches can then be rebased onto the last commit of the master
branch to bring in those general changes.

P.S. The morning after setting this up I found [GitHub does dotfiles](https://dotfiles.github.io/). If you're
interested in setting this up for yourself you should go read that first.
