#!/bin/zsh

ln -s ${HOME}/.dotfiles/shells/zshrc ~/.zshrc
ln -s ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf
ln -s ~/.dotfiles/vimrc ~/.vimrc
mkdir -p ~/.vim/colors
ln -s ~/.dotfiles/CandyPaperLight.vim ~/.vim/colors/CandyPaperLight.vim
mkdir -p ~/.config/nvim
ln -s ~/.vimrc ~/.config/nvim/init.vim
mkdir -p ~/.config
ln -s ~/.dotfiles/fish ~/.config/fish
