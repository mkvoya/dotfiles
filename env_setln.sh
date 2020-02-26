#!/bin/sh


## get zgen
#git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
## get tpm
#git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

## for nvim: curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
## for vim: curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# The fish shell
mkdir -p ~/.config/fish
ln -s ~/.dotfiles/fish ~/.config/fish

# tmux
ln -s ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf

# nvim (not compatible for vim)
mkdir -p ~/.config/nvim
ln -s ~/.dotfiles/vimrc ~/.config/nvim/init.vim
ln -s ~/.dotfiles/vimrc ~/.vimrc

# other configs for nvim
ln -s ~/.config/nvim ~/.vim
mkdir -p ~/.vim/colors
ln -s ~/.dotfiles/CandyPaperLight.vim ~/.vim/colors/CandyPaperLight.vim

## for vim: curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
