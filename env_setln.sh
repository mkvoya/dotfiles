#!/bin/sh


## get zgen
#git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
## get tpm
#git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
## get Vundle
#git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim

# The fish shell
mkdir -p ~/.config/fish
ln -s ~/.dotfiles/fish ~/.config/fish

# tmux
ln -s ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf

# nvim (not compatible for vim)
mkdir -p ~/.config/nvim
ln -s ~/.dotfiles/vimrc ~/.config/nvim/init.vim

# other configs for nvim
mkdir -p ~/.vim/colors
ln -s ~/.dotfiles/CandyPaperLight.vim ~/.vim/colors/CandyPaperLight.vim

