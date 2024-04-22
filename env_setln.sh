#!/bin/sh
## get tpm
#git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# The fish shell
mkdir -p ~/.config/fish
ln -s ~/.dotfiles/fish ~/.config/fish

# tmux
ln -s ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf

# other configs for nvim
ln -s ~/.dotfiles/nvim ~/.config/nvim
