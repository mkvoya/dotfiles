#!/bin/sh

set -n
## get tpm
#git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# The fish shell
ln -s ~/.dotfiles/fish ~/.config/

# tmux
ln -s ~/.dotfiles/tmux/tmux.conf ~/.tmux.conf

# other configs for nvim
ln -s ~/.dotfiles/nvim ~/.config/
