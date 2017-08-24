#!/bin/zsh


pwd=${HOME}/dotfiles
# TODO(mkdong): check real pwd

# get zgen
git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
# get tpm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
# get Vundle
git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim

# links
source env_setln.zsh
source ~/.zshrc

function check {
  # Symlink online-check.sh
  ln -fs $HOME/dotfiles/lib/online-check.zsh $HOME/.online-check.zsh
  
  # Write out current crontab
  crontab -l > mycron
  # Echo new cron into cron file
  echo "* * * * * ~/.online-check.zsh" >> mycron
  # Install new cron file
  crontab mycron
  rm mycron
}
