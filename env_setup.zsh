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
ln -s ${HOME}/dotfiles/zshrc ~/.zshrc
source ~/.zshrc
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/vimrc ~/.vimrc
ln -s ~/dotfiles/prompt_steeeff_setup ~/.zgen/sorin-ionescu/prezto-master/modules/prompt/functions/prompt_steeeff_setup 
mkdir -p ~/.vim/colors
ln -s ~/dotfiles/CandyPaperLight.vim ~/.vim/colors/CandyPaperLight.vim

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
