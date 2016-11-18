#!/bin/zsh


pwd=${HOME}/dotfiles
# TODO(mkdong): check real pwd


# links
ln -s ${HOME}/dotfiles/zshrc ~/.zshrc
ln -s ~/dotfiles/prompt_steeeff_setup ~/.zgen/sorin-ionescu/prezto-master/modules/prompt/functions/prompt_steeeff_setup 

# get zgen
git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"



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
