
# fisher package manager
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# fzf
function fzf-cdhist-widget -d 'cd to one of the previously visited locations'
	# Clear non-existent folders from cdhist.
	set -l buf
	for i in (seq 1 (count $dirprev))
		set -l dir $dirprev[$i]
		if test -d $dir
			set buf $buf $dir
		end
	end
	set dirprev $buf
	string join \n $dirprev | tac | sed 1d | eval (__fzfcmd) +m --tiebreak=index --toggle-sort=ctrl-r $FZF_CDHIST_OPTS | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end


# vi key binding
function hybrid_bindings --description "Vi-style bindings that inherit emacs-style bindings in all modes"
    for mode in default insert visual
        fish_default_key_bindings -M $mode
    end
    fish_vi_key_bindings --no-erase
end
set -g fish_key_bindings hybrid_bindings



function fish_greeting
  set_color $fish_color_autosuggestion
  echo "Hello From Fish!"
  set_color normal
end


# theme modified from lambda
function fish_prompt
  # Cache exit status
  set -l last_status $status
  set -l tomita_vi_mode "$TOMITA_VI"
  set -l vi_color (set_color --bold green)

  if test -z (string match -ri '^no|false|0$' $tomita_vi_mode)
    switch $fish_bind_mode
      case default
        set vi_color (set_color --bold red)
      case insert
        set vi_color (set_color --bold green)
      case visual
        set vi_color (set_color --bold magenta)
    end
  end

  # Just calculate these once, to save a few cycles when displaying the prompt
  if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
  end
  if not set -q __fish_prompt_char
    switch (id -u)
      case 0
        set -g __fish_prompt_char '#-#'
      case '*'
#set -g __fish_prompt_char 'λ'
#set -g __fish_prompt_char '⊨'
        set -g __fish_prompt_char '➡➤'
    end
  end

  # Setup colors
  #use extended color pallete if available
#if [[ $terminfo[colors] -ge 256 ]]; then
#    turquoise="%F{81}"
#    orange="%F{166}"
#    purple="%F{135}"
#    hotpink="%F{161}"
#    limegreen="%F{118}"
#else
#    turquoise="%F{cyan}"
#    orange="%F{yellow}"
#    purple="%F{magenta}"
#    hotpink="%F{red}"
#    limegreen="%F{green}"
#fi
  set -l normal (set_color normal)
  set -l white (set_color FFFFFF)
  set -l turquoise (set_color 5fdfff)
  set -l orange (set_color df5f00)
  set -l hotpink (set_color df005f)
  set -l blue (set_color blue)
  set -l limegreen (set_color 87ff00)
  set -l purple (set_color af5fff)

  # Configure __fish_git_prompt
  set -g __fish_git_prompt_char_stateseparator ' '
  set -g __fish_git_prompt_color 5fdfff
  set -g __fish_git_prompt_color_flags df5f00
  set -g __fish_git_prompt_color_prefix white
  set -g __fish_git_prompt_color_suffix white
  set -g __fish_git_prompt_showdirtystate true
  set -g __fish_git_prompt_showuntrackedfiles true
  set -g __fish_git_prompt_showstashstate true
  set -g __fish_git_prompt_show_informative_status true

  # Line 1
  echo -n $vi_color'⋰ '$hotpink$USER$white' at '$orange$__fish_prompt_hostname$white' in '$limegreen(pwd)$turquoise
  __fish_git_prompt " (%s)"
  echo

  # Line 2
  echo -n $vi_color'⋱'
  # support for virtual env name
  if set -q VIRTUAL_ENV
      echo -n "($turquoise"(basename "$VIRTUAL_ENV")"$white)"
  end
#echo -n $white'─' $vi_color $__fish_prompt_char $normal
  set -l c1 (set_color F54235)
  set -l c2 (set_color FDEB61)
  set -l c3 (set_color 9AE343)
  set -l ar '➨'
  set -l ar2 '➥'
#echo -n ' '$c1$ar$c2$ar$c3$ar

  echo -n $vi_color $__fish_prompt_char $normal

end


function fish_right_prompt
	set -l exit_code $status
  __tmux_prompt
  if test $exit_code -ne 0
    set_color red
  else
    set_color 666666
  end
  printf '%d' $exit_code
  set_color 666666
  set_color E3782C
  printf ' ≡'
  set_color CA1B00
  printf ' %s' (date +"%Y-%m-%d")
  set_color 5FA701
  printf ' %s' (date +"%H:%M:%S")
  set_color normal
end

function __tmux_prompt
  set multiplexer (_is_multiplexed)

  switch $multiplexer
    case screen
      set pane (_get_screen_window)
    case tmux
      set pane (_get_tmux_window)
   end

  set_color 666666
  if test -z $pane
    echo -n ""
  else
    echo -n $pane' | '
  end
end

function _get_tmux_window
  tmux lsw | grep active | sed 's/\*.*$//g;s/: / /1' | awk '{ print $2 "-" $1 }' -
end

function _get_screen_window
  set initial (screen -Q windows; screen -Q echo "")
  set middle (echo $initial | sed 's/  /\n/g' | grep '\*' | sed 's/\*\$ / /g')
  echo $middle | awk '{ print $2 "-" $1 }' -
end

function _is_multiplexed
  set multiplexer ""
  if test -z $TMUX
  else
    set multiplexer "tmux"
  end
  if test -z $WINDOW
  else
    set multiplexer "screen"
  end
  echo $multiplexer
end



# ----------------------
# Git Aliases
# ----------------------
#alias ga='git add'
#alias gaa='git add .'
#alias gaaa='git add -A'
#alias gb='git branch'
#alias gbd='git branch -d '
#alias gc='git commit'
#alias gcm='git commit -m'
#alias gco='git checkout'
#abbr -a gco git checkout
#alias gcob='git checkout -b'
#alias gcom='git checkout master'
#alias gd='git diff'
abbr -a gd git diff
#alias gda='git diff HEAD'
#alias gi='git init'
#alias gl='git log'
#alias glg='git log --graph --oneline --decorate --all'
abbr -a glg git log --graph --oneline --decorate --all
#alias gld='git log --pretty=format:"%h %ad %s" --date=short --all'
#alias gm='git merge --no-ff'
#alias gp='git pull'
#alias gs='git status'
abbr -a gs git status
#alias gss='git status -s'
#alias gst='git stash'
#alias gstl='git stash list'
#alias gstp='git stash pop'
#alias gstd='git stash drop'

#alias vim="nvim"
#alias vi="nvim"
#export EDITOR=nvim
set -x EDITOR nvim
#abbr -a v nvim "conflict with vlang
abbr -a e $EDITOR
abbr -a vim nvim
abbr -a vi nvim

if not type -q rg
    echo "Would you please install rg? (https://github.com/BurntSushi/ripgrep)"
else
    abbr -a ag rg
    abbr -a ack rg
end

if not type -q fzf
    echo "Would you please install fzf? (https://github.com/junegunn/fzf)"
    echo ";and fisher add jethrokuan/fzf"
end

#test -e "$HOME/.iterm2_shell_integration.zsh" && source "$HOME/.iterm2_shell_integration.zsh"

#export LC_ALL=en_US.UTF-8
set -x LC_ALL en_US.UTF-8
#export LANG=en_US.UTF-8
set -x LANG en_US.UTF-8

#export PATH=$PATH:/usr/local/sbin
if test -d ~/.cargo/bin
#    Just dont' use -U, otherwise the fish_user_paths will be appended on each boot, and become extremely long.
#    set -U fish_user_paths $HOME/.cargo/bin $fish_user_paths
    set -x PATH $HOME/.cargo/bin $PATH
end
if test -d /usr/loca/bin
#    set -U fish_user_paths /usr/local/bin $fish_user_paths
    set -x PATH /usr/local/bin $PATH
end
