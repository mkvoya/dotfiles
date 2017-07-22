source ~/dotfiles/gentoo-bashrc

# Safe rm
alias rm='rm -i'
# ----------------------
# Git Aliases
# ----------------------
alias ga='git add'
alias gaa='git add .'
alias gaaa='git add -A'
alias gb='git branch'
alias gbd='git branch -d '
alias gc='git commit'
alias gcm='git commit -m'
alias gco='git checkout'
alias gcob='git checkout -b'
alias gcom='git checkout master'
alias gd='git diff'
alias gda='git diff HEAD'
alias gi='git init'
alias gl='git log'
alias glg='git log --graph --oneline --decorate --all'
alias gld='git log --pretty=format:"%h %ad %s" --date=short --all'
alias gm='git merge --no-ff'
alias gp='git pull'
alias gs='git status'
alias gss='git status -s'
alias gst='git stash'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gstd='git stash drop'

# ----------------------
# Git Functions
# ----------------------
# Git log find by commit message
function glf() { git log --all --grep="$1"; }

# # Search backwards and forwards with a pattern
# bindkey -M vicmd '/' history-incremental-pattern-search-backward
# bindkey -M vicmd '?' history-incremental-pattern-search-forward
# #
# # # set up for insert mode too
# bindkey -M viins '^R' history-incremental-pattern-search-backward
# bindkey -M viins '^F' history-incremental-pattern-search-forward


export PATH="/usr/local/sbin:$PATH"

# stardict
export STARDICT_DATA_DIR=~/.stardict-data
alias def="/usr/bin/sdcv"

# flex
export PATH="/usr/local/opt/flex/bin:$PATH"
export CPATH="/usr/local/opt/flex/include:$CPATH"
#export LIBRARY_PATH="/usr/local/opt/lib:$LIBRARY_PATH"
#export LD_LIBRARY_PATH="/usr/local/lib/opt/lib:$LD_LIBRARY_PATH"
#export SPARKLE_FRAMEWORK="/Applications/Sparkle\ Test\ App.app/Contents/Frameworks/Sparkle.framework/"

eval $(/usr/libexec/path_helper -s)

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
#export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
export EDITOR=vim

export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"
# This export doesn't work due to SIP
export DYLD_LIBRARY_PATH="/usr/local/opt/icu4c/lib:$DYLD_LIBRARY_PATH"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="/Users/mkdong/devtools/fakeroot/bin:$PATH:$HOME/.rvm/bin"

set -o vi

NORM="$(tput setaf 9)"
RESET="$(tput sgr0)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
BOLD="$(tput bold)"
ORANGE="$(tput setaf 166)"
PURPLE="$(tput setaf 135)"
LIMEGREEN="$(tput setaf 118)"
COLUMNS=$(tput cols)
COL214=$(tput setaf 214)

bind "set vi-ins-mode-string \ \1${BOLD}${RED}\2❯\1${YELLOW}\2❯\1${GREEN}\2❯"
bind "set vi-cmd-mode-string \ \1${BOLD}${GREEN}\2❮\1${YELLOW}\2❮\1${RED}\2❮"
bind 'set show-mode-in-prompt on'
RIGHT="$(tput cuf 150) DATE"

export PS1="\[${BOLD}${GREEN}\]\u @ \[${COL214}\]\h \[${GREEN}\][\t] >\$?< \[${LIMEGREEN}\]\w .....\[${RESET}\]
 \[${RESET}\]"
