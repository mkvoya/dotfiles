# load zgen
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then

    echo "Creating a zgen save"

    # prezto options
    zgen prezto editor key-bindings 'vi'
    zgen prezto prompt theme 'steeeff'

    # prezto and modules
    zgen prezto
    zgen prezto git
    zgen prezto command-not-found
    zgen prezto syntax-highlighting

    # plugins
    #zgen load /path/to/super-secret-private-plugin
    zgen load zsh-users/zsh-syntax-highlighting

    # generate the init script from plugins above
    zgen save
fi

autoload -U compinit promptinit zmv
compinit
promptinit
#prompt gentoo
prompt steeeff

zstyle ':completion::complete:*' use-cache 1

# Set key (esp. for Esc) delay to 1ms
KEYTIMEOUT=1

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

# Search backwards and forwards with a pattern
bindkey -M vicmd '/' history-incremental-pattern-search-backward
bindkey -M vicmd '?' history-incremental-pattern-search-forward
#
# # set up for insert mode too
bindkey -M viins '^R' history-incremental-pattern-search-backward
bindkey -M viins '^F' history-incremental-pattern-search-forward

if [[ `uname` == 'Darwin' ]]; then

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
    export DYLD_LIBRARY_PATH="/usr/local/opt/icu4c/lib:$DYLD_LIBRARY_PATH"

else
    export EDITOR=vim
    #alias v=vim
    #alias e=emacs
fi
