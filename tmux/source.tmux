#!/usr/bin/env bash
# Custmoized Tmux Sources

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

main() {
    tmux source-file "$CURRENT_DIR/basic.tmuxtheme"
}

main
