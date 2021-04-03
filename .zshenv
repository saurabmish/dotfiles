# Contains exported variables that should be available to other programs.
# This includes $PATH, $EDITOR, $PAGER, and $ZDOTDIR.

# XDG compliant home directory
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# ZSH
export ZDOTDIR="${ZDOTDIR:-$HOME/.config/zsh}"

# Vim
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'
export VIMDOTDIR="$XDG_CONFIG_HOME/vim"

# Default editor
export VISUAL=mvim
export EDITOR="$VISUAL"
