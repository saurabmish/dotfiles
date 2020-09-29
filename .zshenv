# Contains exported variables that should be available to other programs.
# This includes $PATH, $EDITOR, $PAGER, and $ZDOTDIR.

# ZSH
export ZDOTDIR="${ZDOTDIR:-$HOME/.config/zsh}"

# Default editor
export VISUAL=subl
export EDITOR="$VISUAL"

# XDG compliant home directory
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
