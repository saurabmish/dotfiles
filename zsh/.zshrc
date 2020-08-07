# AWS
export AWS_CLI_FILE_ENCODING=UTF-8
export AWS_CONFIG_FILE=~/.config/aws/config
export AWS_SHARED_CREDENTIALS_FILE=~/.config/aws/credentials


# Docker
export DOCKER_CONFIG=~/.config/docker


# Git is XDG complaint if ~/.config/git/config present


# Python
export PYTHONSTARTUP=~/.config/python/pythonrc
export PYTHON_EGG_CACHE=~/.config/python/python-eggs
export PYLINTHOME=~/.config/python/pylint


# IPython/Jupyter
export IPYTHONDIR=~/.config/ipython
export JUPYTER_CONFIG_DIR=~/.config/jupyter


# Rust
export RUSTUP_HOME=~/.config/rust/rustup
export CARGO_HOME=~/.config/rust/cargo


# Postgres
export PSQLRC="~/.config/pg/psqlrc"
export PSQL_HISTORY="~/.cache/pg/psql_history"
export PGPASSFILE="~/.config/pg/pgpass"
export PGSERVICEFILE="~/.config/pg/pg_service.conf"


# ZSH
export CLICOLOR=1
export TERM=xterm-256color
export PS1="%{%F{green}%}%n%{%f%}%{%F{white}%}@%{%f%}%{%F{green}%}%m %{%F{blue}%}%1~ %{%f%}%% "


# Aliases
alias ll='ls -la'
alias youtube-dl='noglob youtube-dl'
alias emacs='emacs -q --load ~/.config/emacs.d/init.el'
