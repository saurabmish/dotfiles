# AWS
export AWS_CLI_FILE_ENCODING=UTF-8
export AWS_CONFIG_FILE=$XDG_CONFIG_HOME/aws/config
export AWS_SHARED_CREDENTIALS_FILE=$XDG_CONFIG_HOME/aws/credentials


# Docker
export DOCKER_CONFIG=$XDG_CONFIG_HOME/docker


# Python
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/pythonrc
export PYTHON_EGG_CACHE=$XDG_CACHE_HOME/python/python-eggs
export PYLINTHOME=$XDG_CONFIG_HOME/python/pylint


# IPython/Jupyter
export IPYTHONDIR=$XDG_CONFIG_HOME/ipython
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter


# Golang
export GOPATH=$XDG_CONFIG_HOME/go
export GO111MODULE=on


# Rust
export RUSTUP_HOME=$XDG_CONFIG_HOME/rust/rustup
export CARGO_HOME=$XDG_CONFIG_HOME/rust/cargo
export PATH=$XDG_CONFIG_HOME/rust/cargo/bin:$PATH


# Postgres
export PSQLRC=$XDG_CONFIG_HOME/postgres/psqlrc
export PSQL_HISTORY=$XDG_CACHE_HOME/postgres/psql_history
export PGPASSFILE=$XDG_CONFIG_HOME/postgres/pgpass
export PGSERVICEFILE=$XDG_CONFIG_HOME/postgres/pg_service.conf


# ZSH
export CLICOLOR=1
export TERM=xterm-256color
export PS1="%{%F{green}%}%n%{%f%}%{%F{white}%}@%{%f%}%{%F{green}%}%m %{%F{red}%}%1~ %{%f%}%% "
export HISTFILE=$XDG_CACHE_HOME/zsh/zsh_history


## SSH
export SSH_CONFIG=$XDG_CONFIG_HOME/ssh/config
export SSH_PRIVATE_KEY=$XDG_CONFIG_HOME/ssh/id_rsa

# Miscellaneous
export SQLITE_HISTORY=$XDG_CACHE_HOME/sqlite/history
export ECLIPSE_HOME=$XDG_CONFIG_HOME/eclipse


# Aliases
alias ll='ls -lha'
alias youtube-dl='noglob youtube-dl'
alias git='noglob git'
alias py3='python3'
alias ssh="ssh -F $SSH_PRIVATE_KEY"
