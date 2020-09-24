# AWS
export AWS_CLI_FILE_ENCODING=UTF-8
export AWS_CONFIG_FILE=$HOME/.config/aws/config
export AWS_SHARED_CREDENTIALS_FILE=$HOME/.config/aws/credentials


# Docker
export DOCKER_CONFIG=$HOME/.config/docker


# Python
export PYTHONSTARTUP=$HOME/.config/python/pythonrc
export PYTHON_EGG_CACHE=$HOME/.cache/python/python-eggs
export PYLINTHOME=$HOME/.config/python/pylint


# IPython/Jupyter
export IPYTHONDIR=$HOME/.config/ipython
export JUPYTER_CONFIG_DIR=$HOME/.config/jupyter


# Golang
export GOPATH=$HOME/.config/go
export GO111MODULE=on


# Rust
export RUSTUP_HOME=$HOME/.config/rust/rustup
export CARGO_HOME=$HOME/.config/rust/cargo


# Ruby
export GEM_HOME=$HOME/.config/gem
export PATH=/usr/local/opt/ruby/bin:$PATH
export LDFLAGS="-L/usr/local/opt/ruby/lib"
export CPPFLAGS="-I/usr/local/opt/ruby/include"
export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"


# Postgres
export PSQLRC=$HOME/.config/postgres/psqlrc
export PSQL_HISTORY=$HOME/.cache/postgres/psql_history
export PGPASSFILE=$HOME/.config/postgres/pgpass
export PGSERVICEFILE=$HOME/.config/postgres/pg_service.conf


# ZSH
export CLICOLOR=1
export TERM=xterm-256color
export PS1="%{%F{green}%}%n%{%f%}%{%F{white}%}@%{%f%}%{%F{green}%}%m %{%F{red}%}%1~ %{%f%}%% "
export HISTFILE=$HOME/.cache/zsh/zsh_history


# Miscellaneoud
export SQLITE_HISTORY=$HOME/.cache/sqlite/history
export ECLIPSE_HOME=$HOME/.config/eclipse


# Aliases
alias ll='ls -la'
alias youtube-dl='noglob youtube-dl'
alias git='noglob git'
alias py3='python3'
