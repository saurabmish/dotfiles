# AWS
export AWS_CLI_FILE_ENCODING=UTF-8
export AWS_DEFAULT_REGION="us-east-2"
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME/aws/config"
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME/aws/credentials"


# Docker
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export MACHINE_STORAGE_PATH="$XDG_DATA_HOME/docker-machine"


# Kubernetes
export MINIKUBE_HOME="$XDG_CONFIG_HOME/minikube"
export KUBEHOME="$XDG_CONFIG_HOME/kube"


# Python
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export PYLINTHOME="$XDG_CONFIG_HOME/python/pylint"
export PYTHON_EGG_CACHE="$XDG_CACHE_HOME/python/python-eggs"
export PYTHONHISTFILE="$XDG_CACHE_HOME/python_history"


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


# Terraform
export TF_PLUGIN_CACHE_DIR="$XDG_CACHE_HOME/terraform.d/plugin-cache"
export TF_CLI_CONFIG_FILE="$XDG_CONFIG_HOME/terraform.d/saurabhqa.tfrc"


# VS Code
export VSCODE_EXTENSIONS="${XDG_DATA_HOME:-~/.local/share}/code-oss/extensions"


# ZSH
export CLICOLOR=1
export TERM=xterm-256color
export PS1="%{%F{green}%}%n%{%f%}%{%F{white}%}@%{%f%}%{%F{blue}%}%m %{%F{red}%}%1~ %{%f%}%% "
export HISTFILE=$XDG_CACHE_HOME/zsh/zsh_history


## SSH
export SSH_CONFIG=$XDG_CONFIG_HOME/ssh/config
export SSH_PRIVATE_KEY=$XDG_CONFIG_HOME/ssh/id_rsa


# Vim
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'
export VIMDOTDIR="$XDG_CONFIG_HOME/vim"


# Miscellaneous
export SQLITE_HISTORY=$XDG_CACHE_HOME/sqlite/history
export ECLIPSE_HOME=$XDG_CONFIG_HOME/eclipse


# Aliases
alias ll='ls -lha'
alias youtube-dl='noglob youtube-dl'
alias git='noglob git'
alias py3='python3'
alias ssh="ssh -F $SSH_PRIVATE_KEY"
