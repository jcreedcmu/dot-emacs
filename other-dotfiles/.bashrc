
export PS1="\h:\w (\D{%m-%d %H:%M:%S}) \$ "
alias vt="export TERM=vt100"
alias actual="cd \`readlink -f .\`"
#export LANG="en_US.UTF-8"
#export LC_ALL=C
alias chrome="google-chrome --allow-file-access-from-files"

source ~/.git-completion.bash
source ~/.git-prompt.bash

# https://github.com/fidian/ansi
# 38;5 is set foreground color
# 48;5 is set background color
BRANCH_COLOR='\033[38;5;35m'
RED='\[\033[38;5;196m\]'
GRAYBACK='\[\033[48;5;33m\]'  # Medium blue (from 256-color palette)
NIXBACK='\[\033[48;5;177m\]'
LINE='\[\033[K\033[m\]\n'
BRANCH_RESET='\033[m'
RESET='\[\033[m\]'
export PS1="${GRAYBACK}[\t] \w${LINE}"'$(__git_ps1 "(\[${BRANCH_COLOR}\]%s\[${BRANCH_RESET}\]) ")'"${RED}\\$ ""${RESET}"

if [[ -n "$IN_NIX_SHELL" ]]; then
	 export PS1="${NIXBACK}[\t] \w${LINE}"'[nix] $(__git_ps1 "(\[${BRANCH_COLOR}\]%s\[${BRANCH_RESET}\]) ")'"${RED}\\$ ""${RESET}"
fi


export PATH=$PATH:/home/jcreed/bin


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Start SSH agent
if [ -z "$SSH_AUTH_SOCK" ] ; then
    eval $(ssh-agent) >/dev/null
fi

export NIX_SHELL_PRESERVE_PROMPT=1
. "$HOME/.cargo/env"

# Wasmer
export WASMER_DIR="/home/jcreed/.wasmer"
[ -s "$WASMER_DIR/wasmer.sh" ] && source "$WASMER_DIR/wasmer.sh"

# Lean
. ~/.elan/env
