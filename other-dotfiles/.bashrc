
export PS1="\h:\w (\D{%m-%d %H:%M:%S}) \$ "
alias vt="export TERM=vt100"
alias actual="cd \`readlink -f .\`"
export LANG="en_US.UTF-8"
export LC_ALL=C
alias chrome="google-chrome --allow-file-access-from-files"

source ~/.git-completion.bash
source ~/.git-prompt.bash

# https://github.com/fidian/ansi
# 38;5 is set foreground color
# 48;5 is set background color
BRANCH_COLOR='\033[38;5;35m'
RED='\[\033[38;5;196m\]'
GRAYBACK='\[\033[48;5;153m\]'
NIXBACK='\[\033[48;5;177m\]'
LINE='\[\033[K\033[m\]\n'
BRANCH_RESET='\033[m'
RESET='\[\033[m\]'
export PS1="${GRAYBACK}[\t] \w${LINE}"'$(__git_ps1 "(\[${BRANCH_COLOR}\]%s\[${BRANCH_RESET}\]) ")'"${RED}\\$ ""${RESET}"

if [[ -n "$IN_NIX_SHELL" ]]; then
	 export PS1="${NIXBACK}[\t] \w${LINE}"'[nix] $(__git_ps1 "(\[${BRANCH_COLOR}\]%s\[${BRANCH_RESET}\]) ")'"${RED}\\$ ""${RESET}"
fi


export NIX_SHELL_PRESERVE_PROMPT=1
