if [ -n "$1" ]; then
  SELECTION=$1
else
  echo ""
  echo "Select a BRANCH to use [default: trunk]"
  echo "  (1) trunk"
  echo "  (2) release"
  echo ""
  read SELECTION
fi

case ${SELECTION} in
  1)
     export BRANCH=trunk
     ;;
  2)
     export BRANCH=release
     ;;
  *)
     export BRANCH=trunk
     ;;
esac


# -----------------------------------------------------------------------
# aliases
alias c='clear'
alias p='ps -aef | grep $USER'
alias duhere='du -h --max-depth=1'
alias ls='ls -F --color'
alias lcl='while true; do c; ll; sleep 2; done'
alias ll='ls -hltr --color'
alias lal='ls -altr --color'
alias l.='ls -adlrt .* --color'
alias lu='ls -alrt -F --color | grep $USER'
alias psm='ps -u ${USER} -f --sort comm'
alias pst='ps -eLf' # display threads also
#alias diff=colordiff
alias grep='grep --color'
alias egrep='egrep --color'
alias h='history'
alias meminfo='free -m -l -t'
alias psmem='ps auxf | sort -nr -k 4'
alias pscpu='ps auxf | sort -nr -k 3'
#alias du='du -ach'
alias du='du -ksh *'
#alias rm='rm -i'
alias vi='vim -X'
alias vim='vim -X'
alias pgk="ps -eaf | grep $1; ps -eaf | grep $1 | grep -v grep | sed -e \"s/  */ /g\" | cut -d' ' -f2"

# -----------------------------------------------------------------------
# *rc files
alias vimrc="vim ~/.vimrc"
alias bashrc="vim ~/.bashrc"

# -----------------------------------------------------------------------
# editor
export EDITOR=vim

# -----------------------------------------------------------------------
# history settings
export HISTFILESIZE=100000
export HISTSIZE=100000
export HISTTIMEFORMAT='%F %T '

shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# -----------------------------------------------------------------------
# command prompt

# START_FG_COLOR="\e[o;34m"
# START_BG_COLOR="\e[47m"
# END_COLOR="\e[0m"
# export PS1="${START_FG_COLOR}${START_BG_COLOR}\u@\h \w> ${END_COLOR}"
#
# function prompt {
#   local BLUE="\[\033[0;34m\]"
#   local DARK_BLUE="\[\033[1;34m\]"
#   local RED="\[\033[0;31m\]"
#   local DARK_RED="\[\033[1;31m\]"
#   local NO_COLOR="\[\033[0m\]"
#   case $TERM in
#       xterm*|rxvt*)
#           TITLEBAR='\[\033]0;\u@\h:\w\007\]'
#           ;;
#       *)
#           TITLEBAR=""
#           ;;
#   esac
#   PS1="\u@\h [\t]> "
#   PS1="${TITLEBAR}\
#   $BLUE\u@\h $RED[\t]>$NO_COLOR "
#   PS2='continue-> '
#   PS4='$0.$LINENO+ '
# }

set_prompt ()
{
    local BLACK="\[\e[0;30m\]"
    local RED="\[\e[0;31m\]"
    local GREEN="\[\e[0;32m\]"
    local YELLOW="\[\e[0;33m\]"
    local BLUE="\[\e[0;34m\]"
    local PURPLE="\[\e[0;35m\]"
    local CYAN="\[\e[0;36m\]"
    local WHITE="\[\e[0;37m\]"
    local RESET="\[\e[0m\]"

    export PS1="${GREEN}\h${WHITE}:${GREEN}\u${YELLOW}[$BRANCH] ${CYAN}\t ${PURPLE}\w ${RESET}"
    #export PS1="\[\e[0;32m\]\h\[\e[0m\]:\[\e[0;32m\]\u\[\e[0;33m\][${BRANCH}] \[\e[0;36m\]\t \[\e[0;31m\]\w \[\e[0m\]"
    #export PS2="> "
    #export PS4="+ "
}

set_prompt

# -----------------------------------------------------------------------
# other environment-specific settings here
# ...
