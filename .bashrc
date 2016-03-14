# ~/.bashrc

# -------------------------------------------------------------------------
# TIPS
# =========================================================================
#
# __________________ NAVIGATION
#
# C-[a|e]           : move to the start/end of line
# C-[b|f]           : move backward/forward one character
# M-[b|f]           : move backward/forward one word
#
# __________________ EDIT
#
# C-l               : clear screen
# [C|M]-w           : cut from cursor to start/end of word
# C-[u|k]           : cut from cursor to start/end of line
# [C|M]-t           : cut from cursor to end of line
# M-y               : iterate through paste result from C-y
# [C|M]-t           : swap the last two characters/words before the cursor
#
# __________________ HISTORY
#
# C-r               : search as you type
# C-r*              : iterate through the search result
# C-j               : end the search at current history entry
# C-g               : cancel the search and restore original line
# Esc-.             : repeat the previous command's last argument


# -----------------------------------------------------------------------
# choose which branch version to work on
# -----------------------------------------------------------------------
<<COMMENT
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
COMMENT

# -----------------------------------------------------------------------
# OS-specific configs
# -----------------------------------------------------------------------
case "$OSTYPE" in
    linux*)     # Linux
        alias duhere='\du -h --max-depth=1'
        ;;
    
    bsd*)       # BSD
        ;;
    
    solaris*)   # Solaris
        ;;
    
    darwin*)    # Mac OS X
        # alias
        alias duhere='\du -h -d 1'

        # PATH
        export PATH="/usr/local/git/bin:$PATH"  # use the latest git version
        
        # my own config on Mac OS
        alias devdir='cd ~/source/dev'
        alias sourcedir='cd ~/source'
        alias trashdir='cd ~/.Trash'
        ;;

        # autojump
        # [[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
    
    cygwin)     # Cygwin on Windows
        ;;
    
    msys)       # Windows, MinGW
        ;;

    *)
        echo "Unknow OSTYPE: $OSTYPE"
        ;;
esac

# -----------------------------------------------------------------------
# source the utlity files containing aliases and functions for bash
BASH_UTIL_FILE_LIST=(.bash_alias .bash_function .bash_completion .bash_color .bash_local)
BASH_UTIL_FILE_DIR=$HOME
for file in $BASH_UTIL_FILE_LIST; do
    [[ -f $BASH_UTIL_FILE_DIR/$file ]] && source $BASH_UTIL_FILE_DIR/$file
done

# -----------------------------------------------------------------------
# aliases
# -----------------------------------------------------------------------
# directory
alias l='ll'
alias ls='ls -F -color'
alias ll='ls -ahltr -color'
alias l.='ls -adlrt .* -color'
alias lu='ls -alrt -F -color | grep $USER'
alias lal='ls -altr -color'
alias lcl='while true; do c; ll; sleep 2; done'
alias j='autojump'

#alias mv='mv-i'
#alias rm='rm -i'
#alias mkdir='mkdir -pv'

# processes, jobs
# alias jo='jobs -l'
alias p='ps -aef | grep $USER'
alias psm='ps -u ${USER} -f --sort comm'
alias pst='ps -eLf'                     # display threads also
alias pgk="ps -eaf | grep $1; ps -eaf | grep $1 | grep -v grep | sed -e \"s/  */ /g\" | cut -d' ' -f2"

# datetime
alias now='date +"%T"'
alias nowtime=now
alias nowdate='date + "%d-%m-%Y"'

# system info
alias meminfo='free -m -l -t'
alias psmem='ps auxf | sort -nr -k 4'   # get top processes eating memory
alias pscpu='ps auxf | sort -nr -k 3'   # get top processes etaing cpu
alias cpuinfo=lscpu

# disk usage
alias du='du -ksh *'
alias duh='\du -h'
#alias df='df -ah'
#alias du='du -ach'

# networking
alias ping='ping -c 5'                  # stop after sending count ECHO_REQUEST packets
alias pingfast='ping -c 100 -s.2'       # don't wait for 1-second interval, move fast
alias ports='netstat -tulanp'           # list all tcp/udp ports

alias wget='wget -c'                    # resume wget by default

# dotfiles
alias vimrc='vim ~/.vimrc'              # edit these as swift as possible
alias bashrc='vim ~/.bashrc'
alias emacsrc='vim ~/.emacs'
alias hgrc='vim ~/.hgrc'

# editing
alias vi='vim -X'
alias vim='vim -X'
#alias diff=colordiff
alias diff='git diff --no-index'        # in case colordiff is not available
#alias diff='vim -d'                    # enable if you prefer using vim diff
alias emacs='TERM=xterm-256color emacs -nw' # allow emacs to remain beautiful as usual in tmux
alias e='TERM=xterm-256color emacsclient'

# compilation
alias mj='makec -j10' # beat it!!!
alias tu='$EDITOR Tupfile'

# search
alias gerp='grep'                       # common typo
alias grep='grep -v grep | grep --color'
alias egrep='egrep -n --color'

# misc
alias c='clear'                         # equivalent to Ctrl-L
alias br="printf '\e[47m%*s\n' \"${COLUMNS:-$(tput cols)}\" ''" # print out a visual break line
alias bc='bc -l'                        # start calculator with math support
alias path='echo -e ${PATH//:/\\n}'     # show $PATH in a nice way
alias h='history'
alias mnt='mount | column -t'           # show results of 'mount' in a nice format

# -----------------------------------------------------------------------
# editing
# -----------------------------------------------------------------------
export EDITOR=vim

# helper function to edit dotfiles easily
#dot()
#{
#    $EDITOR ~/.$1
#}

# -----------------------------------------------------------------------
# history settings
# -----------------------------------------------------------------------
export HISTFILESIZE=100000              # we got plenty of spaces, what should we do
export HISTSIZE=100000
export HISTTIMEFORMAT='%F %T '
#HISTCONTROL=ignoreboth                 # ignore duplicate lines, lines starting with space

shopt -s histappend                     # append to the histoy file, don't overwrite it

PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# check the window size after each command and update the values of
# LINES and COLUMNS if necessary
shopt -s checkwinsize


# -----------------------------------------------------------------------
# PATH settings
# -----------------------------------------------------------------------
export EMACS_D=~/.emacs.d


# -----------------------------------------------------------------------
# python
# -----------------------------------------------------------------------
#alias virtualenv='~/source/py-env0/bin/virtualenv'
export PATH=~/source/py-env0/bin:$PATH
export WORKON_HOME="$HOME/.virtualenvs"
source virtualenvwrapper.sh

# default python working environment
# workon default

# -----------------------------------------------------------------------
# GTest
# -----------------------------------------------------------------------
export GTEST_DIR=~/source/gtest

# -----------------------------------------------------------------------
# command prompt
# -----------------------------------------------------------------------

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
