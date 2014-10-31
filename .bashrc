
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
# other environment-specific settings here
# ...
