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

        export GREP_COLOR_OPTION='-color'

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
# editing
# -----------------------------------------------------------------------
export EDITOR=vim

# emacs FTW!!!
set -o emacs

# -----------------------------------------------------------------------
# history settings
# -----------------------------------------------------------------------
export HISTFILESIZE=100000              # we got plenty of spaces, what should we do
export HISTSIZE=100000
export HISTTIMEFORMAT='%F %T '
export HISTCONTROL=ignoredups           # ignore duplicate lines

shopt -s histappend                     # append to the histoy file, don't overwrite it

PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# check the window size after each command and update the values of
# LINES and COLUMNS if necessary
shopt -s checkwinsize

# -----------------------------------------------------------------------
# command prompt
# -----------------------------------------------------------------------
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
    #export PS2="> "
    #export PS4="+ "
}

set_prompt

# -----------------------------------------------------------------------
# NVM
# -----------------------------------------------------------------------

# https://stackoverflow.com/questions/23556330/run-nvm-use-automatically-every-time-theres-a-nvmrc-file-on-the-directory
enter_directory()
{
    if [[ $PWD == $PREV_PWD ]]; then
        return
    fi

    PREV_PWD=$PWD
    if [[ -f ".nvmrc" ]]; then
        nvm use
        NVM_DIRTY=true
    elif [[ $NVM_DIRTY = true ]]; then
        nvm use default
        NVM_DIRTY=false
    fi
}

export PROMPT_COMMAND=enter_directory

# -----------------------------------------------------------------------
# load other settings here
# -----------------------------------------------------------------------
BASH_UTIL_FILE_LIST=(.shrc .bash_completion .bash_color)
BASH_UTIL_FILE_DIR=$HOME
for file in "${BASH_UTIL_FILE_LIST[@]}"; do
    [[ -f $BASH_UTIL_FILE_DIR/$file ]] && echo "Loading $file .." && source $BASH_UTIL_FILE_DIR/$file
done

# ---------------------------------------------------------------------------------------------------------------------------------
# autocomplete
# ---------------------------------------------------------------------------------------------------------------------------------
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

