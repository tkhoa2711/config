# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# coloring for 'ls' command on Mac OS terminal
export CLICOLOR=1
#export LSCOLORS=ExFxCxDxBxegedabagacad
#export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx     # for black background
export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd      # emulate default coloring on linux 'ls'

# email settings
export EAMIL='ltkhoa2711@gmail.com'
export NAME='Khoa Le'
export SMTPSERVER='smtp.gmail.com'

##
# Your previous /Users/tkhoa2711/.profile file was backed up as /Users/tkhoa2711/.profile.macports-saved_2015-01-15_at_00:27:53
##

# MacPorts Installer addition on 2015-01-15_at_00:27:53: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# OPAM configuration
. /Users/tkhoa2711/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# Setting PATH for Python 3.5
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"
export PATH
