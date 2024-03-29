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


export PATH="$HOME/.poetry/bin:$PATH"

# load Cargo, the package manager for Rust
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
