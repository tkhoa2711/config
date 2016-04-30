#!/bin/bash

# bail out on error
set -e

for i in "$@"
do
    case $i in
        --uninstall)
            UNINSTALL=true
            shift
            ;;
        *)
            # default option is to install
            ;;
    esac
done

PREFIX_DIR=${PREFIX_DIR:-/usr/local}
CONFIG_DIR=${CONFIG_DIR:-$HOME/source/config}
SYMLINK_FILES=(
    .profile
    .bashrc
    .bash_alias
    .bash_function
    .inputrc
    .colordiffrc
    .emacs
    .vimrc
    .tmux.conf
    .screenrc
    .gitconfig
    .gitignore
    .hgrc
    .hgignore_global
    .sbclrc
    .ccl-init.lisp
    .newlisp-edit.conf
    .ocamlinit
    .conkerrorrc
)

_setup_symlink ()
{
    # setup symlinks
    pushd $HOME >/dev/null
    for FILE in "${SYMLINK_FILES[@]}"; do
        [[ -f "$FILE" ]] && echo "$FILE already exists. Skipped." || ln -s $CONFIG_DIR/$FILE
    done
    popd >/dev/null
}

_setup_emacs ()
{
    emacs -version
    mkdir -p $HOME/.emacs.d
    pushd $HOME/.emacs.d >/dev/null
    ln -s $CONFIG_DIR/.emacs.d/init.el
    ln -s $CONFIG_DIR/.emacs.d/init

    emacs --batch --load "~/.emacs" --eval '(progn (sit-for 5) (message "Done loading emacs"))'
    popd >/dev/null
}

_setup_vim ()
{
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    vim --version
    vim +PluginInstall +qall &>/dev/null
}

_setup_tmux ()
{
    TMUX_VERSION=2.1
    LIBEVENT_VERSION=2.0.21
    NCURSES_VERSION=5.9
    VERSION=$(tmux -V >/dev/null 2>&1)
    # TODO
}

_install ()
{
    _setup_symlink
    _setup_emacs
    _setup_vim
}

_uninstall ()
{
    pushd $HOME >/dev/null
    for FILE in "${SYMLINK_FILES[@]}"; do
        # check if the file is a soft symlink and it points to the config dir
        [[ -L "$FILE" ]] && [[ "$(readlink $FILE)" =~ "$CONFIG_DIR" ]] && rm $FILE && echo "Removed $FILE"
    done
    popd >/dev/null
}

# _setup_emacs_user_directory
if [[ "$UNINSTALL" == "true" ]]; then
    _uninstall
else
    _install
fi

