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
        [[ -f "$FILE" ]] && echo "$FILE already exists. Skipped." || ( ln -s $CONFIG_DIR/$FILE && echo "Setup $HOME/$FILE" )
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
    popd >/dev/null

    emacs --batch --load ~/.emacs --eval '(progn (sit-for 5) (message "Done loading emacs"))'
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
    CURRENT_VERSION=$(tmux -V 2>/dev/null)
    if [[ "$CURRENT_VERSION" =~ $TMUX_VERSION ]]; then
        return
    fi

    TARGET_DIR=$HOME/local
    mkdir -p $TARGET_DIR

    local TMUX_NAME=tmux-${TMUX_VERSION}
    local LIBEVENT_NAME=libevent-${LIBEVENT_VERSION}-stable
    local NCURSES_NAME=ncurses-${NCURSES_VERSION}

    # download source files for tmux, libevent, and ncurses
    wget -O tmux-${TMUX_VERSION}.tar.gz http://sourceforge.net/projects/tmux/files/tmux/tmux-${TMUX_VERSION}/tmux-${TMUX_VERSION}.tar.gz/download
    wget https://github.com/downloads/libevent/libevent/libevent-${LIBEVENT_VERSION}-stable.tar.gz
    wget ftp://ftp.gnu.org/gnu/ncurses/ncurses-${NCURSES_VERSION}.tar.gz

    # extract files, configure and compare

    # libevent installation
    tar xvzf ${LIBEVENT_NAME}.tar.gz
    cd $LIBEVENT_NAME
    ./configure --prefix=$TARGET_DIR --disable-shared
    make
    make install
    cd -

    # ncurses installation
    tar xvzf ${NCURSES_NAME}.tar.gz
    cd $NCURSES_NAME
    ./configure --prefix=$TARGET_DIR
    make
    make install
    cd -

    # tmux installation
    tar xvzf ${TMUX_NAME}.tar.gz
    cd $TMUX_NAME
    local CFLAGS="-I$TARGET_DIR/include -I$TARGET_DIR/include/ncurses"
    local LDFLAGS="-L$TARGET_DIR/lib -L$TARGET_DIR/include -L$TARGET_DIR/include/ncurses"
    ./configure CFLAGS=$CFLAGS LDFLAGS=$LDFLAGS
    CPPFLAGS=$CFLAGS LDFLAGS="-static $LDFLAGS" make
    cp tmux $TARGET_DIR/bin
    cd -

    # verification
    tmux -V
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

