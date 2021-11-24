#!/bin/bash -e

# Install brew if required
if [[ $(command -v brew) == "" ]]; then
  echo "Installing Hombrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

brew update
brew config
# Unlink parallel package, because of conflict with moreutils
brew unlink parallel || >&2 printf 'Unlinking parallel failed.`\n'
brew install augeas \
    antlr \
    antlr4-cpp-runtime \
    bison \
    clang-format \
    dbus \
    discount \
    doxygen \
    flex \
    glib \
    gpgme \
    gradle \
    graphviz \
    libev \
    libgcrypt \
    libgit2 \
    libuv \
    lua \
    moreutils \
    ninja \
    npm \
    openssl \
    pkg-config \
    qt \
    shfmt \
    swig \
    tree \
    xerces-c \
    yajl \
    yaml-cpp \
    zeromq
if [ "$CC" = 'clang' ]; then
    brew install --cask oclint
fi
brew install --cask adoptopenjdk
# Try to install `checkbashisms` (The file server that hosts the package is unfortunately quite unreliable.)
brew install checkbashisms || >&2 printf 'Warning: Unable to install `checkbashims`\n'
