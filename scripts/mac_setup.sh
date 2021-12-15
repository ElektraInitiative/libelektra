#!/usr/bin/env bash

# Install brew if required
if [[ $(command -v brew) == "" ]]; then
	echo "Installing Hombrew"
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

brew update
brew config
# Unlink parallel package, because of conflict with moreutils
brew unlink parallel || printf >&2 'Unlinking parallel failed.`\n'
brew bundle --file=- <<-EOS
brew "antlr"
brew "antlr4-cpp-runtime"
brew "bison"
brew "clang-format"
brew "dbus"
brew "discount"
brew "doxygen"
brew "flex"
brew "glib"
brew "gpgme"
brew "gradle"
brew "graphviz"
brew "libev"
brew "libgcrypt"
brew "libgit2"
brew "libuv"
brew "lua"
brew "moreutils"
brew "ninja"
brew "npm"
brew "openssl"
brew "pkg-config"
brew "qt"
brew "shfmt"
brew "swig"
brew "tree"
brew "xerces-c"
brew "yajl"
brew "yaml-cpp"
brew "zeromq"

if [ "$CC" = 'clang' ]; then
	brew install --cask oclint
	brew install --cask oclint
fi

brew install --cask adoptopenjdk
# Try to install `checkbashisms` (The file server that hosts the package is unfortunately quite unreliable.)
brew install checkbashisms || printf >&2 'Warning: Unable to install `checkbashims`\n'
