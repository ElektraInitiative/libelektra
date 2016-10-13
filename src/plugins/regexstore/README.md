- infos = Information about the regexstore plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/status = difficult preview unfinished
- infos/description =

## Introduction ##

Allows regular expressions to be applied as storage plugin.
The idea is to have something like "lazy lenses" with regex
only on relevant parts of a file. It is unclear if this is
a good idea and we do not encourage you to use this plugin.
Currently it can only read and the potential harm is limited.

In the configuration, below the key (that must also exist)

    regexstore

other keys define which regex are applied on a text file.

The name of these config keys (with .../regexstore/ stripped of)
will be used to build up the names of the keys:
- #[0-9] will be replaced what the regex matched
- If no or the wrong placeholder occurs in the name,
  the keys will overwrite themselves, the last wins then.

The value of the config key is as follows:
- the first three letters need to be '#[0-9 '.
According to this number the value of the key will be set.

The key also might contain metadata.
- the only characters allowed in meta values are '#[0-9]'.
the metadata will be replaced by the regex match then.


## Limitations ##

The semantics of how the regex works is unsteady and the plugin should
be avoided in productive use.

Currently the storage plugin does not have write support. When writing
is added, overlapping regex need to be disallowed.

## Usage ##

### vim config ###

First mount the regexstore with some at least one config key:

    kdb mount vimrc /vim regexstore "regexstore=root,regexstore/map/#2=#1 map ([^ 
    ]*) ([^ 
    ]*)"

(the character classes contain a space and a newline)

So lets say we have a .vimrc with the content:

    something else...
    map map Q :qa<CR>
    something else...
    map <C-Q> :qa<CR>
    something else...

then we will get two keys with:

    user/vim/map/:qa<CR>

### emacs config ###

    kdb mount emacs /emacs

Suppose we want to match:

    (global-set-key (kbd "<escape>")      'keyboard-escape-quit)

TODO.. not finished
