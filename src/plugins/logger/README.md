- infos = Information about the logger plugin is in keys below
- infos/author = Name <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = logging
- infos/placements = prerollback postrollback postcommit postgetstorage
- infos/description =

## Introduction ##

This Plugin writes all Error and Warnings metadata to a log file specified by `config/logfile`.
Additionally it checks every key for metakeys starting with `log/` and logs the keyname, keystring and metakey name.
