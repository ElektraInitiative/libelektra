- infos = Information about the lock plugin is in keys below
- infos/author = Kurt Micheli <e1026558@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage postcommit
- infos/status = maintained unittest tested nodep libc 0
- infos/description =

## LOCK ##

This small plugin lock for each file. The granularity is huge and the logic very simple.

It locks at the `GET` the accessed file with a `$FILENAME$.lock` file.
Other `GET` request end up busy waiting until a `SET` removes the lock file.
