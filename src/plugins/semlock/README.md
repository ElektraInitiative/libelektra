- infos = Information about the semlock plugin is in keys below
- infos/author = Kurt Micheli <kurt.micheli@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage postgetstorage presetstorage postcommit postrollback
- infos/status = maintained reviewed unittest global experimental unfinished nodoc -10000
- infos/description = locks files during usage

## Semlock

This global semlock plugin introduces a read lock while `GET` and a read/write lock
while `SET`.

A semaphore is used for the synchronisation and the implemented algorithm favors the writer,
because updates should be propagated soon as possible.

The algorithm is described [here](https://en.wikipedia.org/wiki/Readers%E2%80%93writers_problem#Second_readers-writers_problem).

The usage of this plugin could lead to deadlocks, due to an ongoing discussion (-10000) ([Link](https://github.com/ElektraInitiative/libelektra/pull/555)).

## /dev/shm

Is the location where the semaphores will be saved. `/dev/shm` should be mounted as tempfs, otherwise the
semaphores can not be created (this issue only appears on older systems). More information [here](http://stackoverflow.com/questions/270113/how-do-i-stop-sem-open-failing-with-enosys).
