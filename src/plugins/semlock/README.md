- infos = Information about the semlock plugin is in keys below
- infos/author = Kurt Micheli <kurt.micheli@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage postgetstorage presetstorage postcommit postrollback
- infos/status = maintained global
- infos/description =

## SEMLOCK ##

This global semlock plugin introduces a read lock while `GET` and a read/write lock
while `SET`.

A semaphore is used for the synchronisation and the implemented algorithm favors the writer,
because updates should be propagated soon as possible.

The algorithm is described [here](https://en.wikipedia.org/wiki/Readers%E2%80%93writers_problem#Second_readers-writers_problem).
