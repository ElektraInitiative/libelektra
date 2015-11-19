elektra-plugins-ordering(7) -- ordering of plugins
==================================================

You should first read [elektra-plugins(7)](/src/plugins/) to get
an idea about plugins.

This document describes how [elektra-plugins(7)](/src/plugins/) are
ordered with an [elektra-backend(7)](elektra-backend.md).

Multiple plugins open up many ways in which they can be arranged.
A simple way is to have one array with pointers to plugins.  To store a
`KeySet`, the first plugin starts off with the `KeySet` passed to it.
The resulting `KeySet` is given to the next plugin.  This is repeated
for every plugin in the array.

To obtain a `KeySet`, the array of plugins is processed the other way
round. An empty `KeySet` is passed to the last plugin. The resulting
`KeySet` is handed over to the previous one until the first plugin
is executed.

This approach has shown not to be powerful enough to express all use
cases by counter-evidence.  Logging should take place after the storage
plugin performs its actions in both directions.  It is not possible to
do this with a *single array* processed in a way as described above.

## Separated lists

Two individual lists (get+set list) of plugins solve the described problem.
Instead of bidirectional processing of
a single list two separate arrays are used.  It turns out that error
scenarios also make this approach unsuccessful.  When a plugin fails,
no other plugin must be executed later on because it might depend on the
previous plugin to work correctly.  In `kdbGet()`, this works well --
the update process will be stopped.  But during `kdbSet()`, changes to
the file system must be reverted. Plugins can leave a lock, temporary
file or journal.  These resources need to be cleaned up properly.

## Error List

So every backend additionally needs a third array that is executed in
error scenarios during `kdbSet()`. Plugins responsible for the cleanup,
rollback or error notification are inserted into it.

The resolver plugin requires this error list to do a proper rollback.
Another use case is logging after a failure has happened.

## Placements

The ordering of plugins inside these three arrays is controlled by
[elektra-contracts(7)](elektra-contracts).
Each of the three arrays has ten slots.  These slots have
names to be referred to in the contract.

Here you see a table that contains all names:

	0     prerollback       getresolver         setresolver  
	1     prerollback       pregetstorage      presetstorage 
	2     prerollback       pregetstorage      presetstorage 
	3     prerollback       pregetstorage      presetstorage 
	4     prerollback       pregetstorage      presetstorage 
	5      rollback            getstorage         setstorage 
	6    postrollback      postgetstorage      precommit     
	7    postrollback      postgetstorage         commit     
	8    postrollback      postgetstorage     postcommit     
	9    postrollback      postgetstorage     postcommit     

how the placement is influenced using infos/placement, infos/ordering
and infos/stacking as described in
[CONTRACT.ini](/doc/CONTRACT.ini).

