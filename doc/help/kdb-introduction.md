# kdb-introduction(1) -- introduction to kdb

**kdb** provides access to the global key database of Elektra via
command-line. In this manual we give an overview of the tool suite
`kdb`. It is part of Elektra’s tools. The tool suite `kdb` consists
of individual commands. Most commands are independent and some commands
are sharing an executable. Some commands are written as external scripts.

The included commands can be listed via:<br>
`kdb`

External commands can be listed via:<br>
`kdb list-tools`

Only a few commands are enough for daily use.
We can retrieve a key by:<br>
`kdb get user/key`

We store a key permanently with a value given by:<br>
`kdb set user/key value`

We list all available keys arranged below a key by:<br>
`kdb ls user/key`

## SEE ALSO

- For more about tools go on reading [here](/doc/help/kdb.md).
  (`man kdb`)
- Get a [big picture about Elektra](/doc/BIGPICTURE.md)
