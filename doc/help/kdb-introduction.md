kdb-introduction(1) -- introduction to kdb
==========================================

**Elektra** solves the task of accessing the configuration access.
Additionally, a tooling gathered around Elektra helps with
administrative configuration problems that appear every day.
Maybe the administrator needs a cron job that periodically changes the
settings of a service.  Maybe the user wants to have an overview of the
whole configuration to learn what can be tweaked.  Maybe the developer
needs to fully export the configuration the program had when a failure
occurred.  These tasks have in common that they become trivial once a
programmatic access to a global key database exists.

In this manual we give an overview of the command-line tool `kdb`. It
is part of Elektra's tools and performs the mentioned tasks.
`kdb` consists of individual subprograms.  The programs are independent,
but can access a shared part that provides functionality too specific to
be in the library -- for example, pretty printing of error messages and
warnings.  Most parts of this suite are short programs which basically
call `kdbGet()`, do something with the data structure and eventually
write it back using `kdbSet()`.  Note that the command-line tool `kdb`
should not be confused with the class `KDB`.

Every part of the application suite accepts its own command line arguments
and has its own documentation (via man pages accessible using `--help`
and `-H`).  To get an overview of all tools in `kdb`, simply type:  
`kdb`

Furthermore, external programs can also be integrated within
`kdb`. They can be listed via:  
`kdb list-tools`.

Only a few commands are enough for daily use.
We can retrieve a key by:  
`kdb get user/keyname`

We store a key permanently with a value given by:  
`kdb set user/keyname value`

We list all available keys arranged below a key by:  
`kdb ls user/keyname`


## SEE ALSO

- Get a [big picture](BIGPICTURE.md)
- For more about tools go on reading [here](/doc/help/kdb.md).
- To read more about the API (that explains what `kdbGet()`, `kdbSet()` and `KDB` is),
  please continue to read [here](http://doc.libelektra.org/api/current/html)
