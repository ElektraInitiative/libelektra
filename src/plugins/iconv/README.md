- infos = Information about iconv plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = conv
- infos/placements = pregetstorage postgetstorage presetstorage
- infos/description = Converts values of keys between charsets

## Introduction ##

This plugin is a filter plugin that converts between different character encodings,
or, if the `checkfile` key is set, validates the fileencoding before reading the file.

## Purpose ##

Consider a user insisting on a `latin1` character encoding because
of some old application. All other users already use, for example,
`UTF-8`. For these users, the conﬁguration ﬁles are encoded in
`UTF-8`. So we need a solution for the user with `latin1` to access the
key database with proper encoding.

On the other hand, contemplate an XML ﬁle which requires a speciﬁc
encoding. But the other key databases work well with the users
encoding. So a quick ﬁx for that backend is needed to feed that XML
ﬁle with a different encoding.

Iconv plugin provides a solution for both scenarios. It converts between
many available character encodings. With the plugin’s conﬁguration
the user can change the from and to encoding.  The default values of the
plugin conﬁguration are: `from` encoding will be determined at run time.
`to` encoding is `UTF-8`.

Note that for writing the conﬁguration `from` and `to` is swapped. A
key database that requires a speciﬁc encoding can make use of it. To
sum up, every user can select a different encoding, but the key databases
are still properly encoded for anyone.
