- infos = Information about iconv plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = conv
- infos/placements = postgetstorage presetstorage
- infos/description = Converts values of keys between charsets

## Introduction ##

This plugin is a filter plugin that converts between different character encodings.

## Purpose ##

Consider a user insisting on a `latin1` character encoding because
of some old application. All other users already use, for example,
`UTF-8`. For these users, the configuration files are encoded in
`UTF-8`. So we need a solution for the user with `latin1` to access the
key database with proper encoding.

On the other hand, contemplate an XML file which requires a specific
encoding. But the other key databases work well with the users
encoding. So a quick fix for that backend is needed to feed that XML
file with a different encoding.

Iconv plugin provides a solution for both scenarios. It converts between
many available character encodings. With the plugin’s configuration
the user can change the from and to encoding.  The default values of the
plugin configuration are: `from` encoding will be determined at run time.
`to` encoding is `UTF-8`.

Parameters:
- `to` is per default UTF-8, to have unicode configuration files
- `from` is per default the users locale

Note that for writing the configuration `from` and `to` is swapped. A
key database that requires a specific encoding can make use of it. To
sum up, every user can select a different encoding, but the key databases
are still properly encoded for anyone.


## Example ##

For example ~/.config/iconv.ini should be latin1, but all users
have UTF-8 settings:

	kdb mount iconv.ini /example/iconv ini iconv from=UTF-8,to=ISO-8859-1
	file ~/.config/iconv.ini             # iconv.ini: ISO-8859 text
	kdb get user/example/iconv/a     # converts ISO-8859 to UTF-8
	kdb set user/example/iconv/a öäß # converts UTF-8 to ISO-8859
