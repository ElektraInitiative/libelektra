# kdb-find-tools(1) -- The tool for finding tools

## SYNOPSIS

```sh
kdb find-tools [-h] [--warnings] [--good] [--alltags] [-n NAME] [-a AUTHOR] [-d DATE]
               [-t TAGS [TAGS ...]] [-b BRIEF] [-e EXECUTE]
```

## DESCRIPTION

If you are looking for a tool, then you have found the right tool to find tools!
`kdb find-tools` provides search and list functionality for tools.

Just enter `kdb find-tools` to get a list of names, type and short description of all available tools.

If you are looking for something special, then there are two ways:

1. Tag Search:
   Type `kdb find-tools --alltags` to get a list of all Tags in use.
   Then you can search with `kdb -t [TAGS [TAGS ...]]`

2. Full Text Search:
   - `kdb find-tools -n NAME` to search for a script name.
   - `kdb find-tools -b BRIEF` to search for a short text.
   - `kdb find-tools -a AUTHOR` to search for an author.
   - `kdb find-tools -d DATE` to search for a creation date.
   - `kdb find-tools -e EXECUTE` to search for a type.

All methods can be combined. For example if you search all bash scripts which do some configuration work.
You can type `kdb find-tools -t configuration -e bash`.

## The Right Way to Add Your Script to the Find Tools

Meta Tags as comments in the beginning of a script are parsed.
Mate Tags start with an `@`, here is a list of all Meta Tags:

| MetaTag  | Meaning                                                         |
| -------- | --------------------------------------------------------------- |
| \@author | Names and Emails (in <>) of the Authors as comma separated list |
| \@brief  | A Short Description (One Line!)                                 |
| \@tags   | Comma Separated List of Tags                                    |
| \@date   | Date when the script was created, use DD.MM.YYYY as format      |

Do not mind the '\' at the beginning it is a doxygen escaping.

Beware, that these metatags should be applied at the beginning of the file (in the first 10 rows)!

## Example

```sh
#!/bin/sh
#
# @author Kurt Micheli <kurt.micheli@libelektra.org>
# @brief This is an example of a build script
# @date 31.10.2018
# @tags configure, build
```

## Notes

The Metatag System of Epydoc is used (http://epydoc.sourceforge.net/manual-fields.html#module-metadata-variables)
and extended with special tags.
