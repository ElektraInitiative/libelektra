kdb-find-tools(1) -- The tool for finding tools
================================================================

## SYNOPSIS

`kdb find-tools` [-h] [--warnings] [--good] [--alltags] [-n NAME] [-a AUTHOR] [-d DATE] [-t TAGS [TAGS ...]] [-b BRIEF] [-e EXECUTE]

## DESCRIPTION

If you are looking for a tool, then you have found the right tool to find tools!
`kdb find-tools` provides a search and list functionality for tools.

Just enter `kdb find-tools` to get a list of names, type and short description of all available tools.

If you are looking for something special, then there are two ways:

1. Tag Search:
   Type `kdb find-tools --alltags` to get a list of all Tags in use.
   Then you can search with `kdb -t [TAGS [TAGS ...]]`

2. Full Text Search:
   * `kdb find-tools -n NAME` to search for a script name.
   * `kdb find-tools -b BRIEF` to search for a short text.
   * `kdb find-tools -a AUTHOR` to search for a author.
   * `kdb find-tools -d DATE` to search for a creation date.
   * `kdb find-tools -e EXECUTE` to search for a type.

All methods can be combined. For example if you search all bash scripts which do some configuration work.
You can type `kdb find-tools -t configuration -e bash`.

## The right Way to add your script to the find tools ##

Meta Tags as comments in the beginning of a script are parsed.
Mate Tags start with an `@`, here is a list of all Meta Tags:

| MetaTag   | Meaning                                                              |
|-----------|----------------------------------------------------------------------|
| \@author  | Names and Emails (in <>) of the Authors as comma separated list      |
| \@brief   | A Short Description (One Line!)                                      |
| \@tags    | Comma Separated List of Tags, there is a list of common tags below   |
| \@date    | Date when the script was created, use DD.MM.YYYY as format           |

Do not mind the '\' at the beginning it is a doxygen escaping.

Beware, that these metatags should be applied at the beginning of the file (in the first 10 rows)!

## Tags ##

List of Common Tags:

| \@tags     | Description                                     |
|-----------|-------------------------------------------------|
| configure | This script is used for the build configuration |
| convert   | This script is used convert things              |
| generator | This script is a generator                      |
| creator   | This script creates things                      |
| env       | This script does some env stuff                 |
| mount     | This script mounts things                       |
| reformat  | This script reformats things                    |
| debian    | Special script for debian system                |


If you choose to add a tag to the `@tags` then do not forget to add it in the tags map of the `find-tools` script
and in the table here.

## Example ##

\verbatim

        <Start of File>
        #!/usr/bin/bash
        #
        # @author Kurt Micheli <kurt.micheli@libelektra.org>
        # @brief This is a example
        # @date 01.06.2016
        # @tags configure, creator, arch

\endverbatim


## Notes ##

The Metatag System of Epydoc is used (http://epydoc.sourceforge.net/manual-fields.html#module-metadata-variables)
and extended with special tags.
