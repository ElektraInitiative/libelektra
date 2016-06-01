# Kdb Meta #

Meta tags as comments in the beginning of a script are parsed by the meta script.
The aim of `kdb meta` is to find the right script in no time!

The Metatag System of Epydoc is used (http://epydoc.sourceforge.net/manual-fields.html#module-metadata-variables)
and extended with the following special tags:

| MetaTag   | Meaning                                                              |
|-----------|----------------------------------------------------------------------|
| \@date    | Date when the script was created, use DD.MM.YYYY as format           |
| \@brief    | A Short Description (One Line!)                                      |
| \@author  | Name of the Author and email in < >                                  |
| \@tags    | Comma Separated List of Tags, there is a list of common tags below   |

Do not mind the '\' at the beginning it is a doxygen escaping.

Because there are not only python scripts, we use the '@' declaration, inside the comment.
Beware, that these metatags should be applied at the beginning of the file (in the first 10 rows)

## Tags ##

List of Common Tags:

| \@tags    | Description                                     |
|-----------|-------------------------------------------------|
| configure | This script is used for the build configuration |
| convert   | This script is used convert things              |
| generator | This script is a generator                      |
| creator   | This script creates things                      |
| env       | This script does some env stuff                 |
| mount     | This script mounts things                       |
| reformat  | This script reformats things                    |
| debian    | Special script for debian system                |


If you choose to add a tag to the `\@tags` then do not forget to add it in the tags map of the `meta` script
and in the table here.

## How to add Meta info to a script ##

Just set the Tags as described above as comments at the start (within 10 lines) of your script.

Example from `meta`:

```
<Start of File>
#!/usr/bin/bash
#
# @author Kurt Micheli <kurt.micheli@libelektra.org>
# @brief This is a example
# @date 01.06.2016
# @tags configure, creator, arch
```
