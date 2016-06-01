# Meta #

Meta tags as comments in the beginning of a script are parsed by the meta script
The aim of this script is to find the right script in no time!

The Metatag System of Epydoc is used (http://epydoc.sourceforge.net/manual-fields.html#module-metadata-variables)
and extendet with the following special tags:

| MetaTag   | Meaning                                                              |
|-----------|----------------------------------------------------------------------|
| \@date    | Date when the script was created, use DD.MM.YYYY as format           |
| \@desc    | A Short Description (One Line!)                                      |
| \@author  | Name of the Author and email in < >                                  |
| \@tags    | Comma Seperated List of Tags, there is a list of common tags below   |
| \@system  | Name of the System that the script is intended for (comma seperated) |
| \@depends | Other script names that depend on (the output) of this script        |

Do not mind the '\' at the beginning it is a doxygen escaping.

Because there are not only python scripts, we use the @ declaration, inside the comment.
Beware, that these metatags should be applied at the beginning of the file (in the first 10 rows)

## Tags ##

If you choose to add a tag to the `\@tags` then do not forget to add it in the tags map of the `meta` script.

## How to add Meta info to a script ##

Just set the Tags as described above as comments at the start (within 10 lines) of your script.


Example from `meta`:

```
<Start of File>
#!/usr/bin/env python
#
# @author Sebastian Bachmann support by <kurt.micheli@libelektra.org>
# @desc This script is used to organize scripts
# @date 10.05.2016
# @tags
# @system all
```
