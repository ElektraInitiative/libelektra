- infos = Information about type plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description = Copies meta data to keys using typebing

## Introduction ##

This plugin is a check plugin that checks for `COBRA` types.

## Additional Information ##

A common and successful type system happens to be CORBA with IDL[43,14]. The system is outstanding because of the many mappings it provides to other programming languages. 

The TYPE CHECKER PLUGIN supports all basic CORBA types: short, unsigned_short, long, unsigned_long, long_long, unsigned_long_long, float, double, char, boolean, any and octet. In Elektra octet is the same as char. When checking any it will always be successful, re- gardless of the content. Elektra also added other types. empty will only yield true if there is no value. string allows any non-empty sequence of octets. 

Sometimes the type should expresses that, for example, both an empty or another type is valid. In Elektra a space-separated list of types expresses that. If any of those types match,the whole type is valid. For example, the type string empty equals the type any. This facility builds a union of the sets of instances existing types specify. 

`enum` works with a list of choices. Any of these choices conﬁrms to the type, others do not. For example, the type FSType accepts all ﬁle system names, for example, ext2, jfs or vfat. 

The `CORBA` type system goes far beyond these basic types. In IDL, it is also allowed to deﬁne classes, interfaces and generic containers. These user-deﬁned types are however not useful on a single Key. 

To sum up, many basic types like int or char are convenient and CORBA ensures that they can be converted to the speciﬁc type of the programming language.

## Restrictions ##

The `CORBA` type system alsohas its limits. The types string and enum, however, can be unsatisfactory. While string is too general and makes no limit on how the sequence of characters is structured, the enumeration is too ﬁnite. For example, it is not possible to say that a string is not allowed to have a speciﬁc symbol in it.
