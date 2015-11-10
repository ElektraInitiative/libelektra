- infos = Information about CCODE plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = code
- infos/placements = postgetstorage presetstorage
- infos/description = Decoding/Encoding engine which escapes unwanted characters.

## Introduction ##

When writing a C string in C code some characters cannot be expressed 
directly but have a special one letter abbreviation. 
The ccode plugin allows us to map any single escaped 
character to be replaced by another single character and vice versa. 
The user can conﬁgure this mapping.

## Restrictions ##

This method of encoding characters is not as powerful as the hexcode plugin in terms of reduction. 
The hexcode plugin allows reduction of the character set to '0'-'9', 'a'-'f' and one escape character. 
So it can represent any key value with only 17 characters. 
On the other hand, ccode cannot reduce the set more than by half.


## Usage ##

Add 'ccode' to 'infos/needs' for any plugin that you want to be filtered by ccode.
