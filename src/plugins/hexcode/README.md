- infos = Information about hexcode plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = code
- infos/placements = postgetstorage presetstorage
- infos/description = Decoding/Encoding engine which escapes unwanted characters.

## Introduction ##

This code plugin translates each unwanted character into a two cypher hexadecimal character. The escape character itself always needs to be encoded, otherwise the plugin would try to interpret the following two characters in the text as a hexadecimal sequence. 

## Restrictions ##

- The escape character itself always needs to be encoded, otherwise the plugin would try to interpret the following two characters in the text as a hexadecimal sequence. 		
- The length of the resulting string increases. In the worst case the hexcode plugin makes the value three times larger.	

## Example ##

Consider the following input:

	value=abc xyz
	
Assuming the escape character is % the input would be encoded to:	

	value%3Dabc%20xyz

## Usage ##

Add `ccode` to `infos/needs` for any plugin that you want to be filtered by ccode.
