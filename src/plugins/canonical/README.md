- infos = Information about the canonical plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained conformant compatible specific unittest tested preview experimental nodoc 
- infos/metadata = transform/canonical transform/canonical/set transform/canonical/# transform/canonical/#/set
- infos/description = one-line description of canonical

## Introduction

Canonicalizes keyvalues matching a lists of valid values, posix extended regex or fnmatch(3) pattern. 

## Examples

 - Lists:
	- Single
		```
			user/key = TRue
				transform/canonical = "caselist:'TRUE', 'ON', 'ENABLE'"
				transform/canonical/set = 1
		
			kdb get user/key 
			1		
		```
		```
			user/key = TRue
				transform/canonical = "list:'TRUE', 'ON', 'ENABLE'"
				transform/canonical/set = 1
		
			kdb get user/key 
			TRue		
		```
	- Array
		```
			user/keyon = ON
				transform/canonical = #1
				transform/canonical/#0 = "list:'TRUE', 'true'"
				transform/canonical/#0/set = "1true"
				transform/canonical/#1 = "list:'on', 'ON'"
				transform/canonical/#1/set = "1on"
		
			user/keytrue = TRUE
				transform/canonical = #1
				transform/canonical/#0 = "list:'TRUE', 'true'"
				transform/canonical/#0/set = "1true"
				transform/canonical/#1 = "list:'on', 'ON'"
				transform/canonical/#1/set = "1on"
		
			kdb get user/keyon 
			1on
		
			kdb get user/keytrue
			1true
		```
	- Enum-like
		```
			user/key = ON
				transform/canonical = #2
				transform/canonical/#0 = "list:'FALSE', 'OFF', 'DISABLE', 'CLOSED'"
				transform/canonical/#1 = "list:'TRUE', 'ON', 'ENABLE', 'OPEN'"
				transform/canonical/#2 = "list:'OTHER', 'NONE', ''"
		
			kdb get user/key
			1
		```
 - Regex:
	```
		user/key = TRue
			transform/canonical = "regex:TR[Uu][Ee]"
			transform/canonical/set = 1
	
		kdb get user/key 
		1		
	```
	```
		user/keyenable = enABLE
			transform/canonical = #1
			transform/canonical/#0 = "regex:TR.?E"
			transform/canonical/#0/set = "1true"
			transform/canonical/#1 = "regex:.*ABLE"
			transform/canonical/#1/set = "1enable"
	
		kdb get user/keyenable
		1enable
	```
 - FNMatch:
	```
		user/key = TRue
			transform/canonical = "fnm:??ue"
			transform/canonical/set = 1
	
		kdb get user/key 
		1		
	```
	```
		user/keyenable = enABLE
			transform/canonical = #1
			transform/canonical/#0 = "fnm:TR*"
			transform/canonical/#0/set = "1true"
			transform/canonical/#1 = "fnm:en*"
			transform/canonical/#1/set = "1enable"
	
		kdb get user/keyenable
		1enable
	```
 - Mixed:
	```
		user/key = ABC
			transform/canonical = #2
			transform/canonical/#0 = "regex:[a-zA-Z]"
			transform/canonical/#1 = "fnm:??3"
			transform/canonical/#2 = "list:'+++','---','==='"
		
		kdb get user/key
		0
	```

## Dependencies

None.

## Examples

None.

## Limitations

None.
