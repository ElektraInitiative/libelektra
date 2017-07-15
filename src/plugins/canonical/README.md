- infos = Information about the canonical plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained conformant compatible specific unittest tested preview experimental nodoc 
- infos/metadata = transform/canonical/fnmatch transform/canonical/fnmatch/canonical transform/canonical/regex transform/canonical/regex/canonical transform/canonical/fnmatch/# transform/canonical/fnmatch/#/canonical transform/canonical/regex/# transform/canonical/regex/#/canonical transform/canonical/list/sensitive transform/canonical/list/sensitive/canonical transform/canonical/list/insensitive transform/canonical/list/insensitive/canonical transform/canonical/list/sensitive/# transform/canonical/list/sensitive/#/canonical transform/canonical/list/insensitive/# transform/canonical/list/insensitive/#/canonical
- infos/description = one-line description of canonical

## Introduction

Canonicalizes keyvalues matching a lists of valid values, posix extended regex or fnmatch(3) pattern. 

## Examples

 - Lists:
	- Single
		```
			user/key = TRue
				transform/canonical/list/insensitive = "'TRUE', 'ON', 'ENABLE'"
				transform/canonical/list/insensitive/canonical = 1
		
			kdb get user/key 
			1		
		```
		```
			user/key = TRue
				transform/canonical/list/sensitive = "'TRUE', 'ON', 'ENABLE'"
				transform/canonical/list/sensitive/canonical = 1
		
			kdb get user/key 
			TRue		
		```
	- Array
		```
			user/keyon = ON
				transform/canonical/list/sensitive = #1
				transform/canonical/list/sensitive/#0 = "'TRUE', 'true'"
				transform/canonical/list/sensitive/#0/canonical = "1true"
				transform/canonical/list/sensitive/#1 = "'on', 'ON'"
				transform/canonical/list/sensitive/#1/canonical = "1on"
		
			user/keytrue = TRUE
				transform/canonical/list/sensitive = #1
				transform/canonical/list/sensitive/#0 = "'TRUE', 'true'"
				transform/canonical/list/sensitive/#0/canonical = "1true"
				transform/canonical/list/sensitive/#1 = "'on', 'ON'"
				transform/canonical/list/sensitive/#1/canonical = "1on"
		
			kdb get user/keyon 
			1on
		
			kdb get user/keytrue
			1true
		```
	- Enum-like
		```
			user/key = ON
				transform/canonical/list/insensitive = #2
				transform/canonical/list/insensitive/#0 = "'FALSE', 'OFF', 'DISABLE', 'CLOSED'"
				transform/canonical/list/insensitive/#1 = "'TRUE', 'ON', 'ENABLE', 'OPEN'"
				transform/canonical/list/insensitive/#2 = "'OTHER', 'NONE', ''"
		
			kdb get user/key
			1
		```
 - Regex:
	```
		user/key = TRue
			transform/canonical/regex = "TR[Uu][Ee]"
			transform/canonical/regex/canonical = 1
	
		kdb get user/key 
		1		
	```
	```
		user/keyenable = enABLE
			transform/canonical/regex = #1
			transform/canonical/regex/#0 = "TR.?E"
			transform/canonical/regex/#0/canonical = "1true"
			transform/canonical/regex/#1 = ".*ABLE"
			transform/canonical/regex/#1/canonical = "1enable"
	
		kdb get user/keyenable
		1enable
	```
 - FNMatch:
	```
		user/key = TRue
			transform/canonical/fnmatch = "??ue"
			transform/canonical/fnmatch/canonical = 1
	
		kdb get user/key 
		1		
	```
	```
		user/keyenable = enABLE
			transform/canonical/fnmatch = #1
			transform/canonical/fnmatch/#0 = "TR*"
			transform/canonical/fnmatch/#0/canonical = "1true"
			transform/canonical/fnmatch/#1 = "en*"
			transform/canonical/fnmatch/#1/canonical = "1enable"
	
		kdb get user/keyenable
		1enable
	```

## Dependencies

None.

## Examples

None.

## Limitations

None.
