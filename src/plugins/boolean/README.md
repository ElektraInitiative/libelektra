- infos = Information about the boolean plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest shelltest tested nodep libc configurable preview nodoc concept
- infos/metadata =
- infos/description =

## Example ##

```
% kdb umount /booltest
% kdb mount bool.dump /booltest dump
% kdb export /booltest ini -c meta=

k1 = true
k2 = tRUe
k3 = i won't be true
k4 = false
k5 = FalsE
k6 = i'm not false
k7 = off
k8 = on
[k1]
check/type = boolean
[k2]
check/type = boolean
[k3]
check/type = boolean
[k4]
check/type = boolean
[k5]
check/type = boolean
[k6]
check/type = boolean
[k7]
check/type = boolean
[k8]
check/type = boolean

% kdb umount /booltest
% kdb mount bool.dump /booltest dump boolean false='FALSE; OFF; 0',true='TRUE; ON; 1',set/true='this should be true',set/false='this should be false'
% kdb export /booltest

k1 = this should be true
k2 = this should be true
k3 = i won't be true
k4 = this should be false
k5 = this should be false
k6 = i'm not false
k7 = this should be false
k8 = this should be true

% kdb set /booltest

Using name user/booltest
Set null value
The command set failed while accessing the key database with the info:
Error (#150) occurred!
Description: not a valid boolean value
Ingroup: plugin
Module: boolean
At: /home/thomas/Dev/libelektra/src/plugins/boolean/boolean.c:277
Reason: i won't be true is not a valid boolean value
Mountpoint: user/booltest
Configfile: /home/thomas/.config/bool.dump.15332:1473271325.447866.tmp
```
