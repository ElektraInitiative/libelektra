- infos = Information about the shell plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = postgetstorage postcommit postrollback
- infos/status = preview unfinished nodoc nodep
- infos/description =

## Usage ##

The shell plugin executes shell commandos after set, get or error.



The configuration keys

* `execute/set`
* `execute/get`
* `execute/error`

are used to store the shell commands.



The configuration keys

* `execute/set/return`
* `execute/get/return`
* `execute/error/return`

can be compared against the return values of the shell commandos.



## Example ##

```
 % cat /tmp/log
cat: /tmp/log: No such file or directory

 % kdb mount /tmp/test.ini system/shelltest ini array= shell 'execute/set=echo set >> /tmp/log,execute/get=echo get >> /tmp/log,execute/get/return=0'

 % kdb set system/shelltest
Create a new key system/shelltest with null value

 % cat /tmp/log
get
set
```
