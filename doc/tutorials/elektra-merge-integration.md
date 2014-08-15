# How-To: Integrate elektra-merge Into a Debian Package #

This guide explains how to use ucf's new `--three-way-merge-command` functionality in
conjunction with Elektra in order to ultilize Elektra's powerful tools in order to allow
automatic three-way merges of your package's configuration during upgrades in a way
that is more reliable than a diff3 merge.  This guide assumes that you are fimilar with
ucf already and are just trying to impliment the `--three-way-merge-command` option
using Elektra.

## The New Option ##

The addition of the `--there-way-merge-command` option was a part of my Google
Summer of Code Project. This option takes the form:  
	--three-way-merge-command command  <New File> <Destination>
	
Where `command` is the command you would like to use for the merge. `New File` and
`Destination` are the same as always. 

## elektra-merge ##

We added a new script to Elektra called [elektra-merge](script/elektra-merge) for use with
this new option in ucf. This script acts as a liaison between ucf and Elektra, allowing a regular
ucf command to run a `kdb merge` even though ucf commands only pass `New File` and 
`Destination` whereas kdb merge requires `ourpath`, `theirpath`, `basepath`, and `resultpath`.
Since ucf already performs a three-way merge, it keeps track of all the necessary files to do
so, even though it only takes in `New File` and `Destination`. 

In order to use `elektra-merge`, the current configuration file must be mounted to KDB to
serve as `ours` in the merge. The script automatically mounts `theirs`, `base`, and `result` 
using the `kdb remount` command in order to use the same backend as `ours` (since all versions
of the same file should use the same backend anyway) and this way users don't need to worry
about specifying the backend for each version of the file. Then the script attempts a merge 
on the newly mounted KeySets. Once this is finished, either on success or not, the script finishes
by unmouting all but `our` copy of the file to cleanup KDB. Then, if the merge was successful ucf
will replace `ours` with the result providing the package with an automatically merged 
configuration which will also be updated in KDB itself. 

## The Full Command ##

The full command to use `elektra-merge` to perform a three-way merge on a file managed
by ucf is:
	ucf --three-way --threeway-merge-command elektra-merge <New File> <Destination>
	
Thats it! As described above, `elektra-merge` is smart enough to run the whole merge off
of the information from that command and utilizes the new `kdb remount` command to
do so. 

## How-To Integrate ##

Integrating `elektra-merge` into a package that already uses ucf is very easy! In `postinst` you
should have a line similar to:
	ucf <New File> <Destination>
	
or perhaps:
	ucf --three-way <New File> <Destination>
	
All you must do is in `postinst`, when run with the `configure` option you must mount the
config file to Elektra (but only on the first install):
	kdb mount <New File> <Mouting Destination> <Backend>
	
As stated above, you must always have the current version of the config file mounted as `ours` in Elektra. You only
need to have this command run on the first install, if it runs on upgrades there will be issues.

Next, you must update the line containing `ucf` with the options `--three-way` and `--threeway-merge-command` like so:
	ucf --three-way --threeway-merge-command elektra-merge <New File> <Destination>
	
Then, in your `postrm` script, during a purge, you must umount the config file before deleting it: 
	kdb umount <name> 

That's it! With those small changes you can use Elektra to perform automatic three-way merges on any files
that your package uses ucf to handle!

## Example ##

Below is a diff representing the changes we made to the samba-common package in order to allow
automatic configuration merging for `smb.conf` using Elektra. We chose this package because it already
uses ucf to handle `smb.conf` but it frequently requires users to manually merge changes across versions.
Here is the patch showing what we changed:
