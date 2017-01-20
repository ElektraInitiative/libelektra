# How-To: Integrate elektra-merge Into a Debian Package #

We assume that you know what [ucf](https://packages.debian.org/sid/ucf) is and have some
[general knowledge about configuration file handling in Debian](https://wiki.debian.org/ConfigPackages).

This guide explains how to use ucf's new `--three-way-merge-command` functionality in
conjunction with Elektra in order to utilize Elektra's powerful tools in order to allow
automatic three-way merges of your package's configuration during upgrades in a way
that is more reliable than a diff3 merge.  This guide assumes that you are familiar with
ucf already and are just trying to implement the `--three-way-merge-command` option
using Elektra.

## The New Option ##

The addition of the `--three-way-merge-command` option was a part of my Google
Summer of Code Project. This option takes the form:
	--three-way-merge-command command  <New File> <Destination>

Where `command` is the command you would like to use for the merge. `New File` and
`Destination` are the same as always.

## elektra-merge ##

We added a new script to Elektra called [elektra-merge](/scripts/elektra-merge) for use with
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
on the newly mounted KeySets. Once this is finished, either with success or not, the script finishes
by unmounting all but `our` copy of the file to cleanup KDB. Then, if the merge was successful ucf
will replace `ours` with the result providing the package with an automatically merged
configuration which will also be updated in KDB itself.

Additionally, we added two other scripts, `elektra-mount` and `elektra-umount` which act
as simple wrappers for `kdb mount` and `kdb umount`. They work identically but are more
script friendly.

## The Full Command ##

The full command to use `elektra-merge` to perform a three-way merge on a file managed
by ucf is:
	ucf --three-way --threeway-merge-command elektra-merge <New File> <Destination>

That's it! As described above, `elektra-merge` is smart enough to run the whole merge off
of the information from that command and utilizes the new `kdb remount` command to
do so.

## How-To Integrate ##

Integrating `elektra-merge` into a package that already uses ucf is very easy! In `postinst` you
should have a line similar to:
	ucf <New File> <Destination>

or perhaps:
	ucf --three-way <New File> <Destination>

All you must do is in `postinst`, when run with the `configure` option you must mount the
config file to Elektra:
	kdb elektra-mount <New File> <Mounting Destination> <Backend>

Next, you must update the line containing `ucf` with the options `--three-way` and `--threeway-merge-command` like so:
	ucf --three-way --threeway-merge-command elektra-merge <New File> <Destination>

Then, in your `postrm` script, during a purge, you must unmount the config file before deleting it:
	kdb elektra-umount <name>

That's it! With those small changes you can use Elektra to perform automatic three-way merges on any files
that your package uses ucf to handle!

## Example ##

Below is a diff representing the changes we made to the samba-common package in order to allow
automatic configuration merging for `smb.conf` using Elektra. We chose this package because it already
uses ucf to handle `smb.conf` but it frequently requires users to manually merge changes across versions.
Here is the patch showing what we changed:

```diff
diff samba_orig/samba-3.6.6/debian/samba-common.postinst samba/samba-3.6.6/debian/samba-common.postinst
92c92,93
< ucf --three-way --debconf-ok "$NEWFILE" "$CONFIG"
---
> kdb elektra-mount "$CONFIG" system/samba/smb ini
> ucf --three-way --threeway-merge-command elektra-merge --debconf-ok "$NEWFILE" "$CONFIG"
Only in samba/samba-3.6.6/debian/: samba-common.postinst~
diff samba_orig/samba-3.6.6/debian/samba-common.postrm samba/samba-3.6.6/debian/samba-common.postrm
4a5
> 	kdb elektra-umount system/samba/smb
```

As you can see, all we had to do was add the line to mount `smb.conf` during install, update the ucf command to include the
new `--threeway-merge-command` option, and unmount `system/samba/smb` during a purge. It really is that easy!
