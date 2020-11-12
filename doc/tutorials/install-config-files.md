# How-To: install configuration files

The `install-config-file` tool makes using Elektra for a configuration file easier.
Please refer to its man page for details on its syntax.

It is especially useful in the context of package upgrades. In this case

- `their` is the current version of the maintainer's copy of a configuration file,
- `base` is the previous version of the maintainer's copy of the configuration file.
- `our` is the user's copy of the configuration file, derived from `base`

First of all, we create a small example configuration file.
To do so, we first create a temporary file and store its location in Elektra.

```sh
kdb set user:/tests/tempfiles/firstFile $(mktemp)
echo -e "keyA=a\nkeyB=b\nkeyC=c" > `kdb get user:/tests/tempfiles/firstFile`
```

The following call to `kdb install-config-file` will mount it at `system:/tests/installing` and additionally create a copy of it.
The copy will be required for a three-way merge later on.

```sh
kdb install-config-file system:/tests/installing `kdb get user:/tests/tempfiles/firstFile` mini
```

We can now safely make changes to our configuration.

```sh
kdb set system:/tests/installing/keyB X
```

Let's assume that we've downloaded a different version of this file from the internet and placed it into the directory `/tmp/new`.
At this point we can use `kdb install-config-file` to merge the changes of this downloaded version into the configuration that we currently store in Elektra.

Note the slash `/` in the beginning of the second parameter of the call to `kdb install-config-file`.
This tool uses `kdb mount` and in consequence also the resolver.
You have to enter the paths accordingly.
Read the tutorials on mounting and namespaces if you are not sure what this means.

```sh
kdb set user:/tests/tempfiles/secondFile $(echo $(mktemp -d)/$(basename $(kdb get user:/tests/tempfiles/firstFile)))
echo -e "keyA=a\nkeyB=b\nkeyC=Y" > `kdb get user:/tests/tempfiles/secondFile`
kdb install-config-file system:/tests/installing $(kdb get user:/tests/tempfiles/secondFile) mini
```

We can check that this worked by calling

```sh
kdb get system:/tests/installing/keyB
#> X
kdb get system:/tests/installing/keyC
#> Y
```

Finally, we use the following commands to clean up our tutorial.

```sh
kdb umount system:/tests/installing
rm -rf $(kdb get user:/tests/tempfiles/firstFile)
rm -rf $(kdb get user:/tests/tempfiles/secondFile)
kdb rm -rf user:/tests/tempfiles
kdb rm -rf user:/elektra/merge/preserve
```
