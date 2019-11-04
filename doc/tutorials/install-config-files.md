# How-To: install configuration files

The `install-config-file` tool makes using Elektra for a configuration file easier.
Please refer to its man page for details on its syntax.

First of all, we create a small example configuration file.

```sh
mkdir ~/.config
echo -e "keyA=a\nkeyB=b\nkeyC=c" > ~/.config/installing.ini
```

The following call to `kdb install-config-file` will mount it at `user/tests/installing` and additionally create a copy of it.
The copy will be required for a three-way merge later on.

```sh
kdb install-config-file user/tests/installing installing.ini ini
```

We can now safely make changes to our configuration.

```sh
kdb set user/tests/installing/keyB X
```

Let's assume that we've downloaded a different version of this file from the internet and placed it into our home directory.
At this point we can use `kdb install-config-file` to merge the changes of this downloaded version into the configuration that we currently store in Elektra.

Note the slash `/` in the beginning of the second parameter of the call to `kdb install-config-file`.
This tool uses `kdb mount` and in consequence also the resolver.
You have to enter the paths accordingly.
Read the tutorials on mounting and namespaces if you are not sure what this means.

```sh
echo -e "keyA=a\nkeyB=b\nkeyC=Y" > ~/installing.ini
kdb install-config-file user/tests/installing /installing.ini ini
```

We can check that this worked by calling

```sh
kdb get user/tests/installing/keyB
#> X
kdb get user/tests/installing/keyC
#> Y
```

Finally, we use the following commands to clean up our tutorial.

```sh
kdb umount user/tests/installing
kdb rm -rf user/elektra/merge/preserve
rm ~/installing.ini
rm ~/.config/installing.ini
rmdir ~/.config
```
