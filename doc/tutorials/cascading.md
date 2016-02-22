## Order of namespaces

This tutorial assumes that you know what [namespaces](/doc/tutorials/namespaces.md) are. We will only be talking about [cascading lookup](/doc/help/elektra-cascading.md) here.

In Elektra, the default order of namespaces is as follows:

 * [spec](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#spec) (contains metadata, e.g. to modify elektra lookup behaviour)
 * [proc](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#proc) (process-related information)
 * [dir](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#dir) (directory-related information, e.g. `.git` or `.htaccess`)
 * [user](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#user) (user configuration)
 * [system](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#system) (system configuration)

Looking at this order, we can see that if a configuration option isn't specified
by the user (then it would be in the `user` namespace), it will be loaded from
the `system` namespace. In this case, the option in the `system` namespace will
be used if the key hasn't been defined by the user.

```
$ sudo kdb set system/t/test "hello world"
create a new key system/t/test with string hello world

$ kdb get /t/test
hello world

$ kdb set user/t/test "hello universe"
Create a new key user/t/test with string hello universe

$ kdb get /t/test
hello universe
```

Furthermore, in the order `dir` is even higher than `user`, which means that
configuration in the current folder can overwrite user configuration.

`.configuration` in your current directory:

```
test = hello dir
```

Then run:

```
$ sudo kdb mount /.configuration dir/t ini
$ kdb get /t/test
hello dir
```


## Cascading

Cascading triggers actions when, for example, the key isn't found.
This concept is used for our previous example of using `system` configuration
when the `user` configuration is not defined. When a key starts with `/`,
*cascading lookup* will automatically be performed. e.g. using `/test` instead
of `system/test` will do a cascading lookup.


## Override links

The `spec` namespace is special as it can completely change how the cascading
lookup works.

For example, in the meta data of the respective `spec`-keys, *override links*
can be specified to use other keys in favor of the key itself. This way, even
config from current folders (`dir`) can be overwritten.

In the cascading lookup, meta data of `spec`-keys comes in as follows:

 1. `override/#` keys will be considered
 2. namespaces specified in the `namespaces/#` keys are considered
 3. Otherwise, all namespaces will be considered, see [here](/doc/help/elektra-namespaces.md).
 4. `fallback/#` keys will be considered
 5. `default` value will be returned

Note: `override/#` means an array of `override` keys, the array can be filled by
      setting `#` followed by the position, e.g. `#0`, `#1`, etc

As you can see, override links are considered before everything else, which
makes them really powerful.

To create an override link, first you need to create a key to link the override
to:

```
$ sudo kdb set system/overrides/test "hello override"
Create a new key system/overrides/test with string hello override
```

Override links can be defined by adding them to the `override/#` array:

```
$ sudo kdb setmeta spec/t/test override/#0 /overrides/test
$ kdb get /test
hello system override
```

Furthermore, you can specify a custom order for the namespaces, set fallback
keys and more. For more information, read the [`elektra-spec` help page](/doc/help/elektra-spec.md).


## User defaults

Override links can also be used to define default values. It's similar to
defining default values via the `system` namespace, but uses overrides, which
means it will be preferred over the configuration in the current folder (`dir`).

This means that user defaults overwrite values specified in the `.configuration`
file we created and mounted earlier in this tutorial.

First you need to create the system default value to link the override to if the
user hasn't defined it:

```
$ sudo kdb set system/overrides/test "hello default"
Create a new key system/overrides/test with string hello default
```

Then we can create the link:

```
$ sudo kdb setmeta spec/t/test override/#0 /overrides/test
$ kdb get /t/test
hello default
```

Now the user overrides the system default:

```
$ kdb set /overrides/test "hello user"
Using name user/overrides/test
Create a new key user/overrides/test with string hello user
$ kdb get /t/test
hello user
```
