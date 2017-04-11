# Cascading Lookups

This tutorial assumes that you are already familiar with [namespaces](/doc/tutorials/namespaces.md). This tutorial will only explain [cascading lookup](/doc/help/elektra-cascading.md).

When Elektra looks up a _cascading key_ (i.e. key names without a namespace and a leading slash `/`, the namespaces are searched in the following order:

 * [spec](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#spec) (contains metadata, e.g. to modify elektra lookup behaviour)
 * [proc](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#proc) (process-related information)
 * [dir](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#dir) (directory-related information, e.g. `.git` or `.htaccess`)
 * [user](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#user) (user configuration)
 * [system](https://github.com/ElektraInitiative/libelektra/blob/master/doc/help/elektra-namespaces.md#system) (system configuration)

If a key, for example, exists in both **user** and **system** namespace, the key in the **user** namespace takes precedence over the one in the **system** namespace. If there is no such key in the **user** namespace the key in the **system** namespace acts as a fallback.

But lets demonstrate this with an example:

### Add a Key to the system Namespace

Configuration in the **system** namespace is the same for all users. Therefore this namespace provides a default or fallback configuration.

With the default Elektra installation only an administrator can update configuration settings within the **system** namespace.

```sh
# Backup-and-Restore:/sw/tutorial

kdb get /sw/tutorial/cascading/#0/current/test
# RET: 1
# STDERR:Did not find key

# Now add the key ...
sudo kdb set system/sw/tutorial/cascading/#0/current/test "hello world"
#> Create a new key system/sw/tutorial/cascading/#0/current/test with string hello world

# ... and verify that it exists
kdb get /sw/tutorial/cascading/#0/current/test
#> hello world
```

### Add a Key to the user Namespace

A user may now want to override the configuration in **system**, so he/she sets a key in the **user** namespace:

```sh
kdb set user/sw/tutorial/cascading/#0/current/test "hello galaxy"
#> Create a new key user/sw/tutorial/cascading/#0/current/test with string hello galaxy

# This key masks the key in the system namespace
kdb get /sw/tutorial/cascading/#0/current/test
#> hello galaxy
```

Note that configuration in the **user** namespace only affects _this_ user. Other users would still get the key from the **system** namespace.

### Add a Key to the dir Namespace

The **dir** namespace is associated with a directory. The configuration in the **dir** namespace applies to the **current working directory** and all its subdirectories.
This is useful if you have project specific settings (e.g. your git configuration or a .htaccess file).

As **dir** precedes the **user** namespace, configuration in **dir** can overwrite user configuration:

```sh
# create and change to a new directory ...
mkdir -p kdbtutorial
cd kdbtutorial

# ... and create a key in this directories dir-namespace
kdb set dir/sw/tutorial/cascading/#0/current/test "hello universe"
#> Create a new key dir/sw/tutorial/cascading/#0/current/test with string hello universe

# This key masks the key in the system namespace
kdb get /sw/tutorial/cascading/#0/current/test
#> hello universe

# But is only present in the associated directory
cd ..
kdb get /sw/tutorial/cascading/#0/current/test
# hello galaxy
```

### Add a Key to the proc Namespace

The **proc** namespace is not accessible by the command line tool **kdb**, as it is unique for each running process using Elektra. So we have to omit an example for this namespace at this point.
[Elektrified](/doc/help/elektra-glossary.md) applications can use this namespace to override configuration from other namespaces internally.

### Add a Key to the spec Namespace

The **spec** namespace is used to store metadata about keys and therefore Elektra handles the **spec** namespace differently to other namespaces. The following part of the tutorial is dedicated to the impact of the **spec** namespace on cascading lookups.


## Override Links

The `spec` namespace is special as it can completely change how the cascading
lookup works.

During a cascading lookup for a specific key, the default Elektra behaviour can be changed by a corresponding `spec-key`, i.e. a key in the **spec** namespace **with the same name**.

For example, the metadata `override/#0` of the respective `spec-key` 
can be specified to use a different key in favor of the key itself. This way, we can implement a redirect or symlink like behaviour and therefore even
config from current folder (`dir` namespace) can be overwritten.

The cascading lookup will consider the following **metadata keys** of `spec-key`:

 1. `override/#n` redirect to one of the specified keys
 2. `namespaces/#n` specifies which namespaces will be considered and in which order
 4. `fallback/#n` if no key was found these keys will act as a fallback
 5. `default` defines a default value for the key if none of the keys was found

**Note:** `override/#n`, `namespaces/#n` and `fallback/#n` are Elektra **array keys**. This means, such keys can exist several times, each with a different number for `n`, e.g. `override/#0`, `override/#1`... This way, we can define multiple values for a specific key with a defined order.

As you can see, override links are considered before everything else, which
makes them really powerful.

Consider the following example:

First, we create a target key to demostrate the override link mechanism:

```sh
sudo kdb set system/overrides/test "hello override"
#> Create a new key system/overrides/test with string hello override
```

Override links can be defined by adding them to the `override/#` metadata array key of the correspoding `spec-key`:

```sh
sudo kdb setmeta spec/sw/tutorial/cascading/#0/current/test override/#0 /overrides/test
```

Now when doing a cascading lookup, we get the value of our target key instead of the specified one:

```sh
kdb get /sw/tutorial/cascading/#0/current/test
#> hello override
```

As we used a cascading key for our override link (`/overrides/test`) we can use this to allow users to provide their own `overrides/test` keys. If a user sets the `/overrides/test` key, the **user** namespace will be used and therefore the new target for our `/sw/tutorial/cascading/#0/current/test` key will be `user/overrides/test` instead `system/overrides/test`.

```sh
kdb set /overrides/test "hello user"
#> Using name user/overrides/test
#> Create a new key user/overrides/test with string hello user
kdb get /sw/tutorial/cascading/#0/current/test
#> hello user
```

Furthermore, you can specify a custom order for the namespaces, set fallback
keys and more. For more information, read the [`elektra-spec` help page](/doc/help/elektra-spec.md).


## Cleanup

As last part in this tutorial we remove the modifications to the database we made previously.

```sh
kdb rm -r user/sw/tutorial/
kdb rm -r system/sw/tutorial
kdb rm -r system/overrides/test

kdb rm /overrides/test

kdb rm -r spec/sw/tutorial/
```
