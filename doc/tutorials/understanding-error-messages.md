# Understanding Error Messages

Errors can happen when trying to mount with insufficient permissions, missing plugin configurations or when you want to write
specifications for your configurations and test it out or many other occasions. The error message gives information on what the problem is and possibly how
to resolve them.

Here is an example how you can trigger an error for an invalid IP entry in the `hosts` file:

```sh
sudo kdb mount --with-recommends /etc/hosts system/tests/hosts hosts
sudo kdb set system/tests/hosts/ipv4/my 88.198.134.1777
# RET: 5
# Sorry, module network issued the error C03200:
# Validation Semantic: name: system/tests/hosts/ipv4/my value: 88.198.134.1777 message: Name or service not known

#cleanup
sudo kdb umount system/tests/hosts
```

Note that you can always pass the **command line arguments -v (verbose) and -d (debug)** to the command line to get
extra information. Especially if you want to locate the configuration file which caused the error.

You can see that every message comes with an error code which is `C03200` in the upper example which is a semantic validation error.
Since v0.9 Elektra has hierarchically structured error codes that are leaned on to [SQL States](https://en.wikipedia.org/wiki/SQLSTATE).
Some categories of Elektra are `parent categories` of more specific error categories. E.g., is the
semantic validation error a concrete subcategory of `Validation errors` (`C03XXX`) that indicate that a certain requirement is not fulfilled
by the provided value.
You can see all error categories along with its meanings in our [categorization guideline](../dev/error-categorization.md).
Every category comes with its own special method of handling the errors.

1. Permanent Errors `C01XXX`

   The problem most likely lies on your system or Elektra has a bug. Investigate the concrete error message
   which indicates the source of failure (Permissions, Memory, Bug, etc.)

2. Conflict `C02XXX`

   It seems that you have changed the mounted file manually. Elektra needs to synchronize with the underlying file
   so try to synchronize your internal state and retry to get rid of this error. You can see the actual file
   by passing the `-v` parameter to the command line. These errors may also happen if you use one of our bindings
   and do not call `kdbGet` before `kdbSet` since Elektra needs to know the state before it can add/modifiy/delete
   existing keys.

3. Validation `C03XXX`

   Validation errors are heavily used for Elektra's `configuration specification`
   feature and should tell users that their given input does not match a certain
   pattern/type/expected semantic. Retry but with a conforming configuration value.

## Error message format

Error messages which are emitted from Elektra have the same message format for all errors
(See [design decision](../decisions/error_message_format.md)).

The design looks like:

```
Sorry, module `MODULE` issued [error|warning] `NR`:
`ERROR_CODE_DESCRIPTION`: Validation of key "<key>" with string "<value>" failed.
```

If you want additional information about the mountpoint or the configfile which
were affected by the error you can add the verbose argument `-v`:

```
Mountpoint: system/tests/hosts
Configfile: /etc/hosts.13163:1575216150.11522.tmp
```

If you need more information you can pass the debug argument `-d` to the command which also shows at which line in the source code
the error was emitted:

```
At: <path>/src/plugins/network/network.c:184
```

Error messages from tools (see [kdb.md](/doc/help/kdb.md)) may yield different messages such as when providing false command line arguments
or mounting without incorrect permissions.

## Error handling for developers

If you want to contribute to Elektra or write your own specific plugin you can use the integrated error macros which
eases the error handling. In general the [plugins README](plugins.md) is a good way to start you journey through writing your own plugin.
All macros can be used by including `kdberrors.h` into the source file.
For more information about the macros please read through `elektra_set_concrete_type_error` section in the [plugins.md](plugins.md#elektra_set_concrete_type_error) tutorial.
Internally when Elektra emits an error it sets a [metadata](/doc/dev/metadata.md) to a Key which then is later on parsed into the error message.
The key name of the error key is the mountpoint of the configuration. If an error occurs, 7 metadata are set which can be seen in our
[error-handling tutorial](../dev/error-handling.md). Later on those metadata are passed into a message template which renders the message
as seen above. The same goes for warnings except that there can be up to 100 warnings but only a single error.

### Catching errors in language bindings

Elektra provides many useful language bindings which allows you to code for Elektra in your favorite programming language.
Since errors are built up hierarchically you can catch either general errors (e.g., all Permanent Errors) or a very specific one (e.g., Resource Error)
and react appropriate. For Java for example you can catch errors in the following form:

```java
		try (final KDB kdb = KDB.open (parentKey))
		{
			final KeySet ks = KeySet.create (10, KeySet.KS_END);
            kdb.get (ks, parentKey);
		}
		catch (PermanentException e)
		{
			// Your handling code
		}
```
