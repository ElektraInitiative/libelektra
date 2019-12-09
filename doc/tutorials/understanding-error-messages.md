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

In this example we mount the hosts file under `/etc/hosts` into Elektra. Elektra also validates
our input which is shown in the next line where we tried to set an invalid IP address. With this we can add or edit entries
by using the tools of Elektra and do not need to edit the file directly.

Note that you can always pass the **command line arguments -v (verbose) and -d (debug)** to the command line to get
extra information. Especially if you want to locate the configuration file which caused the error. Providing both parameter would
yield an additional message which looks as following on my system:

```
sudo kdb set -vd system/tests/hosts/ipv4/my 88.198.134.1777
# RET: 5
# Sorry, module network issued the error C03200:
# Validation Semantic: name: system/tests/hosts/ipv4/my value: 88.198.134.1777 message: Name or service not known
# Mountpoint: system/tests/hosts
# Configfile: /etc/hosts.13163:1575216150.11522.tmp
# At: <path>/src/plugins/network/network.c:184
```

You can see that every message comes with an error code which is `C03200` in the upper example which is a semantic validation error.
Since v0.9 Elektra has hierarchically structured error codes that are leaned on to [SQL States](https://en.wikipedia.org/wiki/SQLSTATE)
because it is easily extensible without introducing major breaking releases. Each character has a special meaning.
Lets take the semantic validation error `C03200` as example:

- `C` indicates `C`ode and should distinguish error codes from normal numbers
- `03` indicates the major parent category (will be explained shortly). Two numbers were taken in case we want more than 10 major error categories in the future
- `2` indicates a subcategory. `1` for example would be syntactic validation errors which differs in the meaning
- `0` and `0` could be used to introduce even further subcategorizations which are under semantic validation errors

Before Elektra v0.9 we had around 210 categories which were reduced to a total of 12. All macros as well as error codes can be seen in
[errors.c](/src/libs/elektra/errors.c) where they are generated via macro preprocessing.
You can see all error categories along with its meanings in our [categorization guideline](../dev/error-categorization.md).
The guideline is important for both developers who want to contribute to Elektra as well as users who struggle with solving certain errors.
Every category comes with its own special method of handling the errors. In the upper example we have a validation error which indicates
that we should retry with a different value which fulfills the semantics of an IP. If we set a correct IP address, Elektra will save it to
the `hosts` file:

```
sudo kdb set system/tests/hosts/ipv4/my 88.198.134.177
Create a new key system/tests/hosts/ipv4/my with string "88.198.134.177"
```

We thought of three major error categories in which every error solution approach fits. The more concrete the category gets,
the more concrete you know which solution approach you will have to take. E.g., whereas the category `Permanent Errors` tells you
that the problem lies on your system, the subcategory `Resource Errors` tells you that there are no enough permissions for a certain action
or the resource does not exist. Elektra enforces developers to use the most specific category as possible to help users find the appropriate
solution. Some categories of Elektra are `parent categories` of more specific error categories. E.g., is the
semantic validation error a concrete subcategory of `Validation errors` (`C03XXX`) that indicate that a certain requirement is not fulfilled
by the provided value. As a result some error categories are not available for putting in errors. We want to give a rough introduction
to the three major error categories but for more detail we encourage you to look into our [categorization guideline](../dev/error-categorization.md).

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

Elektra's error message also includes the information about the `module` which emits the error indicated by the first line:
`` Sorry, module `MODULE` issued [error|warning] `NR`: ``. The module indicates either indicates the core of elektra (`kdb`) or can be
a concrete plugin. All plugins have to have a name and this is guaranteed by the compiler. Before Elektra v9.0 we also
had even more fields to display such as `ingroup` that indicated which group emitted the error (e.g., plugins). In the effort to keep the
message as succinct as possible we decided to [remove this field](/doc/decisions/ingroup_removal.md).

## Error handling for developers

If you want to contribute to Elektra or write your own specific plugin you can use the integrated error macros which
eases the error handling. In general the [plugins README](plugins.md) is a good way to start you journey through writing your own plugin.
All macros can be used by including `kdberrors.h` into the source file.
For more information about the macros please read through `elektra_set_concrete_type_error` section in the [plugins.md](plugins.md) tutorial.
Internally when Elektra emits an error it sets a [metadata](/doc/dev/metadata.md) to a Key which then is later on parsed into the error message.
The key name of the error key is the mountpoint of the configuration. If an error occurs, 7 metadata are set which can be seen in our
[error-handling tutorial](../dev/error-handling.md). Later on those metadata are passed into a message template which renders the message
as seen above. The same goes for warnings except that there can be up to 100 warnings but only a single error.

One of the major painpoints we had during refactoring of all error messages came from inconsistent messages. We encourage
developers to read through our [error-message guidelines](/doc/dev/error-message.md). For out of memory errors we even enforced
a consistent error message by altering the macro which is used for emitting errors. It does not contain a text field anymore because
these messages should be identical in all cases. Each error message category comes with associated information which should be present.
Validation errors for example should require the key and value in every message but is not "templated" yet. If you cannot find good possibility
to add this information meaningful into the message you will most likely want to put an error into the wrong category or there is a need
for a new category (e.g., if multiple keys are affected one might introduce a "structural validation" which accepts these). In the latter
case please forge a [design decision](/doc/decisions/README.md) and open a pull request for discussion.

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
