# Understanding Error Messages

If you want to use Elektra for any of your projects you will sooner or later run into errors. These can happen
quite easily such as trying to mount with insufficient permissions, missing plugin configurations or when you want to write
specifications for your configurations and test it out.

Here is an example how you can trigger an error for an invalid IP entry in the `hosts` file:

```sh
sudo kdb mount --with-recommends /etc/hosts system/hosts hosts
sudo kdb set system/hosts/ipv4/my 88.198.134.1777
# RET: 5
# Sorry, module network issued the error C03200:
# Validation Semantic: name: system/hosts/ipv4/my value: 88.198.134.1777 message: Name or service not known

#cleanup
sudo kdb umount system/hosts
```

Note that you can always pass the **command line arguments -v (verbose) and -d (debug)** to the command line to get
extra information. Especially if you want to locate the configuration file which caused the error.

You can see that every message comes with an error code which is `C03200` in the upper example which is a semantic validation error.
Since v0.9 Elektra has hierarchically structured error codes that are leaned on to [SQL States](https://en.wikipedia.org/wiki/SQLSTATE).
Some categories of Elektra are `parent categories` of more specific error categories. E.g., is the
semantic validation error a concrete subcategory of `Validation errors` (`C03XXX`) that indicate that a certain requirement is not fulfilled
by the provided value.
You can see all error categories along with its meanings in our [categorization guideline](../dev/error-categorization.md).
Every category comes with its own solution approach which is intended to be as generic as possible.

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
