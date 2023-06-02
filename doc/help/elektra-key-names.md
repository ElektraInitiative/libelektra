# elektra-key-names(7) -- the names of keys

Every `Key` object with the same name will receive the very same
information from the global key database. The name locates a
**unique key** in the key database. Key names are always absolute; so no parent
or other information is needed. That makes a `Key` self-contained and
independent both in memory and storage.

Every key name starts with a [namespace](elektra-namespaces.md), for
example `user` or `system`. These prefixes spawn key hierarchies each.

The shared _system configuration_ is identical for every user.
It contains, for example, information about system daemons, network
related preferences and default settings for software. These keys are
created when software is installed, and removed when software is purged.
Only the administrator can change system configuration.

Examples of valid system key names:

```
system:/
system:/hosts/hostname
system:/sw/apache/httpd/#0/current/num_processes
system:/sw/apps/abc/#0/current/default-setting
```

user configuration is empty until the user changes some preferences.
User configuration affects only a single user. The user's settings can
contain information about the user's environment, preferred applications
and anything not useful for the rest of the system.

Examples of valid user key names:

```
user:/
user:/env/#1/LD_LIBRARY_PATH
user:/sw/apps/abc/#0/current/default-setting
user:/sw/kde/kicker/#0/current/preferred_applications/#0
```

The slash (`/`) separates key names and structures them hierarchically.
If two keys start with the same key names, but one key name continues
after a slash, this key is **below** the other and is called a
_subkey_. For example `user:/sw/apps/abc/current` is a subkey of the
key `user:/sw/apps`. The key is not directly below but, for example,
`user:/sw/apps/abc` is. Various functions in `keytest` implement
ways to determine the relationship between two keys.

## Conventions

For computers Elektra would work without any conventions, because it is
possible to rename keys with plugins and link and transform any key-value
to any other key-value. Obviously, for humans such chaos would be confusing and
harder to use, thus we encourage everyone to use the following conventions:

### Arrays

If you want to denote an array, i.e. many unnamed subkeys, use the syntax
`#0`, ..., `#_10`. Then simple string comparisons will yield correct
results and the names are still very compact.

### Application Base Name

As decided [here](https://github.com/ElektraInitiative/libelektra/issues/302),
the key names of software-applications should always start with:

```
/sw/org/myapp/#0/current/name/full
```

- `sw` is for software, `hw` for hardware, `elektra` for internals
- `org` is a URL/organization name to avoid name clashes with other
  application names. Use only one part of the URL/organization,
  so e.g. `kde` is enough.
- `myapp` is the name of the most specific component that has its own
  configuration
- `#0` is the major version number of the configuration (to be incremented
  if you need to introduce incompatible changes).
  (Rationale: it is possible to start the old version of the app,
  using `/sw/org/myapp/#X`, where `X` refers to the previous version number.)
- `current` is the profile to be used. This is needed by administrators
  if they want to start up multiple applications with different
  configurations.

## Further Recommendations

- Avoid having your applications root right under @p system or @p user.
  (rationale: it would make the hierarchy too flat.)
  See **Application Base Name** above.
- Avoid the usage of characters other than `/`, a-z and 0-9.
  (rationale: it would allow too many similar, confusing names.)
  (exceptions: if the user or a technology, decide about parts of
  the key name, this restriction does not apply, e.g. if the wlan
  essid is used as part of the key name)
- The only way to separate names is using `/` (no A-Z, no `_`, no whitespaces)
  (rationale: there are many different opinions about this topic
  and having a choice which separator to choose will certainly lead
  to inconsistencies)
- It is suggested to make your application look for default keys under
  `/sw/org/myapp/#X/%/` where `X` is a major version number, e.g. `#3` for
  the 4th version and `%` is a profile (`%` for default profile). This way, from
  a sysadmin perspective, it will be possible to copy the
  `system:/sw/myapp/#3/%/` tree to something like
  `system:/sw/myapp/#3/old/` and keep system clean and organized.

## SEE ALSO

- [see application integration tutorial](/doc/tutorials/application-integration.md)
- [see namespaces tutorial](/doc/tutorials/namespaces.md)
- [key name source file](/src/libs/core/keyname.c) or [its rendered API documentation](https://doc.libelektra.org/api/master/html/group__keyname.html#details)

- [elektra-namespaces(7)](elektra-namespaces.md)
- [elektra-cascading(7)](elektra-cascading.md)
