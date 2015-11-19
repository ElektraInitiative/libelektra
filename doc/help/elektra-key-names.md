elektra-key-names(7) -- the names of keys
=========================================

Every `Key` object with the same name will receive the very same
information from the global key database.  The name locates a **unique
key** in the key database.  Key names are always absolute; so no parent
or other information is needed. That makes a `Key` self-contained and
independent both in memory and storage.

Every key name starts with a [namespace](elektra-namespaces.md), for
example `user` or `system`.  These prefixes spawn key hierarchies each.

The shared *system configuration* is identical for every user.
It contains, for example, information about system daemons, network
related preferences and default settings for software.  These keys are
created when software is installed, and removed when software is purged.
Only the administrator can change system configuration.

Examples of valid system key names:

	system
	system/hosts/hostname
	system/sw/apache/httpd/num_processes
	system/sw/apps/abc/current/default-setting

user configuration is empty until the user changes some preferences.
User configuration affects only a single user.  The user's settings can
contain information about the user's environment, preferred applications
and anything not useful for the rest of the system.

Examples of valid user key names:

	user
	user/env/#1/LD_LIBRARY_PATH
	user/sw/apps/abc/current/default-setting
	user/sw/kde/kicker/preferred_applications/#1

The slash (`/`) separates key names and structures them hierarchically.
If two keys start with the same key names, but one key name continues
after a slash, this key is **below** the other and is called a
*subkey*.  For example `user/sw/apps/abc/current` is a subkey of the
key `user/sw/apps`.  The key is not directly below but, for example,
`user/sw/apps/abc` is.  `keyRel()` implements a way to decide the relation
between two keys.

