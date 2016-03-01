# Version #

The version of elektra is handeled with the kdb.h macros
KDB_VERSION which is a string and KDB_VERSION_MAJOR,
KDB_VERSION_MINOR and KDB_VERSION_MICRO which are
numbers. They represent the public announced version
information.

The same information can be retrieved at runtime using
	system/elektra/version/constants/KDB_VERSION
	system/elektra/version/constants/KDB_VERSION_MAJOR
	system/elektra/version/constants/KDB_VERSION_MICRO
	system/elektra/version/constants/KDB_VERSION_MINOR


KDB_VERSION

This is the API to programs using elektra. Its interface
is defined in src/include/kdb.h.
Both applications and plugins use this API.

Additionally there is also a very small API
to plugins. It consists of only 5 functions
and is described in src/plugins/doc/doc.c.


## Compatibility ##

This document describes under which circumstances API
and ABI incompatiblities may occur. As developer from
elektra your mission is to avoid that.
The tool icheck against the interfaces mentioned
above may help you too.

In 0.8.* the API and ABI must be always forward-compatible,
but not backwards-compatible.
That means that a program written and compiled against 0.8.0
compiles and links against 0.8.1. But because it is
not necessarily backendwards-compatible a program written
for 0.8.1 may not link or compile against elektra 0.8.0
(but it may do when you use the compatible subset, maybe
with #ifdefs).

Following points are allowed:
When you add a new function you break ABI and API backward-
compatibility, but not forward, so you are allowed to do so.

In the signature you are only allowed to add const to
any parameter. You are *not* allowed to use subtypes to
the objects, in C means you are not allowed to call any
functions of an object which appear new. C does *not*
typecheck that, it's your responsibility.

What C also does not check are the pre and postconditions.
That means you are not allowed to demand more client code
(e.g. first accept a NULL pointer and in the next version
 you crash on it) and you are not allowed to return
values that the previous version did not return. It is
a complex topic, so better don't underestimate it, but
generally said the methods should behave on the same data
the same way.

References:
http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html
http://packages.debian.org/de/sid/icheck


## Increment ##

This document describes how to increment the KDB_VERSION.
KDB_VERSION consists of a triplet integer current:revision:age.

Revision is something which will always incremented when there
is a new bugfix release.

current and age will be incremented by one when you release
a compatible but changed API. Revision is set back to zero then.

Note: All 3 Versioning infos are handled separately!

http://www.gnu.org/software/libtool/manual/libtool.html#Versioning


TODO write about SO_VERSION

