# elektra-bootstrapping(7) -- default backend

One important aspect of a configuration library is the out-of-the-box
experience. How does the system work before anything is configured?
The optimal situation is that everything fully works, and applications,
that just want to load and store configuration, do not see any difference
between out-of-the-box behavior and a well-configured, fine-tuned system.

To support that experience, a so-called **default backend** is
responsible in the case that nothing was configured so far. It must
have a storage that is able to store full Elektra semantics. To avoid reimplementation of storage plugins, a default storage plugin (`storage` or in code `KDB_STORAGE`) is used. A resolver plugin (`resolver`
or in code `KDB_RESOLVER`) takes care of the inevitable portability issues.
The **default backend** stores configuration in `KDB_DB_FILE`. One can easily avoid the
usage of the default backend by simply mounting another backend to `/`.

The mounting configuration (the configuration how to mount the
mount points) also needs to be stored somewhere.
The so-called **init backend** is responsible for fetching configuration
from `system:/elektra`, where the mount points are stored.
Again `KDB_STORAGE` and `KDB_RESOLVER` is used, but now
they write into the configuration file `KDB_DB_INIT` (`elektra.ecf` by default).

Thus for full and static build variants an exchange at run-time is not possible.
Using shared libraries, however, `KDB_STORAGE` and `KDB_RESOLVER` are actually
symbolic links (`libelektra-plugin-resolver.so` and `libelektra-plugin-storage.so`) to concrete plugins
and thus can be changed without recompilation.

The **init backend** is guaranteed to stay mounted at
`system:/elektra` where the configuration for Elektra
itself is stored. After mounting all backends, Elektra checks if
`system:/elektra` still resides at the default backend. If not,
the init backend will be mounted there.

## SUMMARY

To summarize, this approach delivers a good out-of-the-box experience
capable of storing configuration. For special use cases, applications
and administrators can mount specific backends anywhere except at, and
below, `system:/elektra`. On `kdbOpen()`, the system
bootstraps itself starting with the init backend.

The default backend consists of a default storage plugin and default
resolver plugin. The default resolver has no specific requirements, but
the default storage plugin must be able to handle full Elektra semantics.
The backend is mounted to root (`/`), so any keys can be
stored in it. The implementation of the core guarantees that user and
system keys always stay separated.

## TRACEABILITY

<!-- FIXME: functions changed -->

- `elektraOpenBootstrap()` implements above algorithm
- `backendOpenDefault()` opens the default backend
- `/src/include/kdbconfig.h.in` contains above `KDB_*` variables
- `src/plugins/CMakeLists.txt` creates the symbolic links
- `scripts/cmake/Modules/LibAddMacros.cmake` `create_lib_symlink` function

## SEE ALSO

- [bootstrap decision](../decisions/6_implemented/bootstrap.md)
