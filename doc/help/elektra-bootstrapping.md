elektra-bootstrapping(7) -- default backend
===========================================

One important aspect of a configuration library is the out-of-the-box
experience. How does the system work before anything is configured?
The optimal situation is that everything works fully, and applications,
that just want to load and store configuration, do not see any difference
in a well-configured, fine-tuned system.

To support that experience, a so-called **default backend** is
responsible in the case that nothing was configured so far.  It must
have a storage that is able to store full Elektra semantics including
every type and arbitrary metadata. To avoid reimplementation of storage
plugins, for default storage plugins a resolver plugin additionally
takes care of the inevitable portability issues.

The default backend is guaranteed to stay mounted at
`system/elektra` where the configuration for Elektra
itself is stored.  After mounting all backends, Elektra checks if
`system/elektra` still resides at the default backend.  If not,
it will be mounted there.

To summarise, this approach delivers a good out-of-the-box experience
capable of storing configuration. For special use cases, applications
and administrators can mount specific backends anywhere except at, and
below, `system/elektra`.  On `kdbOpen()`, the system
bootstraps itself starting with the default
backend.

The default backend consists of a default storage plugin and default
resolver plugin.  The default resolver has no specific requirements, but
the default storage plugin must be able to handle full Elektra semantics.
The backend is not mounted anywhere in particular, so any keys can be
stored in it. The implementation of the core guarantees that user and
system keys always stay separated.
