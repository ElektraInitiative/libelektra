# Goals

The goal of Elektra is to make it trivial to access and specify configuration settings by an API.
This helps in achieving the following goals:

- Improve robustness of configuration systems by
  - Avoiding common programming errors through the usage of the API.
  - Getting more guarantees when accessing configuration settings.
  - Rejecting invalid configurations.
  - Avoiding reimplementation of parsers for the same configuration file format.
- Allow software to be better integrated via configuration settings
  (e.g. via [Freedesktop](https://www.freedesktop.org)).
- Postpone some decisions from programmers to maintainers/administrators:
  - Rejecting unwanted configuration settings.
  - Uniformity of configuration access (logging, vcs commit, notifications).
  - Syntax of the configuration files (with limitations, see below).

Elektra follows the goals below, in order of preference.
If goals conflict, the higher goal takes precedence.

## 0. Stability

People need to be able to rely on the API/ABI to be stable.
They expect their configuration settings to continue working,
with minimal maintenance burden. In particular following parts
must be immutable:

- The API: Within a major release, the core API can only be extended.
  Every application that compiled with Elektra `x.0.0` must still
  compile with any `x.y.z`.
  Even with new major releases, only small adoptions in the source
  of applications or plugins might be needed.
- The ABI: Even across major releases, the core ABI must stay compatible.
  Every application that links with Elektra `x.0.0` will continue to link
  with any future version of Elektra.
  We use [symbol versioning](/doc/dev/symbol-versioning.md) for that goal.
- Key database and key names: Applications can rely on that whatever they once
  wrote into the key database, they will continue to get identical key names
  and values also with later versions of Elektra.
  Future extensions of the key database (e.g. new plugins) will not interfere.

## 1. Goal: Simplicity

Elektra is based on key-value pairs, the simplest form of what could
be called a database. Elektra's key value pair uniformity allows
with a single concept configuration settings and configuration
specifications to be written and to be introspected.

An overly complex system cannot be managed nor understood.
Extensibility brings some complex issues,
which need to be solved -- but in a way so that the user
sees either nothing of it or only needs to understand very
simple concepts.
Special care for simplicity is taken for the users:

- Endusers when reconfiguring or upgrading
  should never take any notice of Elektra, except that
  it works more robust and is better integrated.
- Programmers should have multiple ways to take advantage of
  Elektra so that it flawlessly integrate with their system.
- Plugin Programmers: it should be simple to extend Elektra
  in any desired way.

## 2. Goal: Robustness

Configuration systems today suffer badly from:

- Different behavior on different systems.
- Missing or weak validation.
- Faulty transformations from strings to concrete types.
- No or misleading error messages.
- Undefined behavior.
- Not working migrations from one version to another.

We tackle this problem by introducing an abstraction layer where
these problems are dealt with. The goal is that for improvements in these areas only
code changes within Elektra are needed (and not within applications using
Elektra). This makes the application's code not only portable towards more systems,
but also enables global improvements in configuration systems.

## 3. Goal: Extensibility

There are many variants of

- Storage formats.
- Frontend integrations.
- Bindings.

Nearly every aspect of Elektra must be extremely extensible.
On the other side semantics must be very clear and well-defined
so that this extensible system works reproducible and predictable.

Only key-value pairs are the common factor and a way to get and set
them, everything else is an extension.

## 4. Goal: Performance

Accessing configuration settings has impact on bootup and startup-time of applications.
Elektra needs to have similar performance as current solutions.
Of particular importance is that `kdbGet` invocations:

- are fast
- have low memory usage

The plugin system has as guideline:

> Only pay for what you need.

# Users

These goals are about Elektra's users, again in order of importance.
Again, lower goals need to be ignored if goals are in conflict.

## 1. Application Developers

Elektra must be easy and robust for application developers to store any
configuration settings referable by keys they need to store. After writing
configuration settings (`kdbSet`) and reading them again (`kdbGet`)
they get the same KeySet (aka "round-trip").

> This means, they must be able to store keys with any name, any string or any binary data
> as needed for their purpose.

## 2. Administrators

Administrators should be empowered by good error messages and validation capabilities.
Furthermore, they should be able to use their favorite tools and configuration file formats.

> There are principal limitations of nearly all configuration file formats, so Elektra cannot
> enable that any configuration file format can be used with any application.
> If maintainers or administrators want to change the configuration file format of some application,
> they need to carefully test if it works.

## 3. Maintainers

Elektra must be available everywhere and flexible enough, so that maintainers can integrate
different applications by specifying and mounting.

> There might be some restrictions that some applications require some plugins to be mounted
> for their configuration settings.

## 4. Possibility to Represent any Configuration File Format

Elektra must be powerful and flexible enough to be able to represent any configuration file
format. We support the development of fully-conforming parsers and emitters.

> This means, that given a correctly written storage plugin, a KeySet can be found
> that represents the configuration settings, its metadata and the hierarchical structure of
> the configuration file.

# Non-Goals:

- Support semantics that do not fit into the KeySet (key-value pairs) with an `kdbGet()`/`kdbSet()` interface.
- Support for non-configuration issues, e.g., storing key-value data unrelated to configuration settings.
- Elektra is not a distributed configuration management tool:
  use your favorite configuration management tool on top or a distributed file system below Elektra.

## Further Readings

- Continue reading: [Who uses Elektra?](WHO.md)
