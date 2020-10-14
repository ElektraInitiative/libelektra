# Goals

The goal of Elektra is to make it trivial to access and specify configuration by an API.
This helps in achieving the following goals:

- Improve robustness of configuration systems by
  - Avoiding common programming errors through the usage of the API.
  - Getting more guarantees when accessing configuration.
  - Rejecting invalid configurations.
  - Avoiding reimplementation of parsers for the same configuration file format.
- Allow software to be better integrated and standardized on configuration level
  (e.g. via [Freedesktop](https://www.freedesktop.org)).
- Postpone some decisions from programmers to maintainers/administrators:
  - Rejecting unwanted configuration.
  - Uniformity of configuration access (logging, vcs commit, notifications).
  - Syntax of the configuration files (with limitations, see below).

Elektra follows following goals, in order of preference.

## 1. Goal: Simplicity

An overly complex system cannot be managed nor understood.
Extensibility brings some complex issues,
which need to be solved -- but in a way so that the user
sees either nothing of it or only needs to understand very
simple concepts so that it works flawlessly.
Special care for simplicity is taken for the users:

- Endusers when reconfiguring or upgrading
  should never take any notice of Elektra, except that
  it works more robust, better integrated and with less problems.
- Programmers should have multiple ways to take advantage of
  Elektra so that it flawlessly integrate with their system.
- Plugin Programmers: it should be simple to extend Elektra
  in any desired way.
- It should be easy for application's maintainers to correctly
  setup and upgrade KDB.
- Key-value uniformity that allows introspection.

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
code changes within Elektra (and not within applications using
Elektra) are needed. This makes your code not only portable towards more systems,
but also enables global improvements in the configuration systems.

## 3. Goal: Extensibility

There are many variants of

- Storage formats.
- Frontend integrations.
- Bindings.

Nearly every aspect of Elektra must be extremely extensible.
On the other side semantics must be very clear and well defined
so that this extensible system works reproducible and predictable.

Only key-value pairs are the common factor and a way to get and set
them, everything else is an extension.

## 4. Goal: Performance

Accessing configuration has impact on bootup and startup-time.
Elektra needs to be similar fast then current solutions.
Ideally it should get faster because of centralized optimization
endeavours where everyone using Elektra can benefit from.

> Only pay for what you need.


# Domain-Specific Goals

These goals are specific to the Elektra's domain, again in order of importance.

## Suitability for Applications

Elektra must be easy and robust for application (developers) to store any configuration they
need to store.

> This means, they must be able to store keys with any name, any string or any binary data
> as needed for their purpose.

## Possibility to Represent any Configuration File Format

Elektra must be powerful and flexible enough to be able to represent any configuration file
format.

> This means, that given a correctly written storage plugin, a KeySet can be found
> that represents the content of the configuration file.

## Flexibility of Administrators

Administrators should be empowered to use their favourite tools and configuration file formats.

> Note:
> There are principal limitations of nearly all configuration file formats, so Elektra cannot
> enable that any configuration file format can be used with any application.
> If administrators or maintainers want to change the configuration file format of some application,
> they need to carefully test if it works.

# Non-Goals:

- Support semantics that do not fit into the KeySet (key-value pairs) with an `kdbGet()`/`kdbSet()` interface.
- Support for non-configuration issues, e.g., storing key-value data unrelated to configuration settings.
- Elektra is not a distributed configuration management tool:
  use your favourite configuration management tool on top or a distributed file system below Elektra.

## Further Readings

- Continue reading: [Who uses Elektra?](WHO.md)
