# Goals

The vision of Elektra is to make it trivial to access and specify configuration by APIs.
This helps in achieving the following goals:

- Improve robustness of configuration systems by
  - avoiding reimplementation of parsers for the same configuration settings.
  - rejecting invalid configuration.
  - avoiding common programming errors through the usage of better bindings.
  - getting more guarantees when accessing configuration.
- Allow software to be better integrated on configuration level.
- Postpone some decisions from programmers to maintainers/administrators:
  - Syntax of the configuration file(s)
  - Side effects (e.g. logging, vcs commit, notifications)
  - Flexible adoption to specific needs
  - Adoption of standards (xdg, xml, JSON)

## Target

- Embedded: Elektra is on the frontier for embedded systems because of
  its tiny core and the many possibilities with its plugins.
  Known users:
  - OpenWRT (distribution)
  - Broadcom (blue-ray devices)
  - Kapsch (cameras)
  - Toshiba (TVs)
- Server: Elektra is ideal suited for a local configuration storage by
  mounting existing configuration files into the global tree. Nodes
  using Elektra can be connected by already existing configuration
  management tools.
  Known users:
  - Allianz
  - TU Wien
  - Other Universities
- Desktop: Elektra allows applications to read and write from a global
  configuration tree. We miss a specification (schema) so that these
  configuration values can be shared (integrated).

## Quality Goals

### 1. Simplicity

An overly complex system cannot be managed nor understood.
Extensibility brings some complex issues,
which need to be solved - but in a way so that the user
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
- Application's Maintainers to correctly setup and upgrade KDB
- Administrators that want to change the maintainers' setup
  according to their needs
- Key-value uniformity that allows introspection

### 2. Robustness

Configuration systems today suffer badly from:

- Different behavior on different systems
- Weak input validation
- Faulty transformations from strings to other types
- No error messages
- Undefined behavior
- Migration from one version to another

We want to tackle this problem by introducing an abstraction layer where
all these problems can be dealt with. The goal is that code changes
are necessary only within Elektra and not in the applications using
Elektra! This makes your code not only portable towards more systems,
but also enables global improvements in the configuration systems.

### 3. Extensibility

There are so many variants of

- Storage formats
- Frontend integrations
- Bindings

Nearly every aspect of Elektra must be extremely extensible.
On the other side semantics must be very clear and well defined
so that this extensible system works reproducible and predictable.

Only key-value pairs are the common factor and a way to get and set
them, everything else is an extension.

### 4. Performance

Accessing configuration has impact on bootup and startup-time.
Elektra needs to be similar fast then current solutions.
Ideally it should get faster because of centralized optimization
endeavours where everyone using Elektra can benefit from.

Only pay for what you need.

## Non-Goals

- Support semantics that do not fit into the KeySet (key-value pairs) with an `kdbGet()`/`kdbSet()` interface.
- Support for non-configuration issues, e.g., storing key-value data unrelated to configuration settings.
- Elektra is not a distributed CM, use Puppet, CFEngine on top or a distributed file system below Elektra.

## Further Readings

- Continue reading: [Why should I use Elektra?](doc/WHY.md)
