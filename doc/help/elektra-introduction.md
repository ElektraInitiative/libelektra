# elektra-introduction(7) -- an introduction to Elektra

**Elektra** is a library implementing access to a global key database.
The **global key database** provides access to all configuration
files found on a system. To elektrify an application means to change
the application so that it uses Elektra afterwards.

Information on Elektra can be found on the
[website](https://www.libelektra.org).
For introduction in the terminology, make sure to read
[the glossary](elektra-glossary.md).

# Motivation

## Why Elektra?

Configurations settings are hierarchical data
structures of keys, each consisting of a name and a value. They can be
used to configure software for the user's needs. Because these settings
stay the same across restarts of the program, they need to be stored
permanently. In the beginning this was done with primitive text files.
Possibilities to structure the text were added later.

Nearly every system developed its own way to read configuration settings.
Some got a de facto standard for a desktop environment (kconfig,
gconfig) or even an operating system (Windows Registry, Open Directory).
But they have a common problem: they are bound to the platform for which
they were developed.

That is where Elektra comes in to fill the gap. On the one hand,
Elektra is not tied to any platform or operating system. On the other
hand, Elektra is powerful enough to be useful immediately for what it
is written for: to access configuration.

For further views see [why Elektra](/doc/WHY.md)

## Why Is It Important?

The configuration files that represent key databases can have binary
or humanly-readable formats. From the latter, an unmanageable number
is established. Developers of programs tend to document the format of
the configuration file extensively. The configuration file may give a
special flavor to a specific program and users frequently need it.

Sometimes limitations in the configuration file even lead to rewrites
of software. For example, inetd has a non-modular flat configuration
file that is not extensible because of a limited number of rows.
In order to extend its functionality, the program had to be rewritten
with a new approach to configuration: xinetd emerged. Both of these
projects are now almost defined by their configuration files giving them
identity and separating them from each other. Elektra has introduced
[backend](elektra-backends.md) to support the storage of key databases in different
formats.

Elektra abstracts configuration so that applications can receive and
store settings without carrying information about how and where these
are actually stored. It is the purpose of the backends to implement
these details. What makes the difference is the situation that
every program can access any configuration because of the abstraction.
In the example of inetd, Elektra allows an elektrified inetd respective
xinetd to store its configuration in /etc/inetd.conf respective
/etc/xinetd.conf. Additionally, each other program interested in these
preferences can access them in a uniform way.

To support a global key database, a mutual agreement on some level is
needed. Elektra provides this common layer with its data structures.
Each elektrified application lies on top of this abstraction layer and
it can talk to each part of the global key database using the classes
presented next.

## SEE ALSO

- Get a [big picture](/doc/BIGPICTURE.md)
- Start reading about [command-line tools](/doc/help/kdb.md)
