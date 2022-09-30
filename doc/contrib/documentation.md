# Documentation

This document gives guidelines for contributors concerning Elektra's documentation.
This document takes preference to the actual situation, so if you see documentation
not according to this document, please [create an issue](https://issue.libelektra.org)
or directly fix it with your next PR.

> Note: It is always allowed to improve the documentation, in every PR, even if it is unrelated.

## Target Groups

We write documentation for three groups:

- User:
  Is a person who uses some tool of Elektra, e.g. `kdb`, Qt-Gui or Web-UI.
- Developer:
  Is a person who uses some library of Elektra within an application, tool or plugin.
- Contributor
  Is a person who does changes within Elektra's repository.

> Takeaway:
> Every document must have a clear target group (user, developer or contributor).
> Sometimes it is clear from the directory, e.g. [doc/help](/doc/help) is for users,
> sometimes it must be explicitly stated, e.g. tutorials.

## Orientation

Each documentation should clearly be oriented to one of these 3 directions:

1. learning-oriented: first introduction is done via tutorials, it teaches the basics.
2. information-oriented: the README.md and API docs together are the references: they cover everything that someone needs to know about a [module](/doc/help/elektra-glossary.md).
3. understanding-oriented: the [doc/decisions](/doc/decisions) explain the "Why?" something is done as it is done (Rationale).

In literature there is also goal-oriented but we prefer learning-oriented approaches.
E.g. of course you might have the goal to write a new plugin.
But why not also learn about plugins while creating a new plugin?

> Takeaway:
> Don't try to combine different orientations in one document,
> instead spit your documentation up in e.g. a README.md (information),
> tutorial (learning),
> decisions (understanding).

## Criteria

Elektra's documentation must fulfill:

- It is self-contained.
  It is not enough to link to some paper and an external page to explain, e.g., decisions.
  All explanation must be within the repo, e.g., in case the external information goes away.
- It is consistent in its [terminology](/doc/help/elektra-glossary.md).
- Spelling is American English with [spellings as defined here](/scripts/sed).

> Note:
> Please extend [terminology](/doc/help/elektra-glossary.md) and
> [spellings](/scripts/sed) as needed.

## Completeness

In general the documentation does not need to be complete.
In particular, we do not want repetition of implementation details as documentation.
Nevertheless, there are a few must-haves:

- A README.md must be available for every module.
- A man page (help/kdb-) must be available for every command (including external commands).

## Templates

In general we use [arc42.org](https://arc42.org/) but we use specialized templates for different modules:

- [plugins](/src/plugins/template/README.md).
- [libs](/src/libs/template/README.md).
- [bindings](/src/bindings/template/README.md).
- [tools](/src/tools/template/README.md).
- [decisions](doc/decisions/template.md)
