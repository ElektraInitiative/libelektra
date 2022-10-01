# Documentation

This document gives guidelines for contributors concerning Elektra's documentation.
This document takes preference to the actual situation.
If you see documentation not according to this document, please [create an issue](https://issues.libelektra.org).
Alternatively, you can directly fix it with your next PR.

> Note:
> It is always allowed to improve the documentation, in every PR, even if the documentation fix is completely unrelated.
> Preferably are several PRs, though.

## Target Groups

We write documentation for three groups:

- User:
  Is a person who uses Elektra, e.g. an application using Elektra or one of Elektra's tools like `kdb`.
- Developer:
  Is a person who uses some library of Elektra within an application, tool or plugin.
- Contributor:
  Is a person who does changes within Elektra's repository.

There are 3 separate folders for these 3 groups:

- [for users](/doc/help)
- [for developers](/doc/dev)
- [for contributors](/doc/contrib)

> Takeaway:
> Every document must have a clear target group (user, developer or contributor).
> Sometimes it is clear from the directory sometimes it must be explicitly stated, e.g. tutorials.

## Orientation

Each documentation should clearly be oriented to one of these 3 directions:

1. learning-oriented:
   First introduction is done via tutorials, they teach the fundamentals.
2. information-oriented:
   The README.md and API docs together are the references:
   They cover everything that someone needs to know about a [module](/doc/help/elektra-glossary.md).
3. understanding-oriented:
   The [doc/decisions](/doc/decisions) explain the "Why?" something is done as it is done, i.e., the rationale.

In literature there is also goal-oriented but we prefer learning-oriented approaches.
E.g. of course you might have the goal to write a new plugin.
But why not also learn about plugins while creating a new plugin?

> Takeaway:
> Don't try to combine different orientations in one document, instead split your documentation up in e.g. a README.md (information), tutorial (learning), decisions (understanding).

## Criteria

Elektra's documentation must fulfill:

- It is self-contained.
  It is not enough to link to some paper and an external page as explanation.
  All explanation must be within the repository, e.g., in case the external information goes away.
  This doesn't apply if the authoritative standard lives outside of Elektra.
  Then you would write, e.g., "The toml plugin implements [this standard](https://toml.io/en/v1.0.0), with following extensions:".
  The extensions, however, again must be fully listed within our repository.
- Always write what **is** not what you would like to have.
  Explanations must always refer to the current situation (as changed with your PR).
- It is consistent with our [terminology](/doc/help/elektra-glossary.md).
- Spelling is American English with [spellings as defined here](/scripts/sed).
- Sentences are short and written in one line.
  I.e. lines usually end with `.`, `:` or `;`.
  Avoid line breaks in the middle of the sentence.
- Use active and strong verbs early in your sentences.
  "We" refers to the community around the Elektra Initiative.
- Use enumerations or itemizations to keep a clear structure in the text.
- We use standard Markdown where possible, with only a few extensions:
  - styled fenced blocks
  - `- [ ]` option lists
  - `- <word>:<line break>` description lists

> Note:
> Please extend [terminology](/doc/help/elektra-glossary.md) and [spellings](/scripts/sed) as needed.

## Completeness

In general the documentation does not need to be complete.
In particular, we do not want repetition of implementation details as documentation.
[Prefer to write self-documenting code](/doc/CODING.md).
Nevertheless, there are a few must-haves:

- A README.md must be available for every module.
- A man page (help/kdb-) must be available for every command (including external commands).
- A tutorial must be present for every important concept.

## Templates

In general we use [arc42.org](https://arc42.org/) but we use specialized templates for different modules:

- [plugins](/src/plugins/template/README.md).
- [libs](/src/libs/template/README.md).
- [bindings](/src/bindings/template/README.md).
- [tools](/src/tools/template/README.md).
- [decisions](/doc/decisions/template.md)
