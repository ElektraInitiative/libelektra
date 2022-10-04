# Documentation

This document gives guidelines for contributors concerning Elektra's documentation.
This document takes preference to the actual situation.
If you see documentation not according to this document, please [create an issue](https://issues.libelektra.org).
Alternatively, you can directly fix it with your next PR.

> **Note:**
> It is always allowed to improve the documentation, in every PR, even if the documentation fix is completely unrelated.
> However, separate PRs are prefered and potentially can get merged sooner.

## Target Groups

We write documentation for three groups:

- **User:**
  A person who uses Elektra, e.g. by using an application, which utilizes Elektra, or one of Elektra's tools like `kdb`.
- **Developer:**
  A person who uses some library of Elektra within an application, tool or plugin.
- **Contributor:**
  A person who makes changes within Elektra's repository.

There are three separate folders for these three groups:

- [for users](/doc/help)
- [for developers](/doc/dev)
- [for contributors](/doc/contrib)

> **Takeaway:**
> Every document must have a clear target group (user, developer or contributor).
> Sometimes it is clear from the directory, sometimes it must be explicitly stated, e.g. for tutorials.

## Orientation

Each documentation should clearly be oriented to one of these three directions:

1. **learning-oriented:**
   First introduction is done via tutorials, they teach the _fundamentals_.
2. **information-oriented:**
   The `README.md` and API docs together are the _references_:
   They cover everything that someone needs to know about a [module](/doc/help/elektra-glossary.md).
3. **understanding-oriented:**
   The [doc/decisions](/doc/decisions) explain the "Why?" something is done as it is done, i.e., the _rationale_.

Literature mentions also goal-oriented concepts, but we prefer _learning-oriented_ approaches.
E.g. of course you might have the goal to write a new plugin.
But why not also learn about plugins while creating a new plugin?

> **Takeaway:**
> Don't try to combine different orientations in one document, instead split your documentation up in e.g. a `README.md` (_information_), tutorial (_learning_) and decisions (_understanding_).

## Criteria

Elektra's documentation must fulfill:

- Always write what **is**, not what you would like to have.
  Explanations must always refer to the current situation (as changed with your PR).
- It is self-contained.
  It is not enough to link to some paper and an external page as explanation.
  All explanation must be within the repository, e.g., in case the external information goes away.
  This doesn't apply if the authoritative standard lives outside of Elektra.
  Then you would write, e.g., "The toml plugin implements [this standard](https://toml.io/en/v1.0.0), with following extensions:".
  The extensions, however, again must be fully listed within our repository. Make sure to link to the correct version of the standard.
- We use standard Markdown where possible, with only a few extensions:
  - styled fenced blocks
  - `- [ ]` option lists
  - `- <word>:<line break>` description lists
- The documentation should be as near to the code as possible.

> **Takeaway:**
> Include full API and Markdown documentation of the current situation directly in your PRs.

## Style

- Sentences are short and written in one line.
  I.e. lines usually end with `.`, `:` or `;`.
  Avoid line breaks in the middle of the sentence.
- Use active and strong verbs early in your sentences.
  "We" refers to the community around the Elektra Initiative.
- Use headings and lists to keep a clear structure in the text.
- Use examples and images to emphasize important points, don't overuse emphasis in text (bold, etc.).
- Spelling is American English with [spellings as defined here](/scripts/sed).
- It is consistent with our [terminology](/doc/help/elektra-glossary.md).

> **Note:**
> Please extend [terminology](/doc/help/elektra-glossary.md) and [spellings](/scripts/sed) as needed.

## Completeness

In general the documentation does not need to be complete.
In particular, we do not want repetition of implementation details as documentation.
[Prefer to write self-documenting code](/doc/CODING.md).
Nevertheless, there are a few must-haves:

- A `README.md` must be available for every module.
- A man page (`help/kdb-`) must be available for every command (including external commands).
- A tutorial must be present for every important concept.
- Every documentation page must be listed in the [Website's structure](/src/tools/website/resources/structure.json.in).
- Everything copied must be properly licensed in [reuse](/.reuse/dep5).

## Links

Generously use links but be very careful to create a coherent documentation (German: "roter Faden"):

- Clearly separate between prerequisites and further readings.
- _Prerequisites:_
  Concepts people need to know before reading the documentation must be linked in the beginning.
- When adding links, check if users cannot easily get lost in circles.
- To link to Elektra's files use internal links.
  Use absolute or relative links as appropriate.
  E.g. for files within the same folder use relative links.
- For release notes use only external links to:
  - [www.libelektra.org](https://www.libelektra.org) whenever possible, e.g. to tutorials, plugins, etc.
  - [master.libelektra.org](https://master.libelektra.org) to link to source files
- For external links use **https** links, if available.
- Only use `*.libelektra.org/*` links, avoid `github.com/ElektraInitiative/*` links.
  Create an issue if redirects are missing.
  Rationale:
  Then we can more easily move to other git hosting platforms.
  Redirects created for this purpose:
  - [issues.libelektra.org](https://issues.libelektra.org) for the issue tracker
  - [pulls.libelektra.org](https://pulls.libelektra.org) for pull requests
  - [git.libelektra.org](https://git.libelektra.org) as main page to the source repository

> **Takeaway:**
> Links are very helpful to readers.
> Make sure documentation can be read one after the other with these links (German: "roter Faden").

## Templates

In general we use [arc42.org](https://arc42.org/) but we use specialized templates for different modules:


- [plugins](/src/plugins/template/README.md)
- [libs](/src/libs/template/README.md)
- [bindings](/src/bindings/template/README.md)
- [tools](/src/tools/template/README.md)
- [decisions](/doc/decisions/template.md)
