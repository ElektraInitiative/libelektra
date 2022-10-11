# Decision Process

## Problem

Simply discussing in an issue and then doing an implementation is okay for non-substantial changes.
But substantial decisions must be made transparently and in a participative way.

## Constraints

- All relevant information about decisions must be within Elektra's repository.
- All decisions must go through at least two review rounds, with a merge in between.
- At least two people need to be in favor of the decision in both rounds.
- [Documentation guidelines](/doc/contrib/documentation.md) apply.

## Assumptions

- People want to be informed about or even participate in what Elektra looks like in the future.

## Considered Alternatives

- Issues like https://issues.libelektra.org/4521
- PEPs: https://peps.python.org
- RFCs: https://www.ietf.org/standards/rfcs/
- Change requests: https://en.wikipedia.org/wiki/Change_request

## Decision

Decisions need to:

- be implementable within the next major release
- be according to [Elektra's goals](/doc/GOALS.md)
- first being decided upon using the decision process described here

We base our decision process and template on:

- [''using patterns to capture architectural decisions''](https://dl.acm.org/doi/10.1109/MS.2007.124),
- [arc42 decisions](http://docs.arc42.org/section-9/),
- [ADR](https://adr.github.io/), and
- [RFCs in rust-lang](https://github.com/rust-lang/rfcs).

Following subsections describe all steps a decision might run through.
Only two of them are mandatory.

Explanations of the template are in [explanations.md](explanations.md).

### Drafts

The first step is to create a PR with:

- **one** decision copied from [template.md](template.md), where at least the "Problem" and "Considered Alternatives" are filled out.
- a link from [README.md](README.md) from the "Drafts" section to this decision.

### In Discussion

This step is mandatory.

Here you must ensure:

- consistency with other decisions
- links from/to related decisions are created
- problem, constraint and assumptions are fully described and sound
- there are several considered alternatives, each with rationale and implication
- decision, rationale and implications is **not** yet filled out if there are people arguing for different options (to keep the discussion unbiased)

### In Progress

- You must include all further alternative proposals made in the "Considered Alternatives" section.
- Now it is allowed to have the decision from the previous round in the "Decision" section.

### Decided

- decision, rationale and implication are now filled out and fixed according to the reviews
- decisions of this status usually already have an implementation PR

### Mostly Implemented

This can be useful for decisions that need to be done for every module like plugin or library.
It is for decisions where only a few not-so-important modules are missing and/or issues exist for the remaining pieces.

### Implemented

This step is mandatory.

- Here the details of the decisions are stripped from the decision and moved to the documentation.
- The documentation links to the decision.
- The decision links to the new documentation.

### Rejected

Alternatively, decisions might be rejected (i.e. status quo wins).
These decision PRs are also merged for documentation purposes.

## Rationale

- The process is lightweight and simple.
- The template helps not forgetting important points.
- PRs allow to better support the constraint that everything must be within Elektra's repository (also rejected PRs).
- PRs allow to suggest changes and review individual sentences of the decision.

## Implications

- Proposal issues are obsolete.
- The decision process creates at least:
  - two chances to comment decisions, and
  - two commits in the git history.

## Related Decisions

## Notes

Written by Markus Raab 10.10.2022
