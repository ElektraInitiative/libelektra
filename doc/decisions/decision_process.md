# Decision Process

## Problem

Simply discussing in an issue and then doing an implementation is okay for non-substantial changes.
But substantial decisions must be made in a transparent and participative way.

## Constraints

- All relevant information about decisions must be within Elektra's repository.
- All decisions must go through at least two review rounds, with a merge in between.
- At least two people need to be in favor of the decision in both rounds.
- [Documentation guidelines](/doc/contrib/documentation.md) apply.
- During the decision process, the PRs constantly get updated:
  - Make changes as new commits to the pull request.
  - Questions in the PRs are answered by updating the PR and answering with the commit id.
  - As generally recommended in Elektra, do not squash commits after they are visible on the pull request.
  - Rebase only if the decision was already accepted and has a merge conflict.
- For reviewers:
  - Prefer to directly give suggestions how to change sentences.
  - General questions should be asked in the root of "Conversation" and not at random sentences in the review.

## Assumptions

- People want to be informed about or even participate in what Elektra looks like in the future.
- People writing or reviewing decisions want Elektra to improve, so they also want to accept (acceptable) decisions.
  In general they also want change if it brings Elektra towards its [goals](/doc/GOALS.md) (but doesn't violate Elektra's stability guarantees).
- We will always be able to reach an consensus.
  We don't need a vote (besides the approved review) or a benevolent dictatorship.
- Unlike the Rust Decision process, decisions in Elektra do not have a disadvantage if they were flawed in early stages.
  Only the end results counts.

## Considered Alternatives

- Issues like https://issues.libelektra.org/4521
- PEPs: https://peps.python.org
- RFCs: https://www.ietf.org/standards/rfcs/
- Change requests: https://en.wikipedia.org/wiki/Change_request

## Decision

Decisions need to:

- be implementable within the next major release
- be according to [Elektra's goals](/doc/GOALS.md)
- first be decided upon using the decision process described here

We base our decision process and template on:

- [''using patterns to capture architectural decisions''](http://eprints.cs.univie.ac.at/2345/1/02_Using_Patterns_to_Capture.pdf),
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

Here "the decision" should not have one decision but several well-described solutions.
So for several solutions a proposal, rationale and implication should be given.

This step is finished when every reviewer approves.

### In Progress

- You must include all further alternative proposals made in the "Considered Alternatives" section.
- Now it is allowed to have the decision from the previous round in the "Decision" section.

### Decided

- decision, rationale and implication are now filled out and fixed according to the reviews
- decisions of this status usually already have an implementation PR

### Partially Implemented

This can be useful for decisions that need to be done for every module like plugin or library.
It is for decisions where only a few not-so-important modules are missing and/or issues exist for the remaining pieces.

The "Implication" must clearly say how much of the decision is already implemented.

### Implemented

This step is mandatory.

- Here the details of the decisions are stripped from the decision and moved to the documentation.
- The documentation links to the decision.
- The decision links to the new documentation.

This step is finished when every reviewer approves.

### Rejected

Alternatively, decisions might be rejected (i.e. status quo wins).
These decision PRs are also merged for documentation purposes.

## Rationale

- The process is lightweight and simple.
- The template makes sure important points are not forgotten.
- Every decision is by design in its own file with its own git history.
- PRs allow to better support the constraint that everything must be within Elektra's repository (also rejected PRs).
- PRs allow to suggest changes and review individual sentences of the decision.
- Several "Related Decisions" are very important even if everyone agrees on one solution.
  They allow reviewers and future readers of the decision to understand which options were considered and why they were rejected.

## Implications

- Proposal issues are obsolete.
- The decision process creates at least:
  - two chances to comment decisions, and
  - two commits in the git history.

## Related Decisions

## Notes

- Early discussions in issues or in discussions is not prohibited.
  They don't bring a decision forward, though.
  To not waste time, it is recommended to start with the decision process as described here asap.

Written by Markus Raab 10.10.2022
