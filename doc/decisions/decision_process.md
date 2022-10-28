# Decision Process

## Problem

Simply discussing in an issue and then implementing a solution is okay for non-substantial changes.
Substantial decisions, however, must be made in a transparent and participative way.

The main purpose of the decision process is to get a common understanding of the problems and the impacts of possible solutions.

## Constraints

- All relevant information about decisions must be within Elektra's repository.
- All decisions must go through at least two review rounds, with a merge in between.
- At least two people need to approve the decision in each round.
- [Documentation guidelines](https://www.libelektra.org/devgettingstarted/documentation) apply.
- During the decision process, the PRs constantly get updated:
  - Make changes as new commits to the pull request.
  - Questions in the PRs are answered by:
    1. Update the PR and incorporate the review.
    2. Reply on GitHub:
       - Give a short summary of what you did in a single comment, and reply to individual questions if necessary.
       - Link to commit SHA-IDs for details.
    3. Mark all GitHub threads as resolved, if you incorporated the feedback as suggested.
       Committing a suggestion directly on GitHub does this automatically.
  - As generally recommended in Elektra, do not squash commits after they are visible on the pull request.
  - Rebase only if the decision was already accepted and has a merge conflict.
- For reviewers:
  - Prefer to directly give suggestions how to change sentences.
  - General questions should be asked in the root of "Conversation" and not at vaguely related sentences in the review.
- Decision PRs do not significantly change anything but one decision.
- Changes not changing the decision step or the direction of the decision are not decision PRs.
- The person merging the decision PR must be someone else as the person that created the decision.
- The purpose of decisions is to have clear descriptions of technical problems and solutions.
  There is no claim that decisions contain everything that was said.
  In particular corrections of wrong decision text is, if at all, only visible via git history.
  Rather it is important that decisions:
  - contain everything relevant, and
  - what is written is technically correct.
- Participants are explicitly allowed to skip discussions and only read the decisions text.
  If the problem is not yet clear, only partial reviews are encouraged.
  @markus2330 will do so of time budget reasons and to make sure the decisions stay understandable.

## Assumptions

- People want to be informed about or even participate in what Elektra looks like in the future.
- People writing or reviewing decisions want Elektra to improve, so they also want to accept (acceptable) decisions.
  In general people want change if it brings Elektra towards its [goals](/doc/GOALS.md).
- We will always be able to reach an consensus even if it requires that the core or plugins get multiple implementations.
  We don't need a vote (besides the approved review) or a benevolent dictatorship.
- Unlike the Rust Decision process, decisions in Elektra do not have a disadvantage if they were flawed in early stages.
  Only the end results counts.
- Different to initiatives like Rust, most contributors in Elektra are not experts in configuration management or programming languages.
  So we do not expect that a clear problem or solution is in the decision writer's mind beforehand.
  Instead the decision process is a supported learning process.
- People focus on getting the best solutions and not to wish for the impossible.
- After 1.0, due to stability guarantees, not so many decisions are required anymore.
  People are free to reimplement the libraries or plugins of Elektra if they disagree with Elektra's [quality goals](/doc/GOALS.md) and still help with our vision.

## Considered Alternatives

- Issues like https://issues.libelektra.org/4521
- GitHub discussions
- Voting in Meetings
- The maintainer decides
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

In all steps we directly update the decision text with the different opinions.
Discussions should focus on the decision text so that the text evolves with the opinions.

We use the template [TEMPLATE.md](TEMPLATE.md).
Explanations of the template are in [EXPLANATIONS.md](EXPLANATIONS.md).

### Drafts

The first step is to create a PR with:

- **one** decision, where at least the "Problem" and "Considered Alternatives" are filled out.
- a link from [README.md](README.md) from the "Drafts" section to this decision.

> At least the problem must be clear to everyone involved before the decision can leave the "Drafts" status.

### In Discussion

> This step is mandatory.

Here you must ensure:

- consistency with other decisions
- links from/to related decisions are created
- problem, constraint and assumptions are fully described and sound
- there are several considered alternatives, each with rationale and implication
- decision, rationale and implications is **not** yet filled out if there are people arguing for different options (to keep the discussion unbiased)

Here "the decision" should not only have one decision but should describe several solutions.
For each solution a proposal, rationale and implication should be given.

### In Progress

- You must include all further alternative proposals made in the "Considered Alternatives" section.
- Now it is allowed to have the decision from the previous round in the "Decision" section.

### Decided

> This step is recommended.

- decision, rationale and implication are now filled out and fixed according to the reviews
- decisions of this status usually already have an implementation PR

### Partially Implemented

This can be useful for decisions that need to be done for every module like plugin or library.
It is for decisions where only a few not-so-important modules are missing and/or issues exist for the remaining pieces.

The "Implication" must clearly say how much of the decision is already implemented.

### Implemented

> This step is mandatory.

- Here the details of the decisions are stripped from the decision and moved to the documentation.
- The documentation links to the decision.
- The decision links to the new documentation.

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
- The decision process is focused around the decision text (and not forum-like discussions), so that:
  - The resulting text is understandable without reading any discussions.
  - There is a common understanding after only reading the decision text.
  - To avoid any gaps of reading discussions and the decision.

## Implications

- Proposal issues are obsolete.
- The decision process creates at least:
  - two chances to comment decisions, and
  - two commits in the git history.

## Related Decisions

## Notes

- Discussions in issues/discussions are not prohibited.
  They don't bring a decision forward, though.
  To not waste time, it is recommended to start with the decision process as described here asap.

Written by Markus Raab 10.10.2022.
Second discussion round 28.10.2022.
