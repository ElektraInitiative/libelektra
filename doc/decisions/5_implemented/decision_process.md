# Decision Process

## Problem

Simply discussing in an issue and then implementing a solution is okay for non-substantial changes.
Substantial decisions, however, must be made in a transparent and participative way.

- Discussing fundamental problems in forum-like threads showed to be repetitive and ineffective.
- Decisions by supervisors are undemocratic.
- Decisions in meetings are nontransparent for the outside world.

### Terminology

- `Decision`:
  A text file which contains the content as [explained here](../EXPLANATIONS.md).
- `Decision PR`:
  A pull request that contains substantial changes for a decision.
- `Decision author`:
  Is the person who creates the decision PR.
- `Decision reviewers`:
  Are the people who review the decision PR.

### Main Purpose

The main purpose of decisions is

- to have clear descriptions of technical problems and solutions.
- to get a common understanding of the problems and the impacts of possible solutions.

## Constraints

- All relevant information about decisions must be within Elektra's repository.
- All decisions must go through at least two review rounds, with a merge in between.
- At least two people need to approve the decision in each round.
- [Documentation guidelines](../../contrib/documentation.md) apply to decisions.
- During the decision process, the PRs constantly get updated:
  - Make changes as new commits to the pull request.
  - Questions in the PRs are answered by:
    1. Update the PR and incorporate the review.
    2. Reply on GitHub:
       - Give a short summary of what you did in a single comment, and reply to individual questions if necessary.
       - Link to commit SHA-IDs for details.
    3. Mark all GitHub threads as resolved, if you incorporated the feedback as suggested.
       Committing a suggestion directly on GitHub does this automatically.
  - As generally recommended in Elektra, do not squash commits after they are visible in a pull request.
  - Rebase only if the decision was already accepted and has a merge conflict.
- For decision reviewers:
  - Prefer to directly give suggestions how to change sentences.
  - General questions should be asked in the root of "Conversation" and not at vaguely related sentences in the review.
- Changes not changing the decision step or the direction of the decision are not decision PRs.
- The person merging the decision PR must be someone other than the person that created the decision.
  There is no claim that decisions contain everything that was said.
  In particular corrections of wrong decision text is, if at all, only visible via Git history.
  Rather it is important that decisions:
  - contain everything relevant, and
  - what is written is technically correct.
- Usually no decisions are needed for libraries, plugins, tools or bindings if:
  - they are similar to already existing modules (e.g. yet another checker plugin)
  - if they reimplement some existing module (e.g. reimplementation in other programming languages)
- Reviewers are only required to read the files in the PR.
  They may not read the discussions surrounding the decision.
  It is encouraged that at least one decision reviewers provides a review _without_ participating in the discussion.
  This ensures that there aren't any unintentional shared assumptions between discussion participants.
- Reviews focus on the "Problem" section first, and only when that is agreed upon focus in the other parts.
- The decision author invites several decision reviewers.
  Ask for help in the decision PR if you do not know whom to invite.

## Assumptions

- People want to be informed about or even participate in what Elektra looks like in the future.
- Decision authors have some scientific background and want decisions based on science, and not only on opinions.
- If assumptions, including this ones written here, are broken, decisions will be redone.
- Decision authors and reviewers want Elektra to improve, so they also want to accept (acceptable) decisions.
  In general people want change if it brings Elektra towards its [goals](/doc/GOALS.md).
- All decision reviewers want to help the decision authors to write a good decision.
- Decision authors are the main force behind a decision and possibly also of specific solutions.
  Nevertheless they don't want to avoid other solutions, are open to arguments/facts/etc. and incorporate all input of decision PR fairly.
- For important decisions, we will able to reach an consensus even if it requires that the core or plugins get multiple implementations.
  We accept indefinite postponement of lesser important decisions, where there is strong disagreement about the problem or the best solution.
  Thus we don't need a vote (besides the approved review) or a benevolent dictatorship.
- Different to initiatives like Rust, most contributors in Elektra are not experts in configuration management or programming languages.
  So we do not expect that a clear problem or solution is in the decision writer's mind beforehand.
  Instead the decision process is a supported learning process.
- People focus on getting the best solutions and not to wish for the impossible.
- People creating decision PRs have strong motives to also finish it.
  They take extra effort on them to be clear about the problem and find the best solution.
- The decision process itself isn't a barrier for people to write their first decision.

## Considered Alternatives

- Issues like https://issues.libelektra.org/4521
- GitHub discussions
- Votings
- maintainer decides
- PEPs: https://peps.python.org
- RFCs: https://www.ietf.org/standards/rfcs/
- Change requests: https://en.wikipedia.org/wiki/Change_request

## Decision

Decisions need to:

- be implementable within the next major release
- be according to [Elektra's goals](/doc/GOALS.md)
- first be decided upon using the decision process described here

We use the template [TEMPLATE.md](../TEMPLATE.md).
Explanations of the template are in [EXPLANATIONS.md](../EXPLANATIONS.md).

Steps are described in [STEPS.md](../STEPS.md):

- Decision authors are the main force to improve the text to get a decision forward.
- Decision authors and decision reviewers together decide about which step a decision currently has.
- Each mandatory step requires two reviews and the merging of the decision PR.
- In each step we directly update the decision text with the different opinions.
  Discussions should focus on the decision text so that the text evolves and improves.

## Rationale

- The process is lightweight and simple.
- The template makes sure important points are not forgotten.
- Every decision is by design in its own file with its own Git history.
- PRs allow to better support the constraint that everything must be within Elektra's repository (also rejected decisions).
- "Decision", "Rationale" and "Implications" are filled out later to keep the discussion unbiased
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
  - two commits in the Git history.
- Decision PRs might be merged quite often until they reach the step "Implemented".
  They might even be merged several times within the same step.
- Decisions encourage to write documentation before actually writing code.
- The proposal encourages healthier discussions despite cognitive biases we all have, see in Notes below.

## Related Decisions

This is the only non-technical decision, so no issues are related.

## Notes

- The first idea often is not the best, don't fixate on it.
  Abraham Luchins called this the “Einstellung effect.”
  Thus we encourage to generate as many ideas as possible for any problem (interrupt effect).
- We have a tendency to add:
  Take courage to also propose solutions that (mostly) remove code.
  See Leidy Klotz et al., e.g. “People systematically overlook subtractive changes“.
- Failures are not a bad thing.
  It is a good thing to have "rejected" decisions for future references and to recheck if assumptions change.
- Decisions are about "Why?" and not about "Who?" or "When?".
  Such discussions should be in separate issues.
- Discussions in issues/discussions are not prohibited.
  They don't bring a decision forward, though.
  To not waste time, it is recommended to start with the decision process as described here asap.
- Make sure that moving between the stages is detected by git.
  E.g. rename in a commit that doesn't rewrite too much of the decision.

Written by Markus Raab.

- First discussion round starting 10.10.2022.
- Second discussion round starting 28.10.2022.
- Third discussion round starting 6.11.2022.
