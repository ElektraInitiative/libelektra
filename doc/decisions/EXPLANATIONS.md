# EXPLANATIONS

## Problem

Clearly define:

- the context in which the problem exists
- your observation of the problem
- ideally an example for that problem

## Constraints

List all constraints given by:

- use cases
- requirements
- [Elektra's goals](/doc/GOALS.md)
- guidelines, e.g. [documentation guidelines](/doc/contrib/documentation.md)
- standards, e.g. [C99](https://www.open-std.org/jtc1/sc22/wg14/)
- other decisions
- the scope (i.e. describe what shouldn't change)

> Note:
> The decision (but not necessarily the considered alternatives) must fulfill all constraints.

## Assumptions

Assumptions are often overlooked, so this section needs special care and honesty.
Assumptions are what we believe to be true but do not or cannot really know, e.g.:

- what users will accept
- perceived usability
- if the implementation will be faster/slower
- estimations of costs
- problems/risks that might turn up

> Note:
> The decision (but not necessarily the considered alternatives) must not break any assumptions.

## Considered Alternatives

This is a list of all solutions and a rationale why the solution was not taken, e.g. because:

- the solution does not solve the whole problem
- some constraints or assumptions are violated
- another solution:
  - solves the problem better
  - is more in line with Elektra's architecture
  - better supports Elektra's goals
  - better fulfills non-functional requirements

## Decision

Here should be a detailed description of the best solution, i.e., the decision.
It should:

- give all details why the solution:
  - solves the problem best
  - is best in line with our goals
  - fulfills all constraints and assumptions
- describe what needs to be changed
- make clear how the implementation should be done

## Rationale

Here is the description why the decision is the best solution.
Also describe all drawbacks the solution has.

## Implications

Here is a full description of everything that the decision will change or whatever needs to be changed because of the decision.
This can be:

- effect on other decisions, goals, etc.
- non-obvious implementation tasks, e.g. changing in different needs to be implemented
- which issues get solved
- which documentation needs to be updated
- which concepts change
- which guarantees are added/removed

## Related Decisions

This section has links to other decisions with description what the relation is.
Decisions that give constraints must be listed in "Constraints" above.

> Note:
> Sometimes the best solution is only understood if the relation between decisions becomes clear.

## Notes

Here is a full list of off-line discussions, issue trackers, PRs etc. related to this decision.
Preferable it is linked, but if it is not possible, it can also be in full-text here.
If particular information is important and not present in any sections above, please quote it here.

Any discarded, incomplete and unexplored ideas/opinions, which are not complete enough to be "Considered Alternatives", can be written here.
For example, if it is obvious that the idea does not even solve the problem.

Furthermore, the author, acknowledgements, dates etc. can be written here.
