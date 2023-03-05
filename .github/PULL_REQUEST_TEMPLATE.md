<!--
Check relevant points but **please do not remove entries**.
-->

## Basics

<!--
These points need to be fulfilled for every PR.
-->

- [ ] Short descriptions of your changes are in the release notes
      (added as entry in `doc/news/_preparation_next_release.md` which contains `_(my name)_`)
      **Please always add them to the release notes.**
- [ ] Details of what you changed are in commit messages
      (first line should have `module: short statement` syntax)
- [ ] References to issues, e.g. `close #X`, are in the commit messages.
- [ ] The buildservers are happy. If not, fix **in this order**:
  - add a line in `doc/news/_preparation_next_release.md`
  - reformat the code with `scripts/dev/reformat-all`
  - make all unit tests pass
  - fix all memleaks
  - fix the CI itself (or rebase if already fixed)
- [ ] The PR is rebased with current master.

<!--
If you have any troubles fulfilling these criteria, please write about the trouble as comment in the PR.
We will help you, but we cannot accept PRs that do not fulfill the basics.
-->

## Checklist

<!--
For documentation fixes, spell checking, and similar none of these points below need to be checked.
-->

- [ ] I added unit tests for my code
- [ ] I fully described what my PR does in the documentation
      (not in the PR description)
- [ ] I fixed all affected documentation (see [Documentation Guidelines](https://www.libelektra.org/devgettingstarted/documentation))
- [ ] I fixed all affected decisions (see [Decision Process](https://www.libelektra.org/decisions/decision-process))
- [ ] I added code comments, logging, and assertions as appropriate (see [Coding Guidelines](https://www.libelektra.org/devgettingstarted/coding))
- [ ] I updated all meta data (e.g. README.md of plugins and [METADATA.ini](https://master.libelektra.org/doc/METADATA.ini))
- [ ] I mentioned [every code](/.reuse/dep5) not directly written by me in [reuse syntax](https://reuse.software/)

## Review

<!--
Reviewers should check the following.
-->

- [ ] Documentation is introductory, concise, good to read and describes everything what the PR does
- [ ] Examples are well chosen and understandable
- [ ] Code is conforming to [our Coding Guidelines](https://master.libelektra.org/doc/CODING.md)
- [ ] APIs are conforming to [our Design Guidelines](https://master.libelektra.org/doc/DESIGN.md)
- [ ] Code is consistent to [our Design Decisions](https://master.libelektra.org/doc/decisions)

## Labels

<!--
If you are already Elektra developer, please adjust the labels.
Otherwise, write a comment and it will be done for you.
-->

- [ ] Add the "work in progress" label if you do not want the PR to be reviewed yet.
- [ ] Add the "ready to merge" label **if everything is done** and no further pushes are planned by you.
