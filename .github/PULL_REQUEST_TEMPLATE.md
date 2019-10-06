## Basics

These points need to be fulfilled for every PR:

- [ ] Short descriptions of your changes are in the release notes
      (added as entry in `doc/news/_preparation_next_release.md` which
      contains `_(my name)_`)
      **Please always add something to the release notes.**
- [ ] Details of what you changed are in commit messages
      (first line should have `module: short statement` syntax)
- [ ] References to issues, e.g. `close #X`, are in the commit messages.
- [ ] The buildservers are happy.
- [ ] The PR is rebased with current master.

If you have any troubles fulfilling these criteria, please write
about the trouble as comment in the PR. We will help you.
But we cannot accept PRs that do not fulfill the basics.

## Checklist

Check relevant points but **please do not remove entries**.
For docu fixes, spell checking, and similar none of these points below
need to be checked.

- [ ] I added unit tests for my code
- [ ] I fully described what my PR does in the documentation
      (not in the PR description)
- [ ] I fixed all affected documentation
- [ ] I added code comments, logging, and assertions as appropriate (see [Coding Guidelines](https://master.libelektra.org/doc/CODING.md))
- [ ] I updated all meta data (e.g. README.md of plugins and [METADATA.ini](https://master.libelektra.org/doc/METADATA.ini))
- [ ] I mentioned every code not directly written by me in [THIRD-PARTY-LICENSES](doc/THIRD-PARTY-LICENSES)

## Review

Reviewers will usually check the following:

- [ ] Documentation is introductory, concise, good to read and describes everything what the PR does
- [ ] Examples are well chosen and understandable
- [ ] Code is conforming to [our Coding Guidelines](https://master.libelektra.org/doc/CODING.md)
- [ ] APIs are conforming to [our Design Guidelines](https://master.libelektra.org/doc/DESIGN.md)
- [ ] Code is consistent to [our Design Decisions](https://master.libelektra.org/doc/decisions)

## Labels

If you are already Elektra developer:

- Add the "work in progress" label if you do not want the PR to be reviewed yet.
- Add the "ready to merge" label **if the basics are fulfilled** and you also
  say that everything is ready to be merged.
