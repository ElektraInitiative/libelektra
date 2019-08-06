## Basics

Check relevant points but **please do not remove entries**.
Do not describe the purpose of this PR in the PR description but:

- [ ] Short descriptions should be in the release notes (added as entry in
      `doc/news/_preparation_next_release.md` which contains `_(my name)_`)
      **Please always add something to the the release notes.**
- [ ] Longer descriptions should be in documentation or in design decisions.
- [ ] Describe details of how you changed the code in commit messages
      (first line should have `module: short statement` syntax)
- [ ] References to issues, e.g. `close #X`, should be in the commit messages.

## Checklist

Check relevant points but **please do not remove entries**.
For docu fixes, spell checking, and similar none of these points below
need to be checked.

- [ ] I added unit tests
- [ ] I ran all tests locally and everything went fine
- [ ] affected documentation is fixed
- [ ] I added code comments, logging, and assertions (see [Coding Guidelines](https://master.libelektra.org/doc/CODING.md))
- [ ] meta data is updated (e.g. README.md of plugins and [METADATA.ini](https://master.libelektra.org/doc/METADATA.ini))

## Review

Reviewers will usually check the following:

- [ ] Code is conforming to [our Coding Guidelines](https://master.libelektra.org/doc/CODING.md)
- [ ] APIs are conforming to [our Design Guidelines](https://master.libelektra.org/doc/DESIGN.md)
- [ ] Code is consistent to [our Design Decisions](https://master.libelektra.org/doc/decisions)
- [ ] Documentation is concise and good to read
- [ ] Examples are well chosen and understandable

## Labels

- Add the "work in progress" label if you do not want the PR to be reviewed yet.
- Add the "ready to merge" label **if the build server is happy** and also you
  say that everything is ready to be merged.
