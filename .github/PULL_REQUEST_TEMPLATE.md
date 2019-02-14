## Basics

Do not describe the purpose here but:

- [ ] Short descriptions should be in the release notes (added as entry in
      `doc/news/_preparation_next_release.md` which contains `*(my name)*`)
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
- [ ] I added code comments, logging, and assertions (see doc/CODING.md)
- [ ] meta data is updated (e.g. README.md of plugins)

## Review

Remove the line below and add the "work in progress" label if you do
not want the PR to be reviewed:

@markus2330 please review my pull request
