# GIT

## Basic GIT Commands

```sh
git add readme.md   # adds the changes of the file `readme.md` to the staging area
git add .           # adds all changes of files in the current directory (recursively) to the staging area
git add --all       # adds all changes of files in the repository to the staging area
git commit -a       # executes a commit that automatically stages all changed and deleted files before
```

## Basic Configuration

make sure to do:

```sh
git config --global merge.ff false
git config merge.ff false
```

## The Commit Message

A commit message should have the following syntax:
`component: short change description`

For a clean and meaningful log the commit
message should fulfil the following:

- use imperative in the subject line
- the subject line should not be longer than 50 characters
- start the subject line with the module name (e.g. resolver:, cpp bindings:)
- separate subject from body with a blank line
- in the body describe in detail what you did, and possibly why
- metadata like "Fixes #123" should be kept at the bottom of the commit message and definitely not in the title

Most commits should have a longer description in the body.

## Remote Branches

To list all remote branches use:

```sh
git branch -a
```

To checkout a remote branch initially use:

```sh
git checkout -b <branchname> origin/<branchname>
```

Once you have done this, it will be a local branch, too.
Following remote branches should exist:

    master

This is the development branch. Please try
to not work directly on it, but instead
you should use feature branches. So the
only commits on master should be non-fastforward
merges from features branches. Commits on
master should always compile and all test
cases should pass successfully.
(see config option above)

## Local Branches

You should always make your own feature branch with:

```sh
git checkout -b <feature-branch-name>
```

On this branch it is not so important that every
commit compiles or all test cases run.

To merge a branch use (no-fastforward):

```sh
git merge --no-ff <branchname>
```

If you already did some commits, but want them in a branch,
you can do:

```sh
git branch foo
git reset HEAD^^  # for 2 commits back
git reset origin/master

git-ref-log # recover
```
