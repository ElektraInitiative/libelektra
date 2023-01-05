# Git

## Basic Git Commands

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

## Working with forks

We recommend you use your own fork of the main `libelektra` repository, if you want to contribute.
For more information on creating a fork, please take a look at [GitHub's tutorial](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo).

Once you have set up and cloned your fork, you need to keep it in sync with the main repository.
To do that, we recommend you never directly commit anything to your fork's `master` branch.
You also need to add a remote for the main repository:

```sh
# We assume the remote for your fork is called `origin` (the default).
git remote add upstream https://github.com/ElektraInitiative/libelektra.git
```

When you want to sync changes from the main repository into your fork, you can use these commands:

```sh
git fetch upstream master:master
git push orign master:master
```

> **Note**: These commands work with any branch checked out.
> You don't need to switch to the `master` branch first.
> However, this only works, if you have not modified your `master` branch, i.e. the latest commit in your forked `master` branch was once the latest commit in the main `master` branch.
