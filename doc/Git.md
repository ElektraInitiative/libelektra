# Git

## Cheat Sheet: Basic Git Commands

```sh
git add readme.md   # adds the changes of the file `readme.md` to the staging area
git add .           # adds all changes of files in the current directory (recursively) to the staging area
git add --all       # adds all changes of files in the repository to the staging area
git commit -a       # executes a commit that automatically stages all changed and deleted files before
git checkout -b     # Create a new local branch based on the current branch
git pull            # Pull changes from the current remote branch
git push            # Push changes to the current remote branch
git stash           # Puts all changes in an dirty wirkling directory aside
git stash apply     # Reapplies stashed changes on the current branch
```

## Installation

- [Windows](https://git-scm.com/download/win)
- [macOS](https://git-scm.com/download/mac)
- [Linux](https://git-scm.com/download/linux)

If you are uncomfortable with command line interfaces, there are also [plenty of GUI options available](https://git-scm.com/downloads/guis).

## Basic Configuration

To configure git, you can use the `git config` command. You can configure each project individually, just by running `git config <setting> <value>` anywhere within a checked out repository, or for all projects for a user on the local machine using `git config --global <setting> <value>`.
We recommend you set the following configuration options, when using git:

```sh
git config merge.ff false # Git will always create a commit, when trying to merge
git config pull.rebase true # Automatically rebase when pulling
```

Also ensure that you have your username and e-mail configured, so your work can be attributed to you:

```sh
git config --global user.name "Your Name"
git config --global user.email "yourname@yourdomain.com"
```

## The Commit Message

A commit message should have the following syntax:
`component: short change description`

For a clean and meaningful log the commit
message should fulfill the following:

- use imperative in the subject line
- the subject line should not be longer than 50 characters
- start the subject line with the module name (e.g. resolver:, cpp bindings:)
- separate subject from body with a blank line
- in the body describe in detail what you did, and possibly why
- metadata like "Fixes #123" should be kept at the bottom of the commit message and definitely not in the title

Most commits should have a longer description in the body.

### GitHub: Attributing Co-Authors

If multiple people worked on a commit, GitHub, supports a nice way to represent this. You can attribute them by leaving two blank lines at the end and then add a single line of `Co-authored-by: NAME <NAME@EXAMPLE.COM>` for every other person that worked on it. [More Details here](https://docs.github.com/en/pull-requests/committing-changes-to-your-project/creating-and-editing-commits/creating-a-commit-with-multiple-authors)

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
master should always compile, and all test
cases should pass successfully.
(see config option above)

### Communication Methods

To communicate with remote branches, Git supports the file protocol (acessible through the local file system), http and ssh. We generally recommend ssh, as it is encrypted and authentication with ssh-keys is simple and secure.

#### GitHub: Adding/creating an SSH Key

GitHub supports both HTTP and SSH for communication. Using SSH, you can remove the annoyance of having to enter your password every time. To learn how to add an SSH-Key follow [this guide provided by GitHub](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account).

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

## Rebasing branches

When you work on a local feature branch, it might happen that a change occurs within the master branch, that introduces merge conflicts. I.e. if you create a pull request, it cannot be merged into master automatically anymore. In these cases, we expect you to rebase your feature branch, so it can be merged automatically once again.

To achieve this, make sure you have your master branch synced up. Then switch to the feature branch you want to update using `git checkout <feature-branch-name>` and then type `git rebase master`. Sometimes, this will result in a merge conflict.

If you already have pushed changes from your feature branch to a remote branch, you'll need to either overwrite it using `git push --force` or push to a new remote branch.

### Resolving merge conflicts

To resolve merge conflicts, edit the files that need to be merged manually. You can check which files need to be merged using `git status`. After you are done merging a file manually, you can use `git add <path-to-file>` and when you have merged all files, continue with `git rebase --continue`.

> **Note**: Keep in mind that manual merges within a rebase are potentially destructive. If you accidentally remove changes you've done in your feature branch, you might be able to recover them using [git-reflog](https://git-scm.com/docs/git-reflog/2.39.0), but they could also be deleted by the [git-gc](https://git-scm.com/docs/git-gc). If you're new to manually merging files, consider creating a new branch from your feature branch using `git checkout -b <new-branch-name>` on which you can perform the merge without risk. When you're stuck on a manual merge, you can also always abort the rebase using `git rebase --abort`

## Further resources

- [Git Book](https://git-scm.com/book/en/v2)
- [GitHub Docs](https://docs.github.com/en)
