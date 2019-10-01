# Contributing

We use [GitHub](https://github.com/ElektraInitiative/libelektra/) to maintain this project. First of all you will need a GitHub account and [Git](https://www.git-scm.com/).

## Configure Git

```sh
$ git config --global user.name "Your Name"
$ git config --global user.email "your_email@example.com"
```

## Fork & Sync 

We use a fork/sync and pull-request (see below) model at GitHub, follow this short [tutorial](https://help.github.com/articles/fork-a-repo/) to get familiar with forking and syncing.

Sync the fork of the repository to keep it up-to-date with the upstream repository by using the following git commands in your local repository. Important: Using these commands will avoid unnecessary merge commits while you are working on a pull-request (see below):

```sh
$ git fetch upstream
$ git rebase upstream/master
$ git push origin master --force
```

## Issues

Check the [Ideas](/doc/IDEAS.md) page if you are searching for a good topic to start with. You can also visit the [issue-tracker](https://github.com/ElektraInitiative/libelektra/issues). Do not hesitate to open a new issue if you want to ask a question, report or fix a bug or approach new topics.

## Creating a Pull-Request

If you want to publish your local changes to this project you have to create a new pull-request.

1. Open your GitHub repository/fork
2. In the tab *Codes* press the *New Pull-Request* button and choose a title and description
3. Select the correct branches 
4. Commit and push your local changes in git (keep in mind to sync your fork - fetch, rebase & push) 
5. Wait for the code-review

## Code-Review

After the pull-request your code will be reviewed and tested with some tests. If your pull-request passes the review , your changes will be merged to the branch.

# General

We recommend that you read:

- [The big picture what Elektra is](/doc/BIGPICTURE.md)
- [The tutorials to get some practice](/doc/tutorials/)
- [The step by step guide of how to contribute](/doc/tutorials/contributing-clion.md)

# Code

Before you issue a pull request that modifies code:

- You should read the [coding document](/doc/CODING.md).
- Make sure you fulfilled the [checklist](/.github/PULL_REQUEST_TEMPLATE.md).

# Architecture

Before you start making fundamental changes:

- Propose it by creating a [github issue](https://github.com/ElektraInitiative/libelektra/issues/new)
  for discussions.
- You should read the [design document](/doc/DESIGN.md).
- Create a [decision](/doc/decisions/README.md) describing you want to do
  to keep the discussion more efficient and architecture documented.

# Labels

- If you do not want your PR to be merged, please label
  the PR with the label "work in progress".
- If you included changes after a review and you think you
  are ready, please tag your PR with the label "ready to merge".
