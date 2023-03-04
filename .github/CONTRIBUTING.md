# Contributing

We use [GitHub](https://github.com/ElektraInitiative/libelektra/) to maintain this initiative.
First you will need a GitHub account and [Git](https://www.git-scm.com/).
If you are interested to use an IDE, there are IDEs which Elektra developers currently use, e.g.:

- [CLion](https://www.jetbrains.com/clion/) with a [tutorial](/doc/tutorials/contributing-clion.md)
- [Visual Studio Code](https://code.visualstudio.com/) with a [tutorial](/doc/tutorials/contributing-windows.md)

## Configure Git

```sh
$ git config --global user.name "Your Name"
$ git config --global user.email "your_email@example.com"
```

When you connect to a GitHub repository from Git, you have to set up the [authentication](https://help.github.com/en/articles/set-up-git#next-steps-authenticating-with-github-from-git) settings.

Check out our guide on [Git](/doc/GIT.md) for more details

## Fork & Sync

We use a fork/sync and pull-request (see below) model at GitHub, follow this short [tutorial](https://help.github.com/articles/fork-a-repo/) to get familiar with forking and syncing.

Sync the fork of the repository to keep it up-to-date with the upstream repository by using the following git commands in your local repository. Important: Using these commands will avoid unnecessary merge commits while you are working on a pull-request (see below):

```sh
$ git fetch upstream
$ git rebase upstream/master
$ git push origin master --force
```

## Issues

Check the [Ideas](/doc/IDEAS.md) page if you are searching for a good topic to start with.
You can also visit the [issue-tracker](https://github.com/ElektraInitiative/libelektra/issues) and filter the list by pre-defined labels like [good first issue](https://github.com/ElektraInitiative/libelektra/labels/good%20first%20issue).
Do not hesitate to open a new issue if you want to ask a question, report or fix a bug or approach new topics.

## Ways to Contribute

- If you want to contribute on existing [plugins](/src/plugins/README.md), or create new ones, like validators, check out the tutorial on [how to write plugins](/doc/tutorials/plugins.md).
- Check out how you can contribute to make Elektra work in your favorite language: [Language Bindings](src/bindings/README.md), [Tutorial on Language Bindings](/doc/tutorials/language-bindings.md).
- Improve the reliability of Elektra, by creating new, and improve existing [tests](/doc/TESTING.md).
  [Check out the Code Coverage Report](https://doc.libelektra.org/coverage/master/debian-bullseye-full/).
  [You can also create tests for tutorials](https://github.com/ElektraInitiative/libelektra/tree/master/tests/shell/shell_recorder/tutorial_wrapper)

## Creating a Pull-Request

If you want to publish your local changes to this project you have to create a new pull-request.

1. Open GitHub and navigate to _your_ libelektra-fork
2. In the tab _Code_ press the _New Pull-Request_ button and choose a title and fill in the [pull-request template](/.github/PULL_REQUEST_TEMPLATE.md)
3. Now you should be able to set up the _Comparing changes_ settings. Choose the head repository and base repository.
   E.g.: if you have some changes in the master branch of your forked repository, select *head repository: yournickname/libelektra and *compare: master* and *base repository: ElektraInitiative/libelektra* and *base: master\*
4. Add some information about the changes in the release notes (path of the file: /libelektra/doc/news/\_preparation_next_release.md), skipping this step may cause a rejected pull-request
5. Do not forget to format your code since the build server fails instantly. Take a look at the [coding guidelines](/doc/CODING.md) for more information.
6. Commit and push your local changes in git (keep in mind to sync your fork - fetch, rebase & push)
7. Wait for the code-review or ask others to review

## Code-Review

After creating the pull-request, your code will be reviewed by other developers.
If your pull-request passes the review and the build server accepts your changes, they will be merged into the master branch.

Got rejected?
Don't worry... there are many reasons why the build server could fail.
Open your created pull-request in GitHub and scroll to the bottom of the site, there should be a list of all checks - by clicking on the _Details_, you get more information about the checks.
There are, however, also cases where the websites are down, build server gets stuck or times-out etc., please get also an overview of the [build server](/doc/BUILDSERVER.md) to avoid frustration and long review times.

# General

We recommend that you read:

- [The big picture what Elektra is](/doc/BIGPICTURE.md)
- [The tutorials to get some practice](/doc/tutorials/)
- [The Documentation Guidelines](https://www.libelektra.org/devgettingstarted/documentation)

# Code

Before you issue a pull request that modifies code:

- You should read the [coding document](/doc/CODING.md).
- Make sure you fulfilled the [checklist](/.github/PULL_REQUEST_TEMPLATE.md).

# Architecture

Before you start making fundamental changes:

- You should read the [design document](/doc/DESIGN.md).
- Create a [decision](/doc/decisions/README.md) describing you want to do
  to keep the discussion more efficient and architecture documented.

# Labels

- If you do not want your PR to be merged, please label the PR with the label "work in progress".
- If you included changes after a review and you think you are ready, please tag your PR with the label "ready to merge".
