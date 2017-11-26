# GIT

## Basic GIT Commands

	git add readme.md   // adds the changes of the file `readme.md` to the staging area
	git add .           // adds all changes of files in the current directory (recursively) to the staging area
	git add --all       // adds all changes of files in the repository to the staging area
	git commit -a       // executes a commit that automatically stages all changed and deleted files before

## Basic Configuration

make sure to do:

	git config --global merge.ff false
	git config merge.ff false

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

	git branch -a

To checkout a remote branch initially use:

	git checkout -b <branchname> origin/<branchname>

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

	debian

Is the branch to be used to build debian
packages. It additionally contains the
debian folder. Only debian related commits
should be on the debian branch - otherwise
it should only contain --no-ff merges from
master. (see config option above)

## Local Branches

You should always make your own feature branch with:

	git checkout -b <feature-branch-name>

On this branch it is not so important that every
commit compiles or all test cases run.

To merge a branch use (no-fastforward):

	git merge --no-ff <branchname>

If you already did some commits, but want them in a branch,
you can do:

	git branch foo
	git reset HEAD^^  (for 2 commits back)
	git reset origin/master

	git-ref-log # recover

## Github

When doing merge requests our [buildserver](https://build.libelektra.org)
will build authorized users. If you are not yet authorized, the following
question will be asked (by user markus2330):

	Can one of the admins verify if this patch should be build?

Then one of the admins:

- fberlakovich
- manuelm
- markus2330
- beku

need to confirm by saying:

	.*add\W+to\W+whitelist.*

or if just the pull request should be checked:

	.*build\W+allow.*

or if just a single build of the mergerequest should be started:

	jenkins build please

or if specific jobs should be started:

* jenkins build [bindings](https://build.libelektra.org/job/elektra-test-bindings/) please
* jenkins build [clang](https://build.libelektra.org/job/elektra-clang/) please
* jenkins build [clang-asan](https://build.libelektra.org/job/elektra-clang-asan/) please
* jenkins build [gcc-asan](https://build.libelektra.org/job/elektra-gcc-asan/) please
* jenkins build [fast](https://build.libelektra.org/job/elektra-mergerequests-fast/) please
* jenkins build [gcc](https://build.libelektra.org/job/elektra-gcc/) please
* jenkins build [gcc-configure-debian](https://build.libelektra.org/job/elektra-gcc-configure-debian/) please
* jenkins build [gcc-configure-debian-debug](https://build.libelektra.org/job/elektra-gcc-configure-debian-debug) please
* jenkins build [gcc-configure-debian-intree](https://build.libelektra.org/job/elektra-gcc-configure-debian-intree/) please
* jenkins build [gcc-configure-debian-log](https://build.libelektra.org/job/elektra-gcc-configure-debian-log) please
* jenkins build [gcc-configure-debian-musl](https://build.libelektra.org/job/elektra-gcc-configure-debian-musl/) please
* jenkins build [gcc-configure-debian-nokdbtest](https://build.libelektra.org/job/elektra-gcc-configure-debian-nokdbtest/) please
* jenkins build [gcc-configure-debian-notest](https://build.libelektra.org/job/elektra-gcc-configure-debian-notest/) please
* jenkins build [gcc-configure-debian-shared](https://build.libelektra.org/job/elektra-gcc-configure-debian-shared/) please
* jenkins build [gcc-configure-debian-stretch](https://build.libelektra.org/job/elektra-gcc-configure-debian-stretch/) please
* jenkins build [gcc-configure-debian-optimizations](https://build.libelektra.org/job/elektra-gcc-configure-debian-optimizations/) please
* jenkins build [gcc-configure-debian-wheezy](https://build.libelektra.org/job/elektra-gcc-configure-debian-wheezy/) please
* jenkins build [gcc-configure-debian-withspace](https://build.libelektra.org/job/elektra-gcc-configure-debian-withspace/) please
* jenkins build [gcc-configure-xdg](https://build.libelektra.org/job/elektra-gcc-configure-xdg/) please
* jenkins build [gcc-i386](https://build.libelektra.org/job/elektra-gcc-i386/) please
* jenkins build [gcc47-all](https://build.libelektra.org/job/elektra-gcc47-all/) please
* jenkins build [git-buildpackage-jessie](https://build.libelektra.org/job/elektra-git-buildpackage-jessie/) please
* jenkins build [git-buildpackage-wheezy](https://build.libelektra.org/job/elektra-git-buildpackage-wheezy/) please
* jenkins build [icc](https://build.libelektra.org/job/elektra-icc/) please
* jenkins build [ini](https://build.libelektra.org/job/elektra-ini-mergerequests/) please
* jenkins build [local-installation](https://build.libelektra.org/job/elektra-local-installation/) please
* jenkins build [mingw64](https://build.libelektra.org/job/elektra-gcc-configure-mingw-w64/) please
* jenkins build [multiconfig-gcc-stable](https://build.libelektra.org/job/elektra-multiconfig-gcc-stable/) please
* jenkins build [multiconfig-gcc47-cmake-options](https://build.libelektra.org/job/elektra-multiconfig-gcc47-cmake-options/) please
* jenkins build [source-package-test](https://build.libelektra.org/job/elektra-source-package-test/) please
* jenkins build [stable](https://build.libelektra.org/job/elektra-mergerequests-stable/) please
* jenkins build [unstable](https://build.libelektra.org/job/elektra-mergerequests-unstable/) please
* jenkins build [homepage](https://build.libelektra.org/job/elektra-homepage/) please
* jenkins build [gcc-configure-debian-stretch-minimal](https://build.libelektra.org/job/elektra-gcc-configure-debian-stretch-minimal/) please
* jenkins build [gcc-configure-debian-jessie-minimal](https://build.libelektra.org/job/elektra-gcc-configure-debian-jessie-minimal/) please

If you want any configuration changes, please contact
`Markus Raab <elektra@markus-raab.org>`.
