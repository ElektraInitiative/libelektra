# INTRODUCTION #

## BASIC GIT COMMANDS ##

    git add .
	git commit -a

## BASIC Configuration ##

make sure to do:  

	git config --global merge.ff false  
	git config merge.ff false  

## Remote Branches ##

To list all remote branches use:

	git-branch -a

To checkout a remote branch initially use:

	git-checkout -b <branchname> origin/<branchname>

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
master.  (see config option above)

## Local branches ##

You should always make your own feature branch with:  

	git-checkout -b <feature-branch-name>

On this branch it is not so important that every
commit compiles or all test cases run.

To merge a branch use (no-fastforward):  

	git-merge --no-ff <branchname>

If you already did some commits, but want them in a branch,
you can do:  

	git-branch foo  
	git reset HEAD^^  (for 2 commits back)  
	git reset origin/master  

	git-ref-log # recover

## Github ##

When doing merge requests our [buildserver](http://build.libelektra.org:8080)
will build authorized users. If you are not yet authorized following
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
or if the bindings job should be started:
	jenkins build bindings please

If you want any configuration changes, please contact
`Markus Raab <elektra@markus-raab.org>`.
