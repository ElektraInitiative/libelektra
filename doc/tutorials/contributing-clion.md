# Introduction

In this tutorial, we will go through the steps necessary to contribute to
Elektra to make it easier for you to get started. We will use a Unix based OS
like Linux and CLion for development, but depending on where you want to
contribute code, other IDEs will also be sufficient. Before you start, please
read [this](/.github/CONTRIBUTING.md) to get familiar with the process of
contributing to Elektra.

## Prerequisites

- Git ([link](https://git-scm.com/download)) or similar software
- CLion ([link](https://www.jetbrains.com/clion/)) or similar software
- GitHub-account ([link](https://github.com/))

## Forking the repository

Libelektra is hosted on GitHub. You can find its repository here:

- https://github.com/ElektraInitiative/libelektra

To be able to make pull requests, you need a copy of this repository inside
your GitHub-account. You can find a tutorial about how to do this
[here](https://help.github.com/en/articles/fork-a-repo) (remember to sign
into your account first). After this, you should see this copy in the list
of your own repositories with a hint of it's origin.

## Getting the source-code

To develop for libelektra, we now have to "download" your copy of it's
original repository. In git this process is called "cloning". Open a
terminal and navigate to the folder you want to save the source code into
and type:

```sh
git clone https://github.com/ElektraInitiative/libelektra.git
```

With the project now locally available we can start developing.

## Setting up the project

It is important to start CLion as root-user, since some functionalities of
_kdb_ require superuser privileges to work as expected. Once you see the
main menu, click "Open" and select the CMakeLists.txt-file inside the
project's root directory. This will import the project accordingly and
populate you run-configuration with some predefined values. In rare
occurrences this won't happen. If that is the case for you, simply restart
CLion using:

_File_ --> _Invalidate Caches / Restart..._ --> _Invalidate and Restart_

Now after all processes of CLion have finished, the project should be set up
and the run-configuration should be populated with entries.

## Development

Usually the folders you have to work in to add functionality or
documentation are as follows:

- doc<br/>
  This folder contains mainly all documentation of the project, including
  almost all pages of the [homepage](https://www.libelektra.org) of this
  project. One important note is, that all markdown-pages can also be used
  for testing using
  [Markdown to Shell Recorder](https://github.com/ElektraInitiative/libelektra/tree/master/tests/shell/shell_recorder/tutorial_wrapper)
  (you can find an example on how to do this [here](/doc/help/kdb-get.md)).
- src<br/>
  Almost all functionality-code resides here.
  - bindings<br/>
    Here is all the code of available [Bindings](/src/bindings/README.md)
    of libelektra. - plugins<br/>
    You can find all developed [Plugins](/src/plugins/README.md) here. If
    you want to fix a bug for an existing plugin or add another one, this is
    the directory you have to work in. For further information on how to
    develop your own plugin, please visit [this](/doc/tutorials/plugins.md)
    tutorial. - tools<br/>
    In this folder the source of all tools for interacting with Electra's
    global key database, e.g. _kdb_, can be found.
- tests<br/>
  Here you can find all sorts of tests (excluding tests created with the
  _Markdown to Shell Recorder_-tool).

## Testing

The most thorough way to test your changes is to run all tests. Therefore
navigate to your run-configurations (Run -> Edit Configurations...) and look
for the entry "run*all". There, "run_all" should be selected as \_Executable*,
"kdb" as _Target_. Now you can execute this run-configuration which will run
all enabled tests!

If you want to test various _kdb_-methods separately, you can create your
own run-configurations. Add a new one by clicking on the "+"-sign on the
top left of the "Edit Configurations..."-window and name it. Here both
_Executable_ and _Target_ should have "kdb" selected. If you for example
want to test `kdb info dump`, write "info dump" next to _Program arguments_.
That's it, now you can just test this part of _kdb_.

For further information please read [this](/doc/TESTING.md).

## Commiting your changes

Once you are satisfied with your changes, you have to commit them to your
forked repository. We'll use the terminal for working with git. By
convention, such commits shouldn't be too large, otherwise it will become
difficult to revert some small changes if they are not working as intended.
To be able to commit code to your repository, you have to configure your
local git-installation to use your GitHub-credentials. You can find
information about how to do this
[here](https://help.github.com/en/articles/set-up-git).

After you've set up your git-configuration, you can continue with uploading
your changes. By default, you are in the master-branch of your repository.
First, you should never directly work on the master branch, since only
working code is expected to be there. This means, we now create a branch in
our repository where our code changes will be published into. To do this,
open a terminal, navigate to the root directory of your local code and type:

```sh
git branch testbranch
git checkout testbranch
```

Here we create a new branch called _testbranch_ and switch to it as our
working branch (of course you can name it as you want). The next step is
to add all files you've changed and also want in your commit to the _stage_.
Suppose we've changed how `kdb get` works, we now have to add it to our files
we want to commit (you can add several files for a commit too):

```sh
git add ./src/tools/kdb/get.cpp
```

Now we've staged our modified file for our commit. The final step is to
actually commit it to your online repository. Therefore type:

```sh
git commit -m "Added functionallity to kdb get. Closes #1234"
git push origin testbranch
```

With this, you've published your changes to your remote branch of your
repository (Assuming you are fixing an issue with the number "1234", otherwise
omit this text). The next step is merging this change into the original
repository.

## Creating a Pull Request

This step is most easily done using a browser. Open the web page of the
git-repository of libelektra (https://github.com/ElektraInitiative/libelektra)
and log in to your account if have not already done so. Navigate to "Pull
requests", there you can find a green button called "New pull request". By
clicking it
([shortcut](https://github.com/ElektraInitiative/libelektra/compare)), you
can now create a pull request referencing your forked repository and the
branch,the modified code resides in. Click on "compare across forks" so that
you can find and select your branch. Choose "<username>/libelectra" as the
head-repository and "testbranch" as the compare-branch. Now the green button
"Create pull requests" should be enabled. By clicking it you can define the
title of your pull request and write a description of the work you have done.
Please read the template in this form and include the information stated there
if possible. Finally by clicking "Create pull request" you've successfully
created a pull request to merge your changes into the official repository! Now
maintainers of libelectra will review your code and, if everything is fine,
merge your changes into the official repository. Otherwise they'll comment on
this pull request if further changes are needed. To include additional changes
in this pull request, just commit new code-changes to the same repository and
branch you've referenced for this pull request, they will be added
automatically to it. In any case check the output of the automated tests!

## Hints

- Especially for not that well-versed programmers don't forget that when
  debugging e.g. `KeySet`, which can contain many `Key`-objects, you can only
  see that there may be more than one key stored in this set by it's
  _size_-attribute. Just the foremost `Key` will be visible directly using the
  debugger.
- Sometimes CLion does not rebuild changed modules accordingly by just running
  the test. If some test-output seems strange to you, try rebuilding the
  project and restart the test.
- Please add a line of changelog to
  [this](/doc/news/_preparation_next_release.md) file and add it to your pull
  request, otherwise some test will fail in any case.
