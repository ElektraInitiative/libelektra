# Introduction

In this tutorial, we will go through the steps necessary to contribute to
Elektra to make it easier for you to get started. We will use a Unix based OS
like Linux and CLion for development, but depending on where you want to
contribute code, other IDEs will also be sufficient. Before you start, please
read [this](/.github/CONTRIBUTING.md) to get familiar with the process of
contributing to Elektra.

## Prerequisites

- [Git](https://git-scm.com/download) or similar software
- [CLion](https://www.jetbrains.com/clion/) or similar software
- [GitHub account](https://github.com/)

## Forking the Repository

Libelektra is hosted on GitHub. You can find its repository here:

- https://github.com/ElektraInitiative/libelektra

To be able to make pull requests, you need a copy of this repository inside
your GitHub account. You can find a tutorial about how to do this
[here](https://help.github.com/en/articles/fork-a-repo) (remember to sign
into your account first). After this, you should see this copy in the list
of your own repositories with a hint of it's origin.

## Getting the Code

To develop for libelektra, we now have to "download" your copy of its
original repository. In git this process is called "cloning". CLion has
built-in Git support which we will use for this tutorial. Once you have
opened CLion click on:

_Check out from Version Control_ --> _Git_

and paste the URL of your forked libelektra-repository inside the
_URL_ field. Then choose the directory you want to store your project in.
To be also able to directly push changes back to your repository, we'll
also configure CLion to be able to use your GitHub account. Click on the
"Log in to GitHub" button and enter your GitHub credentials and confirm.
The last step here is to click "Clone" to download the project and open
it in CLion.

Alternatively you can also clone your repository using the command line.
Open a terminal and navigate to the folder you want to save the source code into
and type:

```sh
git clone https://github.com/ElektraInitiative/libelektra.git
```

With the project now locally available we can start developing.

## Setting Up the Project

To import all of the project's configuration, open CMakeLists.txt inside the
project's root directory and click on "Load CMake project" which will appear
on the top right corner of the source view.

If you've cloned the project using a terminal. start CLion and once you see the
main menu, click "Open" and select the CMakeLists.txt file inside the project's
root directory. This will import the project accordingly and populate you
run configuration with some predefined values. In rare occurrences this won't
happen. If that is the case for you, simply restart CLion using:

_File_ --> _Invalidate Caches / Restart..._ --> _Invalidate and Restart_

Now after all processes of CLion have finished, the project should be set up
and the run configuration should be populated with entries.

Since some _kdb_-operations require root access and it is not recommended to
start programs (like CLion) with root access if they usually don't require it,
we have to change our CMake configurations to get rid of this requirement. To do
that, open:

_File_ --> _Settings_ --> _Build, Execution, Deployment_ --> _CMake_

There you can edit your CMake profiles. To get rid of the root requirement we'll
add the following CMake options to our "Debug" profile:

```sh
-DKDB_DB_SYSTEM="~/.config/kdb/[xyz]/system"
-DKDB_DB_SPEC="~/.config/kdb/[xyz]/spec"
-DKDB_DB_USER=".config/kdb/[xyz]/user"
-DCMAKE_INSTALL_PREFIX=install
```

where "[xyz]" can be replaced by any unique identifier so that different profiles
won't clash with each other. This configuration also isolates your build of
Elektra from any existing Elektra installation on your system. For
debugging purposes we also recommend to add the following CMake options for debug
builds to enable further logging and checks:

```sh
-DENABLE_DEBUG=ON
-DENABLE_LOGGER=ON
```

To increase the build speed you can also change the "Build option" to e.g.

```sh
-j 8
```

which, in this case, starts 8 build jobs in parallel. For optimal performance this
value should represent the number of available cores of your CPU + few extra jobs.

It remains to be noted that CLion maintains all CMake profiles in parallel. If some
CMake file changes, CLion executes `cmake` for each profile which can put a lot of
strain on your system.

Finally check if `ClangFormat` is enabled for the project to automatically adhere
to the formatting guidelines of the project when formatting the code. You can find
the settings here:

_File_ --> _Settings..._ --> _Editor_ --> _Code Style_

Make sure the selected "Scheme" is "Project".

## Development

Usually the folders you have to work in to add functionality or documentation are
as follows:

- doc<br/>
  This folder contains mainly all documentation of the project, including
  almost all pages of the [homepage](https://www.libelektra.org) of this
  project. One important note is, that all Markdown-pages can also be used
  for testing using
  [Markdown to Shell Recorder](https://github.com/ElektraInitiative/libelektra/tree/master/tests/shell/shell_recorder/tutorial_wrapper)
  (you can find an example on how to do this [here](/doc/help/kdb-get.md)).
- src<br/>
  Almost all functionality-code resides here.
  - bindings<br/>
    Here is all the code of available [Bindings](/src/bindings/README.md)
    of libelektra.
  - plugins<br/>
    You can find all developed [Plugins](/src/plugins/README.md) here. If
    you want to fix a bug for an existing plugin or add another one, this is
    the directory you have to work in. For further information on how to
    develop your own plugin, please visit [this](/doc/tutorials/plugins.md)
    tutorial.
  - tools<br/>
    In this folder the source of all tools for interacting with Elektra's
    global key database, e.g. _kdb_, can be found.
- tests<br/>
  Here you can find all sorts of tests (excluding tests created with the
  _Markdown to Shell Recorder_-tool).

## Testing

The most thorough way to test your changes is to run all tests. Therefore
navigate to your run-configurations (Run -> Edit Configurations...) and look
for the entry `run_all`. There, `run_all` should be selected as `Executable`,
`kdb` as `Target`. Now you can execute this run configuration which will run
all enabled tests! Alternatively you can also run all tests using the terminal
by executing `make run_all` inside your build folder (e.g. /cmake-build-debug).

You can also run other specific tests by setting`Target` and `Executable` to
any of the `testmod_*` or `testkdb_*`targets. Additionally all tests using
_Google Test_ (e.g. tests/kdb/\*) can be run directly using CLion by opening
their source code and clicking on the green icon next to the class name.

If you want to test various _kdb_ methods separately, you can create your
own run configurations. Add a new one by clicking on the "+"-sign on the
top left of the "Edit Configurations..." dialog and name it. Here both
_Executable_ and _Target_ should have "kdb" selected. If you for example
want to test `kdb plugin-info dump`, write "info dump" next to _Program arguments_.
That's it, now you can just test this part of _kdb_.

For further information please read [this](/doc/TESTING.md).

## Commiting Your Changes

Once you are satisfied with your changes, you have to commit them to your
forked repository. We'll use the terminal for working with git. By
convention, such commits shouldn't be too large, otherwise it will become
difficult to revert some small changes if they are not working as intended.
If you use the terminal for your Git operations, to be able to commit code
to your repository, you have to configure your local Git installation to
use your GitHub credentials. You can find information about how to do this
[here](https://help.github.com/en/articles/set-up-git). After you've set up
your Git configuration, you can continue with uploading your changes.

By default, you are in the "master" branch of your repository. First, you
should never directly work on the master branch, since only working code is
expected to be there. This means, we now create a branch in our repository
where our code changes will be published into. On the bottom right of your
CLion window you can find the button "Git: <branchname>". Click it and
select "+ New Branch". Type in the name of your new branch (e.g.
"testbranch") and keep "Checkout branch" checked to automatically switch to
it as your working branch.

Alternatively open a terminal, navigate to the root directory of your local
code and type:

```sh
git branch testbranch
git checkout testbranch
```

After you have changed some files it is time to publish them to your
repository. To do that, open:

_VCS_ --> _Commit..._

In the dialog you have opened you can now select the files you want to
include in your commit. The first line should have the following syntax:

```sh
module: short statement
```

If you fixed a bug in `kdb cp` the first line of your commit message could
be `KDB: Fixed cp not copying value`. Your commit message should also
include a reference to the issue you have fixed so that the issue can be
closed automatically once your code change gets included to the official
repository (e.g. `Closes #1234`). Before committing your changes please
make sure that "Reformat code" and "Rearrange code" are disabled in the
commit dialog. Otherwise Clions formatter might produce files that don't
adhere to our formatting guidelines. If you installed the
`pre-commit-check-formatting` pre-commit-hook from the `scripts` directory.
Ensure that "Run Git hooks" is enabled in the commit dialog. Finally you
can commit your changes by clicking the "Commit" button and navigate to:

_VCS_ --> _Git_ --> _Push_

To do that in one step, you can also click on the arrow next to the
"Commit" button and directly choose "Commit and Push...".

Using the terminal you first have to add all files you've changed and also
want in your commit to the _stage_. Suppose we've changed how `kdb cp` works,
we now have to add it to our files we want to commit (you can add several
files for a commit too):

```sh
git add ./src/tools/kdb/cp.cpp
```

Now we've staged our modified file for our commit. The final step is to
actually commit it to your online repository. Therefore type:

```sh
git commit -m "<commit message>"
git push origin testbranch
```

With this, you've published your changes to your remote branch of your
repository. The next step is merging this change into the original
repository.

## Creating a Pull Request

This step is most easily done using a browser. Open the web page of the
Git repository of libelektra (https://github.com/ElektraInitiative/libelektra)
and log in to your account if have not already done so. Navigate to "Pull
requests", there you can find a green button called "New pull request". By
clicking it
([shortcut](https://github.com/ElektraInitiative/libelektra/compare)), you
can now create a pull request referencing your forked repository and the
branch,the modified code resides in. Click on "compare across forks" so that
you can find and select your branch. Choose "<username>/libelektra" as the
head repository and "testbranch" as the compare-branch. Now the green button
"Create pull requests" should be enabled. By clicking it you can define the
title of your pull request and write a description of the work you have done.
Please read the template in this form and include the information stated there
if possible. Finally by clicking "Create pull request" you've successfully
created a pull request to merge your changes into the official repository! Now
maintainers of libelektra will review your code and, if everything is fine,
merge your changes into the official repository. Otherwise they'll comment on
this pull request if further changes are needed. To include additional changes
in this pull request, just commit new code changes to the same repository and
branch you've referenced for this pull request, they will be added
automatically to it. In any case check the output of the automated tests!

If you want to use CLion for creating Pull Request, please check out
[this](https://www.jetbrains.com/help/clion/contribute-to-projects.html#create-pull-request)
link for further information.

## Troubleshooting

### Resolving Missing \*.so Library Error In Debug Mode

In case you fail to run Elektra with the message like this one
`Reason: of module: libelektra-resolver.so, because: libelektra-resolver.so: cannot open shared object file: No such file or directory`
you can solve it by defining the LD_LIBRARY_PATH variable directly in CLion. Click on the debug configurations dropdown in the upper right corner and choose 'Edit Configurations...'.
Then find 'Environmental Variables' field and add the following:
LD_LIBRARY_PATH=PATH_TO_YOUR_LIB_DIRECTORY

Example:

LD_LIBRARY_PATH=/home/username/TU/libelektra/cmake-build-debug/lib

If you want to run built `kdb` outside of CLion, the recommended way is to run this script from your build directory. The script resides in you original directory with project sources.

Example:

```sh
. /PATH/TO/YOUR/PROJECT/scripts/run_dev_env
```

Please keep in mind it sets the variables only in the currently opened shell window/session.

Please refer to [this](/doc/COMPILE.md) tutorial to fix the problem permanently.

## Hints

- Especially for not that well-versed programmers don't forget that when
  debugging e.g. `KeySet`, which can contain many `Key` objects, you can only
  see that there may be more than one key stored in this set by it's
  _size_ attribute since it's keys are referenced by a pointer. Just the first
  `Key` (referenced by the pointer) will be visible directly using the debugger.
  To analyse it's stored `Key` objects you can either add a
  [watch](https://www.jetbrains.com/help/clion/debug-tool-window-watches.html)
  or use the GDB debug console (next to the "variables" tab inside the
  debugger view).
- Sometimes CLion does not rebuild changed modules accordingly by just running
  the test. If you think, that the modules should have been rebuilt (e.g. if
  running `testmod_abc` doesn't rebuild abc), please report the issue under
  https://issues.libelektra.org, so we can fix the CMake dependencies. As
  temporary fix you can try to rebuild the project and restart the test.
- Please add a line of changelog to
  [this](/doc/news/_preparation_next_release.md) file and add it to your pull
  request, otherwise some test will fail in any case.
