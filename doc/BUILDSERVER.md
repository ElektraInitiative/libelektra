# Buildserver

The [Elektra buildserver](https://build.libelektra.org/) handles a variety of
tasks that reaches from testing Pull Requests (PRs) to deploying new versions
of the Elektra homepage.

## Legacy build jobs
Build jobs that are not ported into Jenkinsfiles use
`Github Pull Request Builder` to trigger builds on PR's.

If you are not yet authorized, the following
question will be asked (by user @markus2330):

    Can one of the admins verify if this patch should be build?

Then one of the admins (sorted by activity):

- @sanssecours
- @markus2330
- @beku
- @BernhardDenner
- @fberlakovich
- @manuelm

need to confirm by saying:

    .*add\W+to\W+whitelist.*

or if just the pull request should be checked:

    .*build\W+allow.*

Afterwards builds can be issued via commands as described in `TRIGGERS` below.

## Jenkinsfiles
Jenkinsfiles describe what actions the build system should execute on what
build slave.
Currently Elektra uses two different files.

### Jenkinsfile.daily
* Jenkinsfile.daily is for daily maintanence tasks, like cleaning up build servers.
* [Buildjob: libelektra-daily](https://build.libelektra.org/jenkins/job/libelektra-daily/)
* [Jenkinsfile.daily](https://master.libelektra.org/scripts/jenkins/Jenkinsfile.daily)

### Jenkinsfile
* Triggered on code changes and is for testing changes to the codebase.
* [Buildjob: libelektra](https://build.libelektra.org/jenkins/job/libelektra/)
* [Jenkinsfile](https://master.libelektra.org/scripts/jenkins/Jenkinsfile)

### DSL
The language used is a groovy based DSL described in the
[Jenkinsfile book](https://jenkins.io/doc/book/pipeline/jenkinsfile/).
Most groovy syntax is allowed to describe pipelines in a Jenkinsfile, but it
is executed in a sandbox which might block certain calls.

Since plugins might extend the pool of available commands or variables a full
list of currently available syntax can be seen in
[pipeline
syntax](https://build.libelektra.org/jenkins/job/libelektra/pipeline-syntax/)
after a login to the build server.

## Security
Since a malicious PR could easily destroy the build server and slaves or expose
credentials some restrictions are introduced.
Only PR authors that have the right to push to libelektra can modify the
Jenkinsfile and have those changes be respected for the respective branch.

This setting can be modified on the respective build job configuration site.

## Triggers
All Triggers are described in the configuration of the respective build jobs.

The
[daily build](https://build.libelektra.org/jenkins/job/libelektra-daily/)
is executed according to a cron schedule.

The [libelektra](https://build.libelektra.org/jenkins/job/libelektra/)
build is triggered for all branches of the libelektra repository except for
`debian`.
Additionally all open branches in forks targeting libelektra's repository via
PRs are going to be build.
Pushes to any of those branches will trigger a new build automatically.

The following phrases can be used as comments to manually trigger a specific
build:

* jenkins build [daily](https://build.libelektra.org/jenkins/job/libelektra-daily/) please
* jenkins build [gcc-configure-debian](https://build.libelektra.org/job/elektra-gcc-configure-debian/) please
* jenkins build [gcc-configure-debian-debug](https://build.libelektra.org/job/elektra-gcc-configure-debian-debug) please
* jenkins build [gcc-configure-debian-intree](https://build.libelektra.org/job/elektra-gcc-configure-debian-intree/) please
* jenkins build [gcc-configure-debian-musl](https://build.libelektra.org/job/elektra-gcc-configure-debian-musl/) please
* jenkins build [gcc-configure-debian-shared](https://build.libelektra.org/job/elektra-gcc-configure-debian-shared/) please
* jenkins build [gcc-configure-debian-withspace](https://build.libelektra.org/job/elektra-gcc-configure-debian-withspace/) please
* jenkins build [gcc-configure-xdg](https://build.libelektra.org/job/elektra-gcc-configure-xdg/) please
* jenkins build [gcc-i386](https://build.libelektra.org/job/elektra-gcc-i386/) please
* jenkins build [git-buildpackage-jessie](https://build.libelektra.org/job/elektra-git-buildpackage-jessie/) please
* jenkins build [git-buildpackage-wheezy](https://build.libelektra.org/job/elektra-git-buildpackage-wheezy/) please
* jenkins build [icc](https://build.libelektra.org/job/elektra-icc/) please
* jenkins build [libelektra](https://build.libelektra.org/jenkins/job/libelektra/) please
* jenkins build [local-installation](https://build.libelektra.org/job/elektra-local-installation/) please
* jenkins build [source-package-test](https://build.libelektra.org/job/elektra-source-package-test/) please

Additionally `jenkins build all please` can be used to trigger all build jobs
relevant for PR's.
Since this needs a lot of resources please use it only if

- all of the **standard PR jobs** were already **successful**, and
- you are sure that you **do not want change anything** in your PR anymore

.

## Test Environments
We use Docker containers to provide the various test environments.

They are described
[in the repository](https://master.libelektra.org/scripts/docker)
and the Jenkinsfile describes how to build them.
If a rebuild of the images is needed is determined by the hash of the
Dockerfile used to describe it.
If it has not changed the build step will be skipped.
In the case that an image needed a build it will afterwards be uploaded into a
private Docker image registry (currently on a7) and thus is shared between all
Docker capable build slaves.

## Tests
We will use the Docker images build as described earlier to run compilations
and tests for Elektra.
This allows us to run tests independent of which nodes are available (as the
environment is portable).

The Jenkinsfile describes the steps used to run tests.
Helper functions for easily adding new tests are available
(buildAndTest, BuildAndTestAsan, ...).

Coverage reports are generated automatically when using the buildAndTest helper
and the appropriate Cmake flags for coverage generation have been set. They are
uploaded to https://doc.libelektra.org/coverage/.

Artifacts from `ctest` are also preserved automatically when using
buildAndTest.
The function also takes care of providing a stagename based path so multiple tests
can not overwrite files that share the same name.

Tests are executed in order dictated by the Jenkinsfile.
In general new tests should be added to the 'full build stage' that will only
run after a standard full test run succeeded.
This saves execution time on the build server for most common errors.

Since there is no strict way to enforce the way tests are added we encourage
you to read the existing configuration and modify existing tests so they suite
your needs.

## Deployment
For runs of the build job that are run in the master branch we also execute
deployment steps after all tests pass.
We use it to build debian packages and move it into the repository on the a7
node.
Additionally we recompile the homepage and deploy it on the a7 node.

## Issues with the build environment
If you have issues that are related to the build system you can open a normal
issue and tag it with `build` and `question`.
If you feel like your inquiry does not warrent a issue on its own, please use
[our buildserver issue](https://issues.libelektra.org/160).
