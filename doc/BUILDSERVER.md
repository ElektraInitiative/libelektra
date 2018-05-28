# Buildserver

## Introduction
Most of the build jobs on the jenkins server that handles automated testing of
contributions have been replaced by a single
[job](https://github.com/ElektraInitiative/libelektra/).


## Jenkinsfile
Jenkinsfiles describe what actions the build system should execute on what
build slave.
Currently Elektra uses two different files.

One is for daily maintanence tasks, like cleaning up build servers
([Jenkinsfile.daily](https://github.com/ElektraInitiative/libelektra/blob/master/scripts/jenkins/Jenkinsfile.daily))
.
The other one is triggered on code changes and is for testing changes to the
codebase
([Jenkinsfile](https://github.com/ElektraInitiative/libelektra/blob/master/scripts/jenkins/Jenkinsfile))
.

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
[daily build](https://build.libelektra.org/jenkins/job/elektra-jenkinsfile-daily/)
is executed according to a cron schedule.
Additionally it can manually triggered by its defined phrase as described in
`GIT.md`.

The [libelektra](https://build.libelektra.org/jenkins/job/libelektra/)
build is triggered for all branches of the libelektra repository except for
`debian`.
Additionally all open branches in forks targeting libelektra's repository via
PRs are going to be build.
Pushes to any of those branches will trigger a new build automatically.
Again a trigger phrase has been added that allows forced rebuilds via the
repository.


## Test Environments
We use Docker containers to provide the various test environments.

They are described
[in the repository](https://github.com/ElektraInitiative/libelektra/tree/master/scripts/docker)
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
This allows us to run tests independent: of which nodes are available (as the
environment is portable).

The Jenkinsfile describes the steps used to run tests.
Helper functions for easily adding new tests are available
(buildAndTest, BuildAndTestAsan, ...).

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
We use it to build debian packages and move it into the repository.
Additionally we recompile the homepage and deploy it.

