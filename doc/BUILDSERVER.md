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

## Modern Build Jobs
We reworked most of our build system to use a more modern approach with
benefits such as

* tracking modifications to our build process in SCM
* tracking changes to the build environment
* speeding up builds

.

This section aim to give an introduction into this new setup.

### Multibranch Pipeline Jobs (libelektra)
We use the Jenkins Job type called Multibranch Pipeline for our main
tests called `libelektra`.

Simplified a Multibranch Pipeline Job acts as an umbrella job that spawns
child jobs depending on branch (hence multibranch).
The main purpose of the libelektra job is to scan the repository for
changes to existing branches or to find new ones (for example branches
that are used in PR's).
It also contains the information on how to build the jobs in the form
of a path that points to a Jenkinsfile containing more detailed instructions
as well as some configuration that should be shared by all resulting jobs.
The job also takes care of handling build artifacts that are archived and
cleaning them out once the PR's are closed and a grace period has expired.

Summarized libelektra's job purpose is to combine the where
(our GIT repository), with a when (tracking changes via polling or webhooks)
with a how (pointing to the Jenkinsfile + configuration).


### Jenkinsfiles
Jenkinsfiles describe what actions the build system should execute on what
build slave.
Currently Elektra uses two different files.

#### Jenkinsfile.daily
* Jenkinsfile.daily is for daily maintanence tasks, like cleaning up build servers.
* [Buildjob: libelektra-daily](https://build.libelektra.org/jenkins/job/libelektra-daily/)
* [Jenkinsfile.daily](https://master.libelektra.org/scripts/jenkins/Jenkinsfile.daily)

#### Jenkinsfile
* Triggered on code changes and is for testing changes to the codebase.
* [Buildjob: libelektra](https://build.libelektra.org/jenkins/job/libelektra/)
* [Jenkinsfile](https://master.libelektra.org/scripts/jenkins/Jenkinsfile)

#### DSL
The language used is a groovy based DSL described in the
[Jenkinsfile book](https://jenkins.io/doc/book/pipeline/jenkinsfile/).
Most groovy syntax is allowed to describe pipelines in a Jenkinsfile, but it
is executed in a sandbox which might block certain calls.

Since plugins might extend the pool of available commands or variables a full
list of currently available syntax can be seen in
[pipeline
syntax](https://build.libelektra.org/jenkins/job/libelektra/pipeline-syntax/)
after a login to the build server.

We also provide a number of helper functions in our Jenkinsfiles that are
documented at the function head.

### Security
Since a malicious PR could easily destroy the build server and slaves or expose
credentials some restrictions are introduced.
Only PR authors that have the right to push to libelektra can modify the
Jenkinsfile and have those changes be respected for the respective branch.

This setting can be modified on the respective build job configuration site.

### Test Environments
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

### Tests
We will use the Docker images build as described earlier to run compilations
and tests for Elektra.
This allows us to run tests independent of which nodes are available (as the
environment is portable).

The Jenkinsfile describes the steps used to run tests.
Helper functions for easily adding new tests are available
(buildAndTest, BuildAndTestAsan, ...).

The `withDockerEnv` helper makes sure to print the following information at the
start of a test branch:
* branch name
* build machine
* docker image id

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

### Deployment
For runs of the build job that are run in the master branch we also execute
deployment steps after all tests pass.
We use it to build debian packages and move it into the repository on the a7
node.
Additionally we recompile the homepage and deploy it on the a7 node.

## Jenkins Setup
This section describes how to replicate the current Jenkins configuration.

### Jenkins libelektra configuration
The `libelektra` build job is a multibranch pipeline job.
It is easiest to add via the BlueOcean interface.

Most of the default settings should be ok, however some settings need to be
verified or added to build Elektra correctly:
* In Branch Sources under Behaviours `Filter by name` should be
    added to exclude the `debian` branch from being build.
    The reason for this is that the `debian` branch is not providing a
    Jenkinsfile.
* `Advanced clone behaviors` should be added and the path to the git mirror
    needs to be specified: `/home/jenkins/git_mirrors/libelektra`.
    This reference repository is created and maintained by our
    [daily buildjob](https://build.libelektra.org/jenkins/job/libelektra-daily/).
* Under Property strategy you can add `Trigger build on pull request comment`.
    `jenkins build (libelektra|all) please` is a good starting point.
    It should be noted that this functionality is provided by a non standard
    plugin and might not be available in a new installation.
* For Build Configuration you want to specify `by Jenkinsfile` and add the
    script path: `scripts/jenkins/Jenkinsfile`.

### Adding a Jenkins node
A node needs to have a JRE (Java Runtime Environment) installed.
Further it should run an SSH (Secure SHell) server.
If you want to provide environments via Docker you need to install that as well.

A `jenkins` user with 47000:47000 ids should be created as this is what is
expected in Docker images.
Additionally a public key authentification should be set up so the jenkins
master can establish an ssh connection with the node.
If the node should be able to interact with Docker the jenkins user should be
added to the `docker` group.

Nodes should be set to only build jobs matching their labels and to be online
as much as possible.
As for labels `gitmirror` should be if you want to cache repositories on this
node.
If Docker is available the `docker` label should be set.

## Understanding Jenkins output
Our jenkins build uses parallel steps inside a single build job to do most of
the work.
To reliable determine which stages failed it is best to look over the build
results in the Jenkins Blue Ocean view.
It is the default View opened when accessing the build results from github.
For libelektra the Url's are
https://build.libelektra.org/jenkins/job/libelektra/ and
https://build.libelektra.org/jenkins/blue/organizations/jenkins/libelektra/branches/
.

Failed stages are marked in red.
On selecting a stage you can further expand all steps in the stage.
Of interest should be one of the first steps which echos out the Docker image
used to run the stage.
You also want to look for whatever failed (which should be in a step also marked
red to indicate failure).

## Reproducing buildserver errors locally
First you have to determine which image is used.
This is described above in *Understanding Jenkins output*.

Afterwards you can download it from our registry via `docker pull`.
Pay attention that you have to use **hub-public.libelektra.org** as this vhost
does not require authentification for GET operations used by the Docker client.
As an example:
```
docker pull hub-public.libelektra.org/build-elektra-alpine:201809-791f9f388cbdff0db544e02277c882ad6e8220fe280cda67e6ea6358767a065e
```

You can also rebuild the images locally.
Locate the image in the `DOCKER_IMAGES` map in the Jenkinsfile.
It maps the stage that failed to the Dockerfile in the repository.
Now you can build the image:

```
docker build --build-arg JENKINS_GROUPID=`id -g` \
             --build-arg JENKINS_USERID=`id -u` \
             -t libelektra-debian-stable \
             path/to/identified/Dockerfile/
```

In case the Filename deviates from the default `Dockerfile` you also
have to supply the `-f` argument.
Please consult the docker build --help output for more information.

To run the build commands in the container run the following from libelektra's
base directory:
```
docker run -it \
           -v `pwd`:/home/jenkins/ws \
           -w /home/jenkins/ws \
           libelektra-debian-stable
```

Combined with the user arguments during the build this allows to use the
usage of the source dirctory with the correct rights.

## Modify test environments
You can also modify the test environments (update a dependency, install a new
dependency, ...) by editing the Dockerfiles checked into SCM.
Determine which ones you need to modify by tracing which images are used for
what stages via the `DOCKER_IMAGES` map used in our Jenkinsfile.
The `dockerInit` function should be a good start point.

Following docker best practises you should try to minimize layer count
when possible.
Our docker images are quite large and hence take a long time to build so make
sure to test any modifications locally first.

# Triggers
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
* jenkins build [git-buildpackage-jessie](https://build.libelektra.org/job/elektra-git-buildpackage-jessie/) please
* jenkins build [libelektra](https://build.libelektra.org/jenkins/job/libelektra/) please

Additionally `jenkins build all please` can be used to trigger all build jobs
relevant for PR's.
Since this needs a lot of resources please use it only if

- all of the **standard PR jobs** were already **successful**, and
- you are sure that you **do not want change anything** in your PR anymore

.

# Issues with the build environment
If you have issues that are related to the build system you can open a normal
issue and tag it with `build` and `question`.
If you feel like your inquiry does not warrent a issue on its own, please use
[our buildserver issue](https://issues.libelektra.org/160).
