# Buildserver

The [Elektra buildserver](https://build.libelektra.org/) handles a variety of
tasks that reaches from testing Pull Requests (PRs) to deploying new versions
of the [Elektra website](https://www.libelektra.org).

We reworked our build system to use a more modern Jenkinsfile approach with
benefits such as

- tracking modifications to our build process in SCM
- tracking changes to the build environment
- speeding up builds

.

## Setup

This section aims to give an introduction into this setup.

### Multibranch Pipeline Jobs (libelektra)

We use the Jenkins Job type called
[Multibranch Pipeline](https://jenkins.io/doc/book/pipeline/multibranch/#creating-a-multibranch-pipeline)
provided by the
[Pipeline Multibranch Plugin](https://wiki.jenkins.io/display/JENKINS/Pipeline+Multibranch+Plugin)
for our CI tests called `libelektra`.

Simplified a multibranch pipeline job acts as an umbrella job that spawns
child jobs for different branches (hence multibranch).
The main purpose of the libelektra job is to scan the repository for
changes to existing branches or to find new ones (for example branches
that are used in PR's).
It also contains the information on how to build the jobs in the form
of a path that points to a Jenkinsfile containing more detailed instructions.
The job also takes care of handling build artifacts that are archived and
cleaning them out once the PRs are closed and a grace period has expired.

Summarized libelektra's job purpose is to combine the where
(our Git repository), with a when (tracking changes via polling or webhooks)
with a how (pointing to the Jenkinsfile + configuration).

### Jenkins Shared Library

We use a [shared library](https://www.jenkins.io/doc/book/pipeline/shared-libraries/) to share common functionalities across multiple pipelines.
This shared library contains all docker related functionalities, code to publish artifacts and various other directives and helper functions.
After integrating this shared library with:

```groovy
@Library('libelektra-shared') _

pipelineConfig {
  now = new Date()
}
```

we can use any function that is declared in the shared library.

It is available in the [following repository](https://github.com/ElektraInitiative/jenkins-library).
For more detailed usage instruction or instructions on how to integrate this shared library into Jenkins, see its [README.md](https://github.com/ElektraInitiative/jenkins-library/blob/master/README.md).

### Jenkinsfiles

Jenkinsfiles describe what actions the build system should execute on which
build slave.
Currently Elektra uses four different files.

#### Jenkinsfile.daily

- [Jenkinsfile.daily](/scripts/jenkins/Jenkinsfile.daily) contains daily maintenance tasks, like cleaning up build servers.
- [Buildjob: libelektra-daily](https://build.libelektra.org/job/libelektra-daily/)
- [Jenkinsfile.daily](https://master.libelektra.org/scripts/jenkins/Jenkinsfile.daily)

#### Jenkinsfile.monthly

- [Jenkinsfile.monthly](/scripts/jenkins/Jenkinsfile.monthly) contains monthly triggering of libelektra job on master.
- [Buildjob: libelektra-monthly](https://build.libelektra.org/job/libelektra-monthly/)
- [Jenkinsfile.monthly](https://master.libelektra.org/scripts/jenkins/Jenkinsfile.monthly)

#### Jenkinsfile.release

- Triggered manually with optional arguments. It is used for automated releasing a new version of Elektra.
- [Jenkinsfile.release](/scripts/jenkins/Jenkinsfile.release) contains description how to release a new version of Elektra.
- [Buildjob: libelektra-release](https://build.libelektra.org/job/libelektra-release/)
- [Jenkinsfile.release](https://master.libelektra.org/scripts/jenkins/Jenkinsfile.release)

#### Jenkinsfile

- Triggered on code changes and is for testing changes to the codebase.
- [Jenkinsfile](/scripts/jenkins/Jenkinsfile) contains descriptions how to build, test and deploy Elektra.
- [Buildjob: libelektra](https://build.libelektra.org/job/libelektra/)
- [Jenkinsfile](https://master.libelektra.org/scripts/jenkins/Jenkinsfile)

#### DSL

The language used is a groovy based DSL described in the
[Jenkinsfile book](https://jenkins.io/doc/book/pipeline/jenkinsfile/).
Most groovy syntax is allowed to describe pipelines in a Jenkinsfile, but it
is executed in a sandbox which might block certain calls.

Since plugins might extend the pool of available commands or variables a full
list of currently available syntax can be seen in
[pipeline
syntax](https://build.libelektra.org/job/libelektra/pipeline-syntax/)
after a login to the build server.
Some functionality is not covered by this page when the responsible plugin is
not implementing it.
Usually an approach similar to what is described on
[stackoverflow](https://stackoverflow.com/questions/51103359/jenkins-pipeline-return-value-of-build-step)
can be used to track down the responsible code inside the providing plugins
source code.

We also provide a number of helper functions in our Jenkinsfiles that are
documented at the function head.
Most common use cases, for example adding a new build with certain CMake flags,
are easy to add because of them.
For example, the configuration that is responsible for the `debian-stable-full`
stage is generated completely by a single helper function called `buildAndTest`:

```groovy
tasks << buildAndTest(
  "debian-stable-full",
  DOCKER_IMAGES.stretch,
  CMAKE_FLAGS_BUILD_ALL +
    CMAKE_FLAGS_BUILD_FULL +
    CMAKE_FLAGS_BUILD_STATIC +
    CMAKE_FLAGS_COVERAGE
  ,
  [TEST.ALL, TEST.MEM, TEST.NOKDB, TEST.INSTALL]
)
```

When adding new stages to the build chain it is generally a good idea to look up
an existing stage which does something similar and adapt it to the new use case.

### Security

Since a malicious PR could easily destroy the build server and slaves or expose
credentials some restrictions are introduced.
Only PR authors that have the right to push to libelektra can modify the
Jenkinsfile and have those changes be respected for the respective branch.

### Test Environments

We use Docker containers to provide the various test environments.

They are described
[in the repository](https://master.libelektra.org/scripts/docker)
and the Jenkinsfile describes how to build them.
If a rebuild of the images is needed is determined by the hash of the
Dockerfile used to describe it.
If it has not changed the build step will be skipped.
In the case that an image needed a build it will afterwards be uploaded into a
private Docker image registry (on a7) and thus is shared between all Docker
capable build slaves.

## Jenkinsfile (Main CI Pipeline)

### Tests

We will use the Docker images build as described earlier to run compilations
and tests for Elektra.
This allows us to run tests independent of which nodes are available (as the
environment is portable).

The Jenkinsfile describes the steps used to run tests.
Helper functions for easily adding new tests are available
(`buildAndTest`, `BuildAndTestAsan`, ...).

The `withDockerEnv` helper, that is available in the shared library,
makes sure to print the following information at the start of a test branch:

- branch name
- build machine
- docker image id

Coverage reports are generated automatically when using the `buildAndTest` helper
and the appropriate CMake flags for coverage generation have been set. They are
uploaded to https://doc.libelektra.org/coverage/.

Artifacts from `ctest` are also preserved automatically when using
`buildAndTest`.
The function also takes care of providing a stage name based path so multiple tests
can not overwrite files that share the same name.

Tests are executed in order dictated by the Jenkinsfile.
In general new tests should be added to the 'full build stage' that will only
run after a standard full test run succeeded.
This saves execution time on the build server for the most common errors.

Since there is no strict way to enforce the way tests are added we encourage
you to read the existing configuration and modify existing tests so they suite
your needs.

### Deployment

For runs of the build job that are run in the master branch we also execute
deployment steps after all tests pass.
We use these steps to build Debian and Fedora packages and move them into the repositories
hosted on the community node.

Additionally we recompile the homepage and deploy it on the community node.

## Jenkinsfile.release (Release Pipeline)

### Release Builds

The `buildRelease` function runs various test suites and collects and archives debug
information. It also generates source-packages and packages for Debian based
distributions and Fedora.

The generated packages are additionally installed and tested on a clean
Docker image.

### Manual approval

In order to verify the correct behavior and results of the previous steps and their
generated artifacts, we use the [Input Step](https://www.jenkins.io/doc/pipeline/steps/pipeline-input-step/)
to pause the pipeline.
After manually verifying the correctness, the pipeline run can be resumed.

### Publishing

The previously generated packages are published to the repositories, as described
above in _Jenkinsfile (Main CI Pipeline)_.
The API documentation and source packages get published to the relevant Git repositories.
Finally the Alpine release image is published to Docker Hub.

## Jenkins Setup

This section describes how to replicate the current Jenkins configuration.

### Jenkins libelektra Configuration

The `libelektra` build job is a multibranch pipeline job.
It is easiest to add via the BlueOcean interface.

Most of the default settings should be ok, however some settings need to be
verified or added to build Elektra correctly:

- `Advanced clone behaviors` should be added and the path to the Git mirror
  needs to be specified: `/home/jenkins/git_mirrors/libelektra`.
  This reference repository is created and maintained by our
  [daily buildjob](https://build.libelektra.org/job/libelektra-daily/).
- Under Property strategy you can add `Trigger build on pull request comment`.
  `jenkins build (libelektra|all) please` is a good starting point.
  This functionality is provided by the
  [GitHub PR Comment Build Plugin](https://wiki.jenkins-ci.org/display/JENKINS/GitHub+PR+Comment+Build+Plugin).
- For Build Configuration you want to specify `by Jenkinsfile` and add the
  script path: `scripts/jenkins/Jenkinsfile`.

### Jenkins libelektra-release Configuration

The `libelektra-release` job is a normal pipeline job.

Most of the default settings can be used.

- Under General you need to enable `Permission to Copy Artifact` and set the field
  to `*`.
- For Build Configuration you want to specify `by Jenkinsfile` and add the
  script path: `scripts/jenkins/Jenkinsfile.release`.

### General Set-Up of a New Pipeline

We will cover how to set-up a pipeline that uses a different Git repository than
this `libelektra` repository.

Most of the default settings can be used.

- If a private repository were to be used with a GitHub type job,
  a credentials pair of `elektrabot` must be selected to access this repository.
- For Build Configuration you want to specify `by Jenkinsfile` and add the
  script path to the Jenkinsfile.

We first start by including the shared library as described above in _Jenkins Shared Library_.
Then we declare in a Jenkinsfile an empty map that will contain all Docker images we will use
in this pipeline:

```groovy
DOCKER_IMAGES = [:]
```

This map will later be populated in a function.
For a Docker image with Ubuntu Focal and the Dockerfile located at `./ci/Dockerfile`
this function would need to look as follows:

```groovy
def dockerInit() {
  node('master') {
    checkout scm

    DOCKER_IMAGES.focal = dockerUtils.createDockerImageDesc(
      'ubuntu-focal', dockerUtils.&idTesting,
      './ci/',
      './ci/Dockerfile'
    )
    // more docker images ...
  }
}
```

We can now begin to create the stages that initialize, pull and if necessary
also build these Docker images:

```groovy
stage('Init docker images') {
  dockerInit()
}

stage('Pull docker images') {
  parallel dockerUtils.generateDockerPullStages(DOCKER_IMAGES)
}

maybeStage('Build docker images', DOCKER_IMAGES.any { img -> !img.value.exists }) {
  lock('docker-images') {
        parallel dockerUtils.generateDockerBuildStages(DOCKER_IMAGES)
  }
}
```

Now the actual stages that contain our CI/CD code can be created.

To allow parallel execution of stages we create generator functions that return
a map of stages that can be executed in parallel.
For example the following generator function would create a map of two tasks that
could be run in parallel.

```groovy
def generateTestStages() {
  def tasks = [:]

  tasks << test(
    'ubuntu-focal-test',
    DOCKER_IMAGES.focal
  )

  tasks << test(
    'ubuntu-bionic-test',
    DOCKER_IMAGES.bionic
  )

  return tasks
}
```

Suppose we have a bash script `test.sh` in the root of our Git repository.
This script can be executed inside one of our Docker images by using the
`withDockerEnv` function:

```groovy
def test(stageName, image) {
  return [(stageName): {
    stage(stageName) {
      withDockerEnv(image) {
        sh "./test.sh"
      }
    }
  }]
}
```

We can now call the generator function `generateTestStages` in a new stage:

```groovy
stage('Testing') {
  parallel generateTestStages()
}
```

### Adding a Jenkins Node

A node needs to have a JRE (Java Runtime Environment) installed.
Further it should run an SSH (Secure SHell) server.
Docker need to be installed as well.

A `jenkins` user with 47110:47110 ids should be created as this is what is
expected in Docker images.
`useradd -u 47110 jenkins`
Additionally a public key authentication should be set up so the jenkins
master can establish an SSH connection with the node.
If the node should be able to interact with Docker the jenkins user should be
added to the `docker` group.

Nodes should be set to only build jobs matching their labels and to be online
as much as possible.
As for labels `gitmirror` should be if you want to cache repositories on this
node.
If Docker is available the `docker` label should be set.

All files and folders in the Node under `/home/jenkins` should be owned by user `jenkins`.

## Understanding Jenkins Output

Our Jenkins build uses parallel steps inside a single build job to do most of
the work.
To reliably determine which stages failed it is best to look over the build
results in the Jenkins Blue Ocean view.
It is the default View opened when accessing the build results from GitHub.
For libelektra the URLs are
https://build.libelektra.org/job/libelektra/ and
https://build.libelektra.org/blue/organizations/jenkins/libelektra/branches/
.

Failed stages are marked in red.
On selecting a stage you can further expand all steps in the stage.
Of interest should be one of the first steps which echos out the Docker image
used to run the stage.
You also want to look for whatever failed (which should be in a step also marked
red to indicate failure).

## Reproducing Build Server Errors Locally

First you have to determine which image is used.
This is described above in _Understanding Jenkins output_.

Afterwards you can download it from our registry via `docker pull`.
Pay attention that you have to use **hub-public.libelektra.org** as this subdomain
does not require authentication for GET operations used by the Docker client.
As an example:

```sh
docker pull hub-public.libelektra.org/build-elektra-alpine:201809-791f9f388cbdff0db544e02277c882ad6e8220fe280cda67e6ea6358767a065e
```

You can also rebuild the images locally, which is useful if you want to test changes
you made to the Dockerfiles themselves.
Locate which Dockerfile you need by looking up the reference the stage that used it in the Jenkinsfile.
For _alpine_ this would be `DOCKER_IMAGES.alpine`.
You can search for this entry in the Jenkinsfile to find that this image is build from the
context `./scripts/docker/alpine/*` and uses `./scripts/docker/alpine/*/Dockerfile` as a
Dockerfile.
Now you can build the image as described in
[scripts/docker/README.md](https://master.libelektra.org/scripts/docker/README.md#building-images-locally).

You can find more information on how to use our images in
[scripts/docker/README.md](https://master.libelektra.org/scripts/docker/README.md#testing-elektra-via-docker-images).

Alternatively, you can follow the [tutorial](/doc/tutorials/run_all_tests_with_docker.md).

## Modify Test Environments

You can also modify the test environments (update a dependency, install a new
dependency, ...) by editing the Dockerfiles checked into SCM.
Determine which ones you need to modify by tracing which images are used for
what stages via the `DOCKER_IMAGES` map used in our Jenkinsfile.
The `dockerInit` function should be a good start point.

Following docker best practises you should try to minimize layer count
when possible.
Our docker images are quite large and hence take a long time to build so make
sure to test any modifications locally first.

## Triggers

All Triggers are described in the configuration of the respective build jobs.

The [libelektra](https://build.libelektra.org/job/libelektra/)
build is triggered for all branches of the libelektra repository except for
`debian`.
Additionally, all open branches in forks targeting libelektra's repository via
PRs are going to be build.
Pushes to any of those branches will trigger a new build automatically.

The
[daily build](https://build.libelektra.org/job/libelektra-daily/) and [monthly build](https://build.libelektra.org/job/libelektra-monthly/)
are executed according to a cron schedule.

The
[release pipeline](https://build.libelektra.org/job/libelektra-release/)
is triggered manually with optional arguments.

The following phrases can be used as comments to manually trigger a specific
build:

- jenkins build [libelektra](https://build.libelektra.org/job/libelektra/) please
- jenkins build [daily](https://build.libelektra.org/job/libelektra-daily/) please
- jenkins build [monthly](https://build.libelektra.org/job/libelektra-monthly/) please

Additionally `jenkins build all please` can be used to trigger all build jobs
relevant for PR's.
This is not necessary anymore as all relevant tests have been moved into the libelektra job.

## Authorization

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

## Issues with the Build Environment

If you have issues that are related to the build system you can open a normal
issue and tag it with `build` and `question`.
If you feel like your inquiry does not warrant an issue on its own, please use
[our buildserver issue](https://issues.libelektra.org/160).
