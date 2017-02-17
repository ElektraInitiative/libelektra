# Ideas for Contributing to Elektra

This page is the central point for students interested in contributing to Elektra during
[Google Summer of Code 2017](https://summerofcode.withgoogle.com).

Google Summer of Code is a opportunity for you to get paid while working on free software
initiatives during summer:

- [Timeline is available here.](https://developers.google.com/open-source/gsoc/timeline)
- [Flyers can be found here.](https://developers.google.com/open-source/gsoc/resources/media)

> Disclaimer: We do not yet know if we will participate!


## Which GSoC projects are available?

We think that GSoC is a perfect opportunity to bridge Elektra and other free
software projects. So the ideal GSoC project is to elektrify some free software.

Elektra may be used for applications (frameworks) like KDE, GNOME,
and LXCE or for any applications that need to read configuration files,
access environment variables, or command-line parameters.

Your task is to integrate Application (Framework) into Elektra.

Elektrify means:

- Patch the application so that it uses Elektra as its configuration system.
- Write a specification that describes how the configuration of the application
  looks like.

If you are interested in directly improving Elektra, other projects might be of
more interest to you:

- Currently, plugins cannot be arbitrarily nested and it is not easy to introduce
  completely different forms of backends (not using configuration files).
- There are further ways to explore how configuration can be validated.
- Further configuration file formats could be supported.
- The qt-gui could be improved by integrating validation.
- Your own ideas?


## What are the requirements to participate as student?

Needed skills:

- Fluent in English or German
- Good writing skills in English
- Profound programming skills with focus on good code quality, documentation and unit tests.
- Ideally experience with an application (framework) or even author of a FLOSS application


## How can I participate as student?

First you should familiarize with the Elektra Initiative:

- If you did not yet look at the [home page](https://www.libelektra.org/) please do so!
- Look into the [contributing guidelines](/.github/CONTRIBUTING.md)
- Look into the [issue tracker](https://issues.libelektra.org/) and pick an easy task.
- Say hello in the thread of the task and that you are interested in participating
  GSoC 2017 and that you want to solve this issue.
- In a timely fashion, create a PR which solves the task as good as you can.
  We will give you further feedback how you can improve.

Based on the quality of the PR(s) and the communication you have a good chance
to participate in GSoC.


## How can I participate as mentor?

In any case we will contact applications that should be elektrified and will
discuss requirements. Furthermore, we will make sure that students will submit
elektrification patches at a very early stage to make sure that there is enough
time for rework.

That said, we would love to have co-mentors from other organizations so that the GSoC projects
can be more tuned towards the application that should be elektrified.

