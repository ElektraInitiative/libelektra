---
title: "Elektra: universal framework to access configuration parameters"
tags:
  - configuration
  - context awareness
  - configuration files
  - interception
  - integration
authors:
  - name: Markus Raab
    orcid: 0000-0002-1493-9065
    affiliation: 1
affiliations:
  - name: TU Wien (TUW)
    index: 1
date: 23 July 2016
bibliography: paper.bib
---

# Summary

Today, most software invent their own configuration systems to dynamically load their configuration files at run-time.
Up to now, it was difficult to integrate and specify the configuration across such applications.
Elektra [@elektra] provides a framework to bridge this gap [@raab2010thesis].
In its essence, Elektra can be thought of a database that uses standard configuration files to persist data.
Furthermore, a simple configuration specification language can be used to describe the data and its access to it.

Because of its generic nature, we cannot give an exhaustive list of what can be done with Elektra.
The obvious cases, how developers and administrators should use Elektra to avoid configuration integration issues mentioned earlier, are described in Elektra’s documentation.
But it is ongoing research to find further use cases where these abstractions are useful.
In this paper, we will give three concrete examples where Elektra has value to the research community.

## Context Awareness

Currently, applications sometimes modify configuration values before using them.
The reasons for such modifications can be called context, e.g., the number of CPUs, the current operating system, or the battery status [raab2016persistent].
The modifications within applications are problematic because it is not transparent for the user which configuration values the application actually will use.

We propose to move the logic that is responsible for determining configuration values into Elektra’s specification language [@raab2015kps][@raab2016improving] [@raab2016unanticipated].
This way, the user can query the up-to-date configuration values and get identical results to what the application will see.
But even better, users can change the way context is taken into account easily.

Elektra allows us to intercept unmodified applications (by ''hijacking'' calls to their configuration system) [@raab2016unanticipated].
For example, an application calls the C-function getenv() but actually retrieves a value from Elektra and not from an environment variable.
This way we can make applications context aware that previously were not.

## Validation

Developers often do not provide a way to validate configuration files [@raab2015safe].
So administrators are forced to start applications to see if the configuration file is rejected.

We propose to move the validation from the applications to Elektra’s specification language [@raab2016improving].
Then every modification of the configuration files via Elektra gets automatically validated.
This can be via an editor, a graphical user interface, or a web interface.

Furthermore, based on the specification language, we can generate valid and invalid configuration files.
Such configuration files can be used to test the behavior of applications, e.g., injecting faulty configuration files to see if applications crash.

## Code Generator

Applications today often have hand-written glue code to transform the strings received from configuration files to the variables used in the application.
In Elektra a code generator [@raab2015safe] allows us to generate this code.
Based on the configuration specification, Elektra provides methods with type-safe access to configuration values.

This simplifies writing new applications [@raab2014program] because we can also generate documentation and code to parse command-line options.
But this technique can also be used to replace existing hand-written code.

# References
