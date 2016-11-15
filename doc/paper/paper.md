---
title: 'Elektra: universal framework to access configuration parameters'
tags:
  - configuration
  - context awareness
  - configuration files
  - interception
  - integration
authors:
 - name: Markus Raab
   orcid: 0000-0002-1493-9065
   affiliation: TU Wien (TUW)
date: 23 July 2016
bibliography: paper.bib
---

# Summary

Applications today typically invent their own configuration systems to dynamically load their configuration (files) at run-time.
Up to now, it was difficult to integrate and specify the configuration of such applications.
Elektra [@elektra] provides a framework to bridge this gap [@raab2010thesis].

While the documentation covers how developers and administrators can use Elektra, in this paper we will explain
its research value.

The research value of Elektra is two-fold:
First Elektra allows us to intercept unmodified applications (by ''hijacking'' calls to their configuration system)  [@raab2016unanticipated] and extend their configuration access with the features Elektra has [@raab2015kps].
Second Elektra provides a plugin framework [@raab2016improving] where researchers can experiment with new configuration features.
Elektra allows us to directly apply novel ideas on many existing applications for case studies.
Furthermore a code generator [@raab2015safe] simplifies writing new applications [@raab2014program].

# References
