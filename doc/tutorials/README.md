Elektra has many different aspects to explore.
Not all parts are needed by everyone.
In this document we classify which parts should
be read by whom.

## General Information

Read this first to get the basic concepts of Elektra.

- [Namespaces](namespaces.md)
- [Key names](/doc/KEYNAMES.md)
- [Cascading](cascading.md)
- [Arrays](arrays.md)
- [Mount Configuration Files](mount.md)

## Developers

For these tutorials we assume you want to elektrify your
application, that means, you want your application
to participate in the global key database Elektra
provides.

- [Hello, Elektra in C](hello-elektra.md)
- [Integration of your C Application](application-integration.md)
- [Writing a specification for your configuration](specification.md)
- [Meta specification language](/doc/METADATA.ini)
- [Plugins Introduction](plugins.md)
- [Storage Plugins](storage-plugins.md)
- [Compilation Variants of plugins](compilation-variants.md) (advanced topic)
- [High Level API](highlevel.md)
- [Command Line Options](command-line-options.md)
- [Python Bindings](python-kdb.md)
- [Java Bindings](java-kdb.md)
- [Java Plugins](java-plugins.md)
- [Ruby Bindings](/src/bindings/swig/ruby/README.md)
- [High Level API Bindings](highlevel-bindings.md)
- [Notifications](notifications.md)
- [Changetracking](changetracking.md)

## System Administrators

For these tutorials we assume that you want to
work with the configuration of applications
already somehow integrated with Elektra.

- [Import Configuration](import.md)
- [Export Configuration](export.md)
- [Intercept Environment](/src/bindings/intercept/env/README.md)
- [Intercept File System](/src/bindings/intercept/fs/README.md)
- [Merge Configuration](cmerge.md) (new version)
- [Merge Configuration](merge.md) (deprecated)
- [Validate Configuration](validation.md)
- [Encrypt Configuration](crypto.md)
- [Install Configuration Files](install-config-files.md)
- [Write a specification for dockerd](dockerd-specification.md)
- [Recording Changes](recording.md)
- [Configure Xfconf Applications](xfconf.md)

## Elektra Developers

These tutorials are for persons that want to contribute to
Elektra:

- [Contributing with CLion](contributing-clion.md)
- [Contributing with Visual Studio (Windows)](contributing-windows.md)
- [Run all Tests with Docker](run_all_tests_with_docker.md)
- [Run Reformatting with Docker](run_reformatting_script_with_docker.md)
- [Using Podman instead of Docker](using_podman_instead_of_docker.md)
- [Language Bindings](language-bindings.md)
- [Code Generator](code-generator.md)
- [Benchmarking](benchmarking.md)
- [Profiling](profiling.md)
- [Logging](logger.md)
- [Changetracking](changetracking.md)

## Installation Manuals

These tutorials provide additional information on how to
install and set up specific tools.

- [Webui](install-webui.md)
