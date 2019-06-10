# Elektra REST Backend

## Introduction

> Note: If you need a REST Service that provides get and set of
> local configuration, you actually want [/src/tools/web/elektrad]

This document aims to provide information about Elektra’s `rest-backend` feature.
`rest-backend` offers a RESTful server to search, store and convert configuration snippets.
It also offers authentication methods to restrict access to manipulative methods.
A detailed description of the implemented API can be found in the
[API description](/doc/api_blueprints/snippet-sharing.apib).

Stored are snippet and user entries, both below different paths.
Each entry consists of multiple keys (+ meta keys) belonging together,
while the root key matching a certain schema (regex) is used to identify an entry.

The REST service operates on user-defined repositories (paths).
This paths are not meant to be used in other tools or applications,
as the service caches all data in-memory as well.
Changes to the repositories can therefore result in unexpected and undefined behavior,
both for the service as well as the stored data.

The reason why a special repository is used to store and retrieve entries is
that opening the system to everyone by providing a public interface to the
whole key database would result in a security leak.
For a REST API with such an interface, another tool will be published.

## Run and Configure

To run the `@tool@` we need to find out where it has been installed to.
This can be done by running the command `kdb list-tools` which will output a list of
installed tools and something like `External tools are located in /usr/local/lib/elektra/tool_exec`.
With this path we can run the service like `cd /usr/local/lib/elektra/tool_exec/ && sh run-@tool@`.
An alternative is to make use of the `kdb` tool and run `kdb run-@tool@`.

The REST service can also be configured. All configuration is read from Elektra’s
key database at start-time of the service. Further details and configuration options
are listed below.

To stop the service, run `sh stop-@tool@` in the directory where the start script is located
or `kdb stop-@tool@` from anywhere.

### Configuration

The service stores all its configuration below `@config_root@@config_default_profile@`,
which is split into two parts:

- one for CppCMS below `@config_root@@config_default_profile@/cppcms` and
- one for the service itself below `@config_root@@config_default_profile@/backend`

#### CppCMS

All configuration options for CppCMS are listed on their [Website](http://cppcms.com/wikipp/en/page/cppcms_1x_config).
The JSON configuration explained on the website can be translated into Elektra keys easily.
For the values `true` and `false`, the strings `"true"` and `"false"` can be used.

The following example configuration in CppCMS style

```json
{
  "service": {
    "api": "http",
    "port": 8080,
    "ip": "0.0.0.0"
  },
  "security": {
    "display_error_messages": true
  }
}
```

can be realized within Elektra like

```sh
kdb set @config_root@@config_default_profile@/cppcms/service/api "http"
kdb set @config_root@@config_default_profile@/cppcms/service/port 8080
kdb set @config_root@@config_default_profile@/cppcms/service/ip "0.0.0.0"
kdb set @config_root@@config_default_profile@/cppcms/security/display_error_messages "true"
```

Simply set the desired settings as keys in the key database and you are done!

#### Backend

The service itself offers quite some configuration options as well.
In detail, the options (without the base key `@config_root@`) are:

```
@configuration-specification@
```

### Configure as Service

To configure the rest-backend as service, it is possible to use `systemd` on most systems.

1. Create a new service file with the following command
   (and make sure the paths of `ExecStart` match your installation of Elektra):

   ```sh
   cat > /etc/systemd/system/@tool@.service << EOF
   [Unit]
   Description=Start the REST backend for sharing of configuration snippets
   Requires=network.target
   After=network.target

   [Service]
   Type=simple
   Restart=always
   ExecStart=kdb run-@tool@
   ExecStop=kdb stop-@tool@

   [Install]
   WantedBy=multi-user.target
   EOF
   ```

2. Reload the configuration of `systemctl` with `systemctl daemon-reload`.
3. Enable the rest-backend service with `systemctl enable @tool@.service`, a symbolic link should be created.
4. Make sure the service is enabled with `systemctl is-enabled @tool@.service`.
5. Restart the rest-backend service with `systemctl restart @tool@.service`.
   If everything went fine, the service should be reachable and `systemctl status @tool@.service`
   should print information about the running service (PID, etc).

## Compiling and Installation

### Dependencies

In order to compile and use the new `@tool@` there are a few dependencies which must be installed.

- CppCMS version 1.0.0 or higher
- Boost version 1.45 or higher
- LibJWT version 1.5 or higher
- OpenSSL

An extensive tutorial describing the installation and configuration can be found
[here](/doc/tutorials/snippet-sharing-rest-service.md).

### Compiling

Compile Elektra as normal as per the [COMPILE document](https://master.libelektra.org/doc/COMPILE.md),
but make sure to include the `rest-backend` tool using the `-DTOOLS` flag.

For instance:
`-DTOOLS=ALL` or `-DTOOLS=@tool@`

### Installing

You can now install Elektra as you normally would or as described
in the [install documentation](https://master.libelektra.org/doc/INSTALL.md).

## Implementation Notes and Hints for Front-Ends

The here described tool offers an API which can be consumed by either a command line tool
like cURL or a custom frontend. In the following some hints for frontend implementations will be given.

### Usability

The API validates all inputs, but does not respond always with exact error messages.
Normally error messages contain a general hint on what input was wrong
(e.g. 'the `username` has to be 3-20 signs long, contain only letters, digits and dashes'),
but not what particular constraint was wrong for the last input
(e.g. that the input was only 2 instead of 3 signs long).
This limitation comes from the usage of regex patterns instead of atomic comparisons during validation.

In terms of usability this is sufficient, but not the best possible.
Therefore it would be advisable to implement live-validation for frontends with more granularity.
Information about allowed input formats can be found in the
[API description](https://master.libelektra.org/doc/api_blueprints/snippet-sharing.apib).

## Benchmarks

The service has been benchmarked against a MySQL solution, for further details see [benchmarks readme](benchmarks/README.md).
