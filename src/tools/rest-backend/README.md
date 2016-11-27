# Elektra REST Backend #

## Introduction ##

This document aims to provide information about Elektra's `rest-backend` feature.
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

## Run and Configure ##

To run the `@tool@` we need to find out where it has been installed to.
This can be done by running the command `kdb list-tools` which will output a list of
installed tools and something like `External tools are located in /usr/local/lib/elektra/tool_exec`.
With this path we can run the service like `cd /usr/local/lib/elektra/tool_exec/ && sh run-@tool@`.
An alternative is to make use of the `kdb` tool and run `kdb run-@tool@`.

The REST service can also be configured. All configuration is read from Elektras
key database at start-time of the service. Further details and configuration options
are listed below.

To stop the service, run `sh stop-@tool@` in the directory where the start script is located
or `kdb stop-@tool@` from anywhere.

### Configuration ###

The service stores all its configuration below `/sw/elektra/@tool@`,
which is split into two parts:

- one for CppCMS below `/sw/elektra/@tool@/cppcms` and
- one for the service itself below `/sw/elektra/@tool@/backend`

#### CppCMS ####

All configuration options for CppCMS are listed on their [Website](http://cppcms.com/wikipp/en/page/cppcms_1x_config).
The JSON configuration explained on the website can be translated into Elektra keys easily.
For the values `true` and `false`, the strings `"true"` and `"false"` can be used.

The following example configuration in CppCMS style
```
{
    "service": {
        "api": "http"
        "port": 8080,
        "ip": "0.0.0.0"
    },
    "security": {
        "display_error_messages": true
    }
}
```
can be realized within Elektra like
```
kdb set /sw/elektra/@tool@/cppcms/service/api "http"
kdb set /sw/elektra/@tool@/cppcms/service/port 8080
kdb set /sw/elektra/@tool@/cppcms/service/ip "0.0.0.0"
kdb set /sw/elektra/@tool@/cppcms/security/display_error_messages "true"
```

Simply set the desired settings as keys in the key database and you are done!

#### Backend ####

The service itself offers quite some configuration options as well.
In detail, the options (without the base key `/sw/elektra/@tool@/backend`) are:

```
[api_specification/raw]
check/type = string
description = A link to the blueprint describing the API.
example = https://doc.libelektra.org/api/snippet-sharing.md

[api_specification/html]
check/type = string
description = A link to the compiled blueprint describing the API.
example = https://doc.libelektra.org/api/snippet-sharing.html

[jwt/encryption_key]
check/type = string
description = A secret string used to encrypt session tokens (JWT).
example = al3h120d8a_19s

[jwt/expiration_time]
check/type = int
description = The number of seconds a JWT is valid from its creation.
example = 3600
default = 7200

[kdb/path/configs]
check/type = string
description = The root path being used to store configuration snippet entries, must start with a namespace.
example = user/@tool@/configs
default = dir/configs

[kdb/path/users]
check/type = string
description = The root path being used to store user entries, must start with a namespace.
example = user/@tool@/users
default = dir/users

[output/default/entry/sort]
check/enum = 'asc', 'desc'
description = The default sort direction being used for requests against configuration snippet entry resources.
default = 'asc'

[output/default/entry/sortby]
check/enum = 'key', 'organization', 'application', 'scope', 'slug', 'title', 'author', 'created_at'
description = The default sort criteria being used for requests against configuration snippet entry resources.
default = 'key'

[output/default/entry/filterby]
check/enum = 'all', 'key', 'title', 'description', 'author', 'tags'
description = The default filter criteria being used for requests against configuration snippet entry resources.
default = 'all'

[output/default/user/sort]
check/enum  = 'asc', 'desc'
description = The default sort direction being used for requests against user entry resources.
default = 'asc'

[output/default/user/sortby]
check/enum = 'username', 'email', 'created_at'
description = The default sort criteria being used for requests against user entry resources.
default = 'username'

[output/default/user/filterby]
check/enum = 'all', 'username', 'email'
description = The default filter criteria being used for requests against user entry resources.
default = 'all'

[permissions/entry/create]
check/type = int
check/type/min = 0
check/type/max = 100
description = The required rank a user needs to be able to create new configuration snippet entries.
default = 10

[permissions/entry/edit]
check/type = int
check/type/min = 0
check/type/max = 100
description = The required rank a user needs to be able to edit any configuration snippet entry (also from other users).
default = 50

[permissions/entry/delete]
check/type = int
check/type/min = 0
check/type/max = 100
description = The required rank a user needs to be able to delete any configuration snippet entry (also from other users).
default = 50

[permissions/user/view
check/type = int
check/type/min = 0
check/type/max = 100
description = The required rank a user needs to be able to view account details of other users.
default = 100

[permissions/user/edit]
check/type = int
check/type/min = 0
check/type/max = 100
description = The required rank a user needs to be able to edit account details of other users.
default = 100

[permissions/user/delete]
check/type = int
check/type/min = 0
check/type/max = 100
description = The required rank a user needs to be able to delete user accounts of other users.
default = 100
```

### Configure as service ###

To configure the rest-backend as service, it is possible to use `systemd` on most systems.

1) Create a new service file with the following command
(and make sure the paths of `ExecStart` match your installation of Elektra):
```
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
2) Reload the configuration of `systemctl` with `systemctl daemon-reload`.
3) Enable the rest-backend service with `systemctl enable @tool@.service`, a symlink should be created.
4) Make sure the service is enabled with `systemctl is-enabled @tool@.service`.
5) Restart the rest-backend service with `systemctl restart @tool@.service`.
If everything went fine, the service should be reachable and `systemctl status @tool@.service`
should print information about the running service (PID, etc).

## Compiling and Installation ##

### Dependencies ###

In order to compile and use the new `@tool@` there are a few dependencies which must be installed. 

- CppCMS version 1.0.0 or higher
- Boost version 1.45 or higher
- LibJWT version 1.5 or higher
- OpenSSL

#### CppCMS ####

The CppCMS requires following dependencies:

- Boost library
- Zlib library
- PCRE library
- Python 2.4 or higher, but not python 3

To install them via `apt` use:
`sudo apt-get install libboost-all-dev libpcre3-dev zlib1g-dev libgcrypt11-dev libicu-dev python`

To install CppCMS, there are two options:

- Download and build from source
- Install via dependency manager

To install CppCMS manually without dependency manager:
- Download (latest) CppCMS from  [SourceForge](https://sourceforge.net/projects/cppcms/files/cppcms/)
- Extract CppCMS: `tar -xjf cppcms-1.x.x.tar.bz2 && cd cppcms-1.x.x` (replace 1.x.x with your version)
- Configure the build: `mkdir build && cd build && cmake ..`
- Execute the build and run tests: `make && make test && make install`

Further build and installation information can be found on their
[website](http://cppcms.com/wikipp/en/page/cppcms_1x_build).
It contains also a [guide](http://cppcms.com/wikipp/en/page/apt)
explaining the installation through a dependency manager.

### Compiling ###

Compile Elektra as normal as per the [COMPILE document](http://libelektra.org/tree/master/doc/COMPILE.md),
but make sure to include the `rest-backend` tool using the `-DTOOLS` flag.

For instance:
`-DTOOLS=ALL` or `-DTOOLS=@tool@`

### Installing ###

You can now install Elektra as you normally would or as described
in the [install documentation](http://libelektra.org/tree/master/doc/INSTALL.md).

## Implementation notes and hints for Front-Ends ##

The here described tool offers an API which can be consumed by either a command line tool
like cURL or a custom front-end. In the following some hints for front-end implementations will be given.

### Usability ###

The API validates all inputs, but does not respond always with exact error messages.
Normally error messages contain a general hint on what input was wrong
(e.g. 'the `username` has to be 3-20 signs long, contain only letters, digits and dashes'),
but not what particular constraint was wrong for the last input
(e.g. that the input was only 2 instead of 3 signs long).
This limitation comes from the usage of regex patterns instead of atomic comparisons during validation.

In terms of usability this is sufficient, but not the best possible.
Therefore it would be advisable to implement live-validation for front-ends with more granularity.
Information about allowed input formats can be found in the
[API description](http://libelektra.org/tree/master/doc/rest_api/snippet_sharing/api-description.apib).

## Benchmarks ##

The service has been benchmarked against a MySQL solution, for further details see [benchmarks readme](benchmarks/README.md).
