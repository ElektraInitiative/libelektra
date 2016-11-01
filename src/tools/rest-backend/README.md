# Elektra REST Backend #

## Introduction ##

This document aims to provide information about Elektra's `rest-backend` feature. `rest-backend` offers a RESTful server to search, store and convert configuration snippets. It also offers authentication methods to restrict access to manipulative methods. A detailed description of the implemented API can be found in the [API description](/doc/api_blueprints/snippet-sharing.apib).

The REST service operates on a at compile time defined repository (path). This path is not meant to be used in other tools or applications, as the service caches all data in-memory as well. Changes to the repository can therefore result in unexpected and undefined behavior.

The reason why a special repository is used to store and retrieve entries is that opening the system to everyone by providing a public interface to the whole key database would result in a security leak. For a REST API with such an interface, another tool will be published.

 The REST service will leave some space for configuration, as not every deployment will look the same.

## Compiling and Installation ##

### Dependencies ###

In order to compile and use the new `rest-backend` there are a few dependencies which must be installed. 

- CppCMS version 1.0.0 or higher
- Boost version 1.45 or higher

`CppCMS` itself requires following dependencies:

- Zlib library
- PCRE library
- Python 2.4 or higher, but not python 3

I was able to install the correct dependencies on my system, running ubuntu 15.04, using the following steps (it is assumed that CMake is already installed):
- Install base dependencies: `sudo apt-get install libboost-all-dev libpcre3-dev zlib1g-dev libgcrypt11-dev libicu-dev python`
- Download (latest) CppCMS from  [SourceForge](https://sourceforge.net/projects/cppcms/files/cppcms/)
- Extract CppCMS: `tar -xjf cppcms-1.x.x.tar.bz2 && cd cppcms-1.x.x` (replace 1.x.x with your version)
- Configure the build: `mkdir build && cd build && cmake ..`
- Execute the build and run tests: `make && make test && make install`

### Compiling ###

Compile Elektra as normal as per the [COMPILE document](http://libelektra.org/tree/master/doc/COMPILE.md) making sure to include the `rest-backend` tool using the `-DTOOLS` flag.

For instance:
`-DTOOLS=ALL` or `-DTOOLS=rest-backend`

Following options are available at compile time:
- `REST_REPOSITORY_CONFIGS` representing a path to the location where configuration snippets should be stored. An example would be `-DREST_REPOSITORY_CONFIGS=dir/configs`. The default value is `dir/configs`.
- `REST_REPOSITORY_USERS` representing a path to the location where user information should be stored. An example would be `-DREST_REPOSITORY_USERS=user/rest/service/users`. The default value is `dir/users`.
- `REST_AUTH_ENCRYPT_KEY` holding an encryption key to be used to encrypt authentication tokens. This option is mandatory because the default value is not secure. It can be a simple non-empty string like `a_5ecure-encrypt1on_key`.
- `REST_JWT_ISSUER` holding the name of the token issuer. This value should also be kept secret as it serves as verification detail. It can be a simple non-empty string like `my_issuer531`.

### Installing ###

You can now install Elektra as you normally would or as described in the [install documentation](http://libelektra.org/tree/master/doc/INSTALL.md).

## Run and Configure ##

To run the `rest-backend` we need to find out where it has been installed to. This can be done by running the command `kdb list-tools` which will output a list of installed tools and something like `External tools are located in /usr/local/lib/elektra/tool_exec`. With this path we can run the service like:
`cd /usr/local/lib/elektra/tool_exec/ && sh run-rest-backend.sh`

The REST service can also be configured. The file `rest-backend-config.js` in the same path contains informations about where the service should listen to. Detailed information about how to configure the REST service can be found on the [CppCMS website](http://cppcms.com/wikipp/en/page/cppcms_1x_config).

To stop the service, run `sh stop-rest-backend.sh` in the same path.

### Configure as service

To configure the rest-backend as service, it is possible to use `systemd` on most systems.

1) Create a new service file with the following command (and make sure the paths of `ExecStart` match your installation of Elektra):
```
cat > /etc/systemd/system/rest-backend.service << EOF
[Unit]
Description=Start the REST-backend for sharing of configuration snippets
Requires=network.target
After=network.target

[Service]
Type=simple
Restart=always
ExecStart=/usr/local/lib/elektra/tool_exec/rest-backend -c /usr/local/lib/elektra/tool_exec/rest-backend-config.js

[Install]
WantedBy=multi-user.target
EOF
```
2) Reload the configuration of `systemctl` with `systemctl daemon-reload`.
3) Enable the rest-backend service with `systemctl enable rest-backend.service`, a symlink should be created.
4) Make sure the service is enabled with `systemctl is-enabled rest-backend.service`.
5) Restart the rest-backend service with `systemctl restart rest-backend.service`. If everything went fine, the service should be reachable and `systemctl status rest-backend.service` should print information about the running service (PID, etc).

## Implementation notes and hints for Front-Ends

The here described tool offers an API which can be consumed by either a command line tool like cURL or a custom front-end. In the following some hints for front-end implementations will be given.

### Usability

The API validates all inputs, but does not respond always with exact error messages. Normally error messages contain a general hint on what input was wrong (e.g. 'the `username` has to be 3-20 signs long, contain only letters, digits and dashes'), but not what particular constraint was wrong for the last input (e.g. that the input was only 2 instead of 3 signs long). This limitation comes from the usage of regex patterns instead of atomic comparisons during validation.

In terms of usability this is sufficient, but not the best possible. Therefore it would be advisable to implement live-validation for front-ends with more granularity. Information about allowed input formats can be found in the [API description](http://libelektra.org/tree/master/doc/rest_api/snippet_sharing/api-description.apib).



