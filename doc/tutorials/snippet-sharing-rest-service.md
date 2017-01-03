# REST Service for Sharing of Configuration Snippets #

This tutorial explains how to install, configure and run everything that is required
for the Snippet Sharing service and website. As operating system we expect Debian
Jessie to be used, although most parts should also be applicable to other systems.

## What is this? ##

The Snippet Sharing service consists of two parts, a backend (the REST server) and
a frontend (an AngularJS application executed in the browser), which also includes
the Elektra website. The service itself can be used to share configuration snippets,
i.e. configuration files of arbitrary applications.

## Installation ##

### Install Dependencies ###

As first step we need to install some dependencies required by the applications.

#### Backend ####

The backend requires the most dependencies and unfortunately some of them need to be
installed manually because no APT packages are available.

The required dependencies with their own dependencies are:

- CppCMS >= v1.0.0
    - Zlib library
    - PCRE library
    - Python >= v2.4 (but not v3)
    - ICU library >= v3.6
    - gcrypt or OpenSSL library
- Boost >= v1.45
- OpenSSL
- LibJWT >= v1.5
    - Jansson
    - OpenSSL

You'll also need CMake and a modern C/C++ compiler, so in case you don't have them
installed already, use `apt-get install build-essential cmake` to do so.

##### APT Packages #####

To install all required APT packages at once, simply run:
`apt-get install libboost-all-dev libpcre3-dev zlib1g-dev libgcrypt11-dev libicu-dev python libssl-dev`

##### CppCMS #####

To install CppCMS, there are two options available:

- Download and build from source
- Install via dependency manager

To install CppCMS manually without dependency manager:

- Download the (latest) CppCMS source from [SourceForge](https://sourceforge.net/projects/cppcms/files/cppcms/)
- Extract the archive: `tar -xjf cppcms-1.x.x.tar.bz2 && cd cppcms-1.x.x` (replace 1.x.x with your version)
- Configure the build: `mkdir build && cd build && cmake ..`
- Execute the build, run tests and install: `make && make test && make install`

Further build and installation information can be found on their
[website](http://cppcms.com/wikipp/en/page/cppcms_1x_build).
It contains also a [guide](http://cppcms.com/wikipp/en/page/apt)
explaining the installation through a dependency manager.
Unfortunately, no repository is available for Debian Jessie yet.

##### Jansson #####

The Jansson library supports working with JSON data in C.
To install it, use the following steps:

- Download the (latest) source from [their website](http://www.digip.org/jansson/releases/)
- Extract the archive: `bunzip2 -c jansson-2.x.tar.bz2 | tar xf - && cd jansson-2.x` (replace 2.x with your version)
- Configure the build: `mkdir build && cd build && cmake ..`
- Execute the build, run tests and install: `make && make check && make install`

##### LibJWT #####

Now we can also install LibJWT:

- Download the (latest) source from [GitHub](https://github.com/benmcollins/libjwt/releases)
- Extract the archive: `tar -xjf v1.x.x && cd libjwt-1.x.x` (replace 1.x.x with your version)
- Configure the build: `mkdir build && cd build && cmake ..`
- Execute the build and run tests: `make jwt && make jwt_static && make check`

If you are using Ubuntu, LibJWT can also be installed through a pre-built APT package:

- Add APT repository: `add-apt-repository ppa:ben-collins/libjwt`
- Install via apt-get: `apt-get update && apt-get install libjwt`

#### Frontend ####

The frontend does only require the package manager [npm](https://www.npmjs.com/) (>= v3).
It is preferred to install it along with [nodeJS](https://nodejs.org/),
for example via [provided packages](https://nodejs.org/en/download/package-manager/).
The version that comes via Debian Jessie (v1.4) is insufficient.
The installation was tested with npm version 3.10.8-9, but it should also work with
other versions of 3+.

### Build & Install the Applications ###

After installing the dependencies, we are ready to build the applications.
To do so, we can follow the steps explained in the [build guide](/doc/COMPILE.md).
Make sure to include the two tools `rest-backend` and `rest-frontend`, e.g. by
using the arguments `-DTOOLS="ALL;rest-backend;rest-frontend"`.

After building Elektra and the applications, we can use `make install` to install
them. Further information and troubleshooting can be found in the
[install guide](/doc/INSTALL.md).

## Configuration ##

The applications configure themselves as much as possible during build,
but some settings have to be set manually afterwards.

### Backend ###

After the installation, the configuration specification of the backend has to be mounted
with the command `kdb mount-rest-backend-config`. You can check if the mounting was successful
by issuing `kdb mount`. There should be an entry in the list with a path like
`spec/sw/elektra/restbackend/#0` and a similar one without the leading `spec`.
If you do not see this mountpoints, have a look at the mount script in the tool_exec
(which is defined during installation of Elektra). You can also run the commands manually.

To complete the mounting, you also need to ensure that the `spec` plugin is used as
global plugin. If you have not changed anything about the global plugins yet, you can simply
use `kdb global-mount` to ensure that the `spec` plugin is added as global plugin. If not,
you need to add all currently mounted global plugins to the command as they will be deleted
otherwise.

After that you need to set an additional configuration parameter that has no default value.
It is recommended to set it for the system namespace if you will use a tool like
`systemctl` to manage the services.

```sh
kdb set -N system /sw/elektra/restbackend/#0/current/backend/jwt/encryption/secret "use a secret key here"
```

To generate a secure key, you can also use `pwgen` (install via `apt-get install pwgen`). Use

```sh
kdb set -N system /sw/elektra/restbackend/#0/current/backend/jwt/encryption/secret "$(pwgen -1cns 30)"
```

to generate and set a strong random encryption secret.

The option `-N system` for `kdb set` defines the used namespace (in this case it is `system`).
In order for the configuration specification to work (e.g. validation), we have to use
cascading key names. This is why we have to define the namespace by using this option instead
of passing it as part of the key name.

If you want to know the default values, you can get a list of used keys with
`kdb ls /sw/elektra/restbackend/#0`. You can then retrieve the configuration value
for each key with `kdb get <key>`, e.g.,
`kdb get /sw/elektra/restbackend/#0/current/backend/jwt/encryption/secret`

Additionally to the settings above, CppCMS needs some configuration. All configuration
options are listed on [their website](http://cppcms.com/wikipp/en/page/cppcms_1x_config).
A stand-alone installation of the service (without proxy server) requires following
configuration:

```sh
kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/api "http"
kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/ip "0.0.0.0"
kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/port 8080
kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/http/script_names/#0 "/"
```

Note: here we have not used the option `-N system` because the CppCMS configuration is
not part of the specification. That means it does not get validated.

### Frontend ###

The frontend does only require small mandatory changes in its configuration.
Before they can be made, the configuration file has to be mounted though. This can be
achieved by issuing `kdb mount-rest-frontend-config`. The configuration should then
be available at `system/sw/elektra/restfrontend/#0/current`. To get a list of possible
configuration parameters, use `kdb ls system/sw/elektra/restfrontend/#0/current`.

Note that the frontend is not elektrified, only changes within
`system/sw/elektra/restfrontend/#0/current` will work.

The parameters that need to be changed in order for the frontend to work correctly, are:

- `system/sw/elektra/restfrontend/#0/current/backend/root`: set it to the URL where the backend will be reachable, e.g. `http://restapi.libelektra.org/` (with trailing slash!)
- `system/sw/elektra/restfrontend/#0/current/website/url`: set it to the URL where the frontend will be reachable, e.g. `http://libelektra.org/` (with trailing slash!)

## Running the Applications ##

As last step we need to run the applications:

- First we start the backend server with `kdb run-rest-backend`. To ensure the backend is accessible, you can use `curl http://localhost:8080/version` (change port to your setting), which should show you some version information in JSON format.
- Although the frontend was compiled during installation already, we want to have a freshly built homepage and use `kdb build-rest-frontend` to do so.
- Then we run the frontend analogously with `kdb run-rest-frontend`. It should now be reachable at the configured port.

If everything went smooth, both applications should now be online and reachable.

## Stopping the Applications ##

Both applications can be stopped with a simple command:

- Backend: `kdb stop-rest-backend`
- Frontend: `kdb stop-rest-frontend`

## Additional Tasks ##

### API Specification ###

For the backend a detailed description in the
[API blueprint](https://apiblueprint.org/) format is available.
To compile the [blueprint](/doc/api_blueprints/snippet-sharing.apib) we need
the [apiary-client](https://github.com/apiaryio/apiary-client) to be installed,
which in return is installed as Ruby gem:

- So we first install Ruby with gem: `apt-get install ruby`
- Then we install the apiary-cli: `gem install apiaryio`
- Finally we can use `apiary preview --path=snippet-sharing.apib --output=snippet-sharing-api.html` in the [blueprints](/doc/api_blueprints) directory to generate the pretty version of the API description. This file can then be placed wherever it is accessible or needed.

In case you change the API description (and the backend), you may want to ensure
that your API blueprint is still syntax conform. To do so, you can use the tool
[Drafter](https://github.com/apiaryio/drafter). After installing it, you can use
`drafter <filename>` (e.g. `drafter snippet-sharing.apib`) to run the check.

### Use other Webserver than the built-in Grunt Webserver ###

Of course it is possible to use another webserver instead of the built-in one.
To do so, simply run `kdb build-rest-frontend` and copy the content of the
`/usr/local/share/elektra/tool_data/rest-frontend/public` directory to
your desired target location.

It is required that you set a rewrite rule that serves the `index.html` for every
request that does not access a static file (js, css, png, md, etc.). If you omit
this step, it will not be possible to use direct links to access resources of the
frontend; accessing the frontend from the `index.html` will still work though.

## Example Structure ##

The following is a description of the setup we used for the Elektra website
reachable at https://www.libelektra.org.

We assume that Elektra is now installed to the default path on Debian Jessie,
which is `/usr/local`.

### Web Server ###

As web server we are using an Apache2 with the version coming with Debian Jessie.
Several domains are used for different tasks, whereas only two are relevant for
the here described service:

- `restapi.libelektra.org` for the API provided by the `rest-backend`
- `www.libelektra.org` for the website and frontend provided by the `rest-frontend`

The server redirects requests on port 80 (non-SSL) to 443 using a very simple
configuration like

```
# file: /etc/apache2/sites-available/www.libelektra.org.conf
<VirtualHost *:80>
    ServerName www.libelektra.org
    Redirect permanent / https://www.libelektra.org/
</VirtualHost>
```

for the `www.libelektra.org` domain (similar for `restapi.libelektra.org`).

The secured variant of the configuration looks like

```
# file: /etc/apache2/sites-available/www.libelektra.org-le-ssl.conf
<IfModule mod_ssl.c>
<VirtualHost *:443>
    ServerName www.libelektra.org

    DocumentRoot "/usr/local/share/elektra/tool_data/rest-frontend/public"
    <Directory />
        FallbackResource /index.html
        Options Indexes MultiViews
        AllowOverride None
        Require all granted
        Allow from all
    </Directory>

    LogLevel info
	ErrorLog ${APACHE_LOG_DIR}/error.log
	CustomLog ${APACHE_LOG_DIR}/access.log combined

    SSLCertificateFile /etc/letsencrypt/live/www.libelektra.org/fullchain.pem
    SSLCertificateKeyFile /etc/letsencrypt/live/www.libelektra.org/privkey.pem
    Include /etc/letsencrypt/options-ssl-apache.conf
</VirtualHost>
</IfModule>
```

Important is the `Directory` configuration because the `rest-frontend` requires the
`FallbackResource` option to function correctly.

For the `restapi.libelektra.org` domain we use an SCGI setup:

```
# file: /etc/apache2/sites-available/restapi.libelektra.org-le-ssl.conf
<IfModule mod_ssl.c>
<VirtualHost *:443>
    ServerName restapi.libelektra.org

    SCGIMount / 127.0.0.1:8081

    LogLevel info
	ErrorLog ${APACHE_LOG_DIR}/error.log
	CustomLog ${APACHE_LOG_DIR}/access.log combined

    SSLCertificateFile /etc/letsencrypt/live/restapi.libelektra.org/fullchain.pem
    SSLCertificateKeyFile /etc/letsencrypt/live/restapi.libelektra.org/privkey.pem
    Include /etc/letsencrypt/options-ssl-apache.conf
</VirtualHost>
</IfModule>
```

### rest-backend ###

The `rest-backend` itself is configured normally as described in the configuration
section above, but with CppCMS using SCGI instead of HTTP as API variant.
This requires setting the keys

```sh
kdb set system/sw/elektra/restbackend/#0/current/cppcms/service/api "scgi"
kdb set system/sw/elektra/restbackend/#0/current/cppcms/service/ip "127.0.0.1"
kdb set system/sw/elektra/restbackend/#0/current/cppcms/service/port 8081
```

Additionally we are using a worker process, which ensures that in case of a crash
the backend restarts automatically (= basically supervisor + worker). Config:

```sh
kdb set system/sw/elektra/restbackend/#0/current/cppcms/service/worker_processes 1
```

Configuration snippets and users are stored at `system/configs` and `system/users`:

```sh
kdb set system/sw/elektra/restbackend/#0/current/backend/kdb/path/configs = system/configs
kdb set system/sw/elektra/restbackend/#0/current/backend/kdb/path/users = system/users
```

### rest-frontend ###

Because of the Apache server using the rest-frontend installation directory as
document root, there is no further configuration necessary other than already
explained in the configuration section above.

During the build, the frontend will be recompiled. It is not taken offline for this
though and everything that is necessary to accomplish a clean deployment is taken
care of by the compile script as well.

### Jenkins Build Script ###

The build script basically builds the applications, runs tests, installs everything
and restarts the backend. Finally, it can run the configuration script for the frontend,
which updates the website content.

The current build script can be found [here](/scripts/build-homepage).
