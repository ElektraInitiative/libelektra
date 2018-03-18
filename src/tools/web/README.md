# elektra-web

_an API and web user interface to remotely manage multiple Elektra instances_


## Dependencies

Elektra-web requires:

 * [Elektra](https://libelektra.org/) with the [`yajl` plugin](https://master.libelektra.org/src/plugins/yajl/) installed
 * A recent [node.js](https://nodejs.org/en/) installation (at least 6.x)


## Getting Started

 * Install dependencies (see above)
 * Clone libelektra repo and `cd libelektra/src/tools/web`
 * Install and start an elektrad instance:
   * `cd elektrad`
   * `npm install`
   * `npm start`
 * Install and start the client (connects to the elektrad instance):
   * `cd client`
   * `npm install`
   * `npm start`
 * You can now access the client on: [http://localhost:33334](http://localhost:33334)

### Running elektra-web on a single instance

If you do not want to configure multiple instances, you can set the `INSTANCE`
environment variable to the server you want to configure. You can also set
`user/sw/elektra/web/#0/current/instance` to the host. Make sure to enter a full
HTTP URL, e.g. `http://localhost:33333`.

If this configuration option is set, elektra-web will load the configuration
page for that instance instead of the main overview page.

If you want to host elektra-web with the client and elektrad on the same
instance, after starting elektrad via `npm start`, you can run start the
client as follows:

```
INSTANCE="http://localhost:33333" npm start
```

Now, when you open [http://localhost:33334](http://localhost:33334) in your
browser, the configuration page for the instance will be opened immediately.


## Overview

Elektra web consists of multiple components:

 * (multiple) servers running an elektra daemon ([`elektrad`](elektrad/))
 * a single server to communicate with the elektra daemons and serve the client ([`webd`](webd/))
 * a web browser that accesses the client (Web UI) on the [`webd`](webd/) server ([`client`](client/))

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/network_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/network_structure.png)


## GUI

The Web UI allows the user to add multiple instances to be configured.

The configuration view of elektra web is similar to the tree view of the
[qt-gui](https://git.libelektra.org/tree/master/src/tools/qt-gui), but with
dynamic fields rendered via key metadata.

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/ui_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/ui_structure.png)


## API

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/daemon_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/daemon_structure.png)

[API blueprints](https://apiblueprint.org/) are available for both APIs:

 * [elektrad](https://master.libelektra.org/doc/api_blueprints/elektrad.apib), documentation: http://docs.elektrad.apiary.io/
 * [clusterd](https://master.libelektra.org/doc/api_blueprints/clusterd.apib), documentation: http://docs.clusterd.apiary.io/


## Auth

Currently, clusterd does not support authentication. The best way to work around
this is to use a reverse proxy (e.g. [nginx reverse proxy](https://www.nginx.com/resources/admin-guide/reverse-proxy/)).

Once you set up a reverse proxy on your web server, you can use it to
authenticate users, e.g. by [username/password auth](https://www.digitalocean.com/community/tutorials/how-to-set-up-password-authentication-with-nginx-on-ubuntu-14-04)
