# elektra-web

_an API and web user interface to remotely manage multiple Elektra instances_


## Dependencies

Elektra-web requires:

 * [Elektra](http://libelektra.org/) with the [`yajl` plugin](http://tree.libelektra.org/src/plugins/yajl/) installed
 * A recent [node.js](https://nodejs.org/en/) installation (at least 6.x)


## Getting Started

 * Install dependencies (see above)
 * Clone libelektra repo and `cd libelektra/src/tools/web`
 * Install and start elektrad:
   * `cd elektrad`
   * `npm install`
   * `npm start`
 * Install and start clusterd:
   * `cd clusterd`
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

If you want to host elektra-web with clusterd and elektrad on the same instance,
you can run clusterd as follows:

```
INSTANCE="http://localhost:33333" npm start
```

Now, when you open [http://localhost:33334](http://localhost:33334) in your
browser, the configuration page for the instance will be opened immediately.


## Overview

Elektra web consists of multiple components:

 * (multiple) servers running an elektra daemon ([`elektrad`](elektrad/))
 * a single cluster management server to communicate with the elektra daemons ([`clusterd`](clusterd/))
 * a client (web browser) that accesses the Web UI on the cluster management server ([`client`](client/))

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/network_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/network_structure.png)


## GUI

The Web UI allows the user to add new instances to the network, as well as
combine multiple instances into a cluster. If the configuration of a cluster is
edited, the changes are pushed to all instances in the cluster. Furthermore,
single instances can be configured independently.

The configuration view of elektra web is similar to the tree view of the
[qt-gui](http://git.libelektra.org/tree/master/src/tools/qt-gui).

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/ui_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/ui_structure.png)


## API

![https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/daemon_structure.png](https://cdn.rawgit.com/ElektraInitiative/libelektra/master/src/tools/web/doc/daemon_structure.png)

[API blueprints](https://apiblueprint.org/) are available for both APIs:

 * [elektrad](http://tree.libelektra.org/doc/api_blueprints/elektrad.apib), documentation: http://docs.elektrad.apiary.io/
 * [clusterd](http://tree.libelektra.org/doc/api_blueprints/clusterd.apib), documentation: http://docs.clusterd.apiary.io/


## Auth

Currently, clusterd does not support authentication. The best way to work around
this is to use a reverse proxy (e.g. [nginx reverse proxy](https://www.nginx.com/resources/admin-guide/reverse-proxy/)).

Once you set up a reverse proxy on your web server, you can use it to
authenticate users, e.g. by [username/password auth](https://www.digitalocean.com/community/tutorials/how-to-set-up-password-authentication-with-nginx-on-ubuntu-14-04)
