# elektra web

_an API and web user interface to remotely manage multiple Elektra instances_


## dependencies

Elektra web requires:

 * Elektra with the `yajl` plugin installed
 * A recent [node.js](https://nodejs.org/en/) installation (at least 4.x)


## Overview

Elektra web consists of multiple components:

 * (multiple) servers running an elektra daemon ([`elektrad`](elektrad/))
 * a single cluster management server to communicate with the elektra daemons ([`clusterd`](clusterd/))
 * a client (web browser) that accesses the Web UI on the cluster management server ([`client`](client/))

![https://cdn.rawgit.com/omnidan/libelektra/elektra-web/src/tools/web/doc/network_structure.png](https://cdn.rawgit.com/omnidan/libelektra/elektra-web/src/tools/web/doc/network_structure.png)


## GUI

The Web UI allows the user to add new instances to the network, as well as
combine multiple instances into a cluster. If the configuration of a cluster is
edited, the changes are pushed to all instances in the cluster. Furthermore,
single instances can be configured independently.

The configuration view of elektra web is similar to the tree view of the
[qt-gui](http://git.libelektra.org/tree/master/src/tools/qt-gui).

![https://cdn.rawgit.com/omnidan/libelektra/elektra-web/src/tools/web/doc/ui_structure.png](https://cdn.rawgit.com/omnidan/libelektra/elektra-web/src/tools/web/doc/ui_structure.png)


## API

![https://cdn.rawgit.com/omnidan/libelektra/elektra-web/src/tools/web/doc/daemon_structure.png](https://cdn.rawgit.com/omnidan/libelektra/elektra-web/src/tools/web/doc/daemon_structure.png)

[API blueprints](https://apiblueprint.org/) are available for both APIs:

 * [elektrad](https://github.com/omnidan/libelektra/elektra-web/doc/apiblueprints/elektrad.apib), documentation: http://docs.elektrad.apiary.io/
 * [clusterd](https://github.com/omnidan/libelektra/elektra-web/doc/apiblueprints/clusterd.apib), documentation: http://docs.clusterd.apiary.io/
