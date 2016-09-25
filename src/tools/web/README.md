# elektra web

_an API and web user interface to remotely manage multiple elektra instances_


## Overview

Elektra web consists of multiple components:

 * (multiple) servers running an elektra daemon ([`elektrad`](elektrad/))
 * a single cluster management server to communicate with the elektra daemons ([`clusterd`](clusterd/))
 * a client (web browser) that accesses the Web UI on the cluster management server ([`client`](client/))

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/src/tools/web/doc/network_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/src/tools/web/doc/network_structure.png)


## GUI

The Web UI allows the user to add new instances to the network, as well as
combine multiple instances into a cluster. If the configuration of a cluster is
edited, the changes are pushed to all instances in the cluster. Furthermore,
single instances can be configured independently.

The configuration view of elektra web is similar to the tree view of the
[qt-gui](https://github.com/ElektraInitiative/libelektra/tree/master/src/tools/qt-gui).

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/src/tools/web/doc/ui_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/src/tools/web/doc/ui_structure.png)


## API

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/src/tools/web/doc/daemon_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/src/tools/web/doc/daemon_structure.png)

[API blueprints](https://apiblueprint.org/) are available for both APIs:

 * [elektrad](https://github.com/omnidan/libelektra/http-api-proposal/doc/apiblueprints/elektrad.apib), documentation: http://docs.elektrad.apiary.io/
 * [clusterd](https://github.com/omnidan/libelektra/http-api-proposal/doc/apiblueprints/clusterd.apib), documentation: http://docs.clusterd.apiary.io/
