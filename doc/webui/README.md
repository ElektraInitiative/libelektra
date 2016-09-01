# elektra web

_a web user interface (Web UI) to remotely manage multiple elektra instances_


## Overview

Elektra web consists of multiple components:

 * (multiple) servers running an elektra daemon (`elektrad`)
 * a single cluster management server to communicate with the elektra daemons (`clusterd`)
 * a client (web browser) that accesses the Web UI on the cluster management server

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/network_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/network_structure.png)


## GUI

The Web UI allows the user to add new instances to the network, as well as
combine multiple instances into a cluster. If the configuration of a cluster is
edited, the changes are pushed to all instances in the cluster. Furthermore,
single instances can be configured independently.

The configuration view of elektra web is similar to the tree view of the
[qt-gui](https://github.com/omnidan/libelektra/tree/master/src/tools/qt-gui).

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/ui_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/ui_structure.png)


## API

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/daemon_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/daemon_structure.png)

To access single instances, each elektra daemon (`elektrad`) provides a RESTful
HTTP API:

 * **GET /version** - get `elektrad` version
 * **GET /kdb/:path** - get `path` configuration (similar to `kdb get path`)
 * **POST /kdb/:path** - edit `path` configuration (similar to `kdb set path`)

The cluster management server (`clusterd`) also provides a RESTful HTTP API.
Single instances can be configured as follows:

 * **GET /instances** - get a list of all instances
 * **POST /instances** - create a new instance
 * **GET /instances/:id** - get information about a single instance
 * **POST /instances/:id** - edit a single instance
 * **GET /instances/:id/kdb** - get full configuration of an instance
 * **GET /instances/:id/kdb/:path** - get `path` configuration of an instance (similar to `kdb get path`)
 * **POST /instances/:id/kdb/:path** - edit `path` configuration of an instance (similar to `kdb set path`)
 * **GET /instances/:id/version** - get `elektrad` version of an instance

It is also possible to create and manage groups of multiple elektra instances (clusters):

 * **GET /version** - get `clusterd` version
 * **GET /clusters** - get a list of all clusters
 * **POST /clusters** - create a new cluster
 * **GET /clusters/:id** - get information about a single cluster
 * **POST /clusters/:id** - edit a single cluster
 * **GET /clusters/:id/kdb** - get full configuration of a cluster
 * **GET /clusters/:id/kdb/:path** - get `path` configuration of a cluster (similar to `kdb get path`)
 * **POST /clusters/:id/kdb/:path** - edit `path` configuration of a cluster (similar to `kdb set path`)
 * **GET /clusters/:id/version** - get list of `elektrad` versions of all instances in the cluster

Cluster configuration is stored on the cluster management server and persisted
to all instances.
