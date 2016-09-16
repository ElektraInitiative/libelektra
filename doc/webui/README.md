FORMAT: 1A

# elektra web

_an API and web user interface to remotely manage multiple elektra instances_


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
[qt-gui](https://github.com/ElektraInitiative/libelektra/tree/master/src/tools/qt-gui).

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/ui_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/ui_structure.png)


## API

![https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/daemon_structure.png](https://cdn.rawgit.com/omnidan/libelektra/http-api-proposal/doc/webui/daemon_structure.png)

### Group elektrad API

To access single instances, each elektra daemon (`elektrad`) provides a REST
HTTP API:

#### GET /version

get API and elektra version

+ Response 200 (application/json)
    + Attributes (Version)


#### /kdb/{path}

access the elektra key database by specifying a `path`

+ Parameters
    + path: `user/hello` (string) - path to the elektra config

##### get configuration [GET]

this is the same as calling `kdb get {path}`

+ Response 200 (application/json)

        "hello world"

+ Request nonexistant path
    + Parameters
        + path: `user/nonexistant`

+ Response 404

##### set configuration [PUT]

this is the same as calling `kdb set {path}`

+ Request (application/json)

        "hello world"

+ Response 204

##### delete configuration [DELETE]

this is the same as calling `kdb rm {path}`

+ Response 204

+ Request nonexistant path
    + Parameters
        + path: `user/nonexistant`

+ Response 404



### Group clusterd API

The cluster management server (`clusterd`) also provides a REST HTTP API:

#### GET /

get the API version

+ Response 200 (application/json)
    + Attributes (Base Version)




#### POST /register

API to allow instances to register themselves with `clusterd`

+ Request (application/json)
    + Attributes (Base Instance)

+ Response 204




#### /instances

##### list all instances [GET]

+ Response 200 (application/json)
    + Attributes (array[Instance])

##### create a new instance [POST]

+ Attributes (Base Instance)

+ Request (application/json)

+ Response 200
    + Attributes (Instance)




#### /instances/{instance_id}

+ Attributes (Instance)

+ Parameters
    + instance_id: `507f191e810c19729de860ea` (string) - id of an instance

##### get information about a single instance [GET]

+ Response 200 (application/json)
    + Attributes (Instance)

##### edit a single instance [PUT]

+ Request update host of instance (application/json)
    + Attributes (object)
        + host: 192.168.0.6

+ Response 204

##### delete a single instance [DELETE]

+ Response 204

##### get version of a single instance [GET /instances/{instance_id}/version]

+ Response 200 (application/json)
    + Attributes (Version)




#### /instances/{instance_id}/kdb/{path}

you can access a single instances' configuration via `clusterd` the same way you would directly access it

+ Parameters
    + instance_id: `507f191e810c19729de860ea` (string) - id of an instance
    + path: `user/hello` (string) - path to the elektra config

##### get configuration [GET]

this is the same as calling `kdb get {path}` on the instance

+ Response 200 (application/json)

        "hello world"

+ Request nonexistant path
    + Parameters
        + path: `user/nonexistant`

+ Response 404

##### set configuration [PUT]

this is the same as calling `kdb set {path}` on the instance

+ Request (application/json)

        "hello world"

+ Response 204

##### delete configuration [DELETE]

this is the same as calling `kdb rm {path}` on the instance

+ Response 204

+ Request nonexistant path
    + Parameters
        + path: `user/nonexistant`

+ Response 404




#### /clusters

It is also possible to create and manage groups of multiple elektra instances (clusters).
The API is the same as above, but with `/clusters` instead of `/instances`.

For cluster responses, the results of the operation are grouped together.
If everything is a success, the status code of the combined document will be 200.
Otherwise, it will show an error (400).

# Data Structures

## Base Version (object)
+ api: 1 (number, required) - version of the api

## Version (Base Version)
+ elektra: 0.8.17 (string, required) - libelektra version

## Base Instance (object)
+ host: 192.168.0.5 (string, required)
+ name: test instance (string)

## Instance (Base Instance)
+ id: 507f191e810c19729de860ea (string, required)
