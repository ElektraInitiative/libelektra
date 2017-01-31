# elektra-web/client

The client for elektra-web, built using
[React](https://facebook.github.io/react/) and [Redux](http://redux.js.org/). It
accesses the API that [`clusterd`](../clusterd/) provides.


## Running

The elektra-web client is now served by `clusterd`, please check
[the `clusterd` README](../clusterd/README.md) for information on how to run it.


## Features

**Implemented:**

 - manage ([`elektrad`](../elektrad/)) instances and clusters
 - configure instances and clusters with a simple tree view
 - creating, setting, removing keys

**In development:**

 - use elektra metadata
 - improved tree view with dynamic fields
 - [undo/redo](https://github.com/omnidan/redux-undo)
