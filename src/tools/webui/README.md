# elektra-web/client

The client for elektra-web, built using
[React](https://facebook.github.io/react/) and [Redux](http://redux.js.org/). It
accesses the API that [`webd`](../webd/) provides.

## Installation

- first, install [libelektra](http://libelektra.org/) with the [`yajl` plugin](http://master.libelektra.org/src/plugins/yajl/)
- now run `npm install`

## Running

Start the client in **production** mode via:

```
npm start
```

Alternatively, you can use **development** mode:

```
npm run start:dev
```

## Features

**Implemented:**

- manage ([`elektrad`](../elektrad/)) instances
- configure instances with a simple tree view
- creating, setting, removing keys
- uses elektra metadata to display dynamic fields
- editing of metadata
- drag & drop to move keys
- search to filter keys
