# elektra-web/webd

The instance management server for elektra-web, built using
[express](https://expressjs.com/). It accesses the API of multiple
[`elektrad`](../elektrad/) instances and provides an API to manage instances.

## Installation

- first, install [libelektra](https://libelektra.org/) with the [`yajl` plugin](https://master.libelektra.org/src/plugins/yajl/)
- now run `npm install`

## Running

```
npm start
```

to run in verbose mode (full logs):

```
npm run start:verbose
```

to run in development mode (full logs & running from source):

```
npm run start:dev
```

## Client

By default, `webd` serves its API and the elektra-web client on
[http://localhost:33334](http://localhost:33334)

If you want to use the `webd` in combination with the client in development
mode, start the client via `npm run start:dev`, it will also start `webd`.

## API

- API documentation: https://elektrawebd.docs.apiary.io/
- API blueprint: [webd.apib](https://master.libelektra.org/doc/api_blueprints/webd.apib)
