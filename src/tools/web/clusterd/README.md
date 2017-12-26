# elektra-web/clusterd

The cluster management server for elektra-web, built using
[express](https://expressjs.com/). It accesses the API of multiple
[`elektrad`](../elektrad/) instances and provides an API to manage single
instances and clusters of instances (multiple instances at once).


## Installation

 * first, install [libelektra](http://libelektra.org/) with the [`yajl` plugin](http://tree.libelektra.org/src/plugins/yajl/)
 * now run `npm install`


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

By default, `clusterd` serves its API and the elektra-web client on
[http://localhost:33334](http://localhost:33334)

If you want to use the `clusterd` in combination with the client in development
mode, start the client via `npm run start:dev`, it will also start `clusterd`.


## API

 - API documentation: http://docs.clusterd.apiary.io/
 - API blueprint: [clusterd.apib](http://tree.libelektra.org/doc/api_blueprints/clusterd.apib)
