# elektra-web/elektrad

A server that provides an [HTTP API](http://docs.elektrad.apiary.io/) to access
Elektra remotely, built using [express](https://expressjs.com/).


## Installation

 * first, install [libelektra](http://libelektra.org/)
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


## API

By default, `elektrad` runs on [http://localhost:33333](http://localhost:33333)

 - API documentation: http://docs.elektrad.apiary.io/
 - API blueprint: [elektrad.apib](http://tree.libelektra.org/doc/api_blueprints/elektrad.apib)
