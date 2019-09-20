# elektra-web/elektrad

A server that provides an [HTTP API](http://docs.elektrad.apiary.io) to access
Elektra remotely, built using [go](https://golang.org).

## Installation

- make sure you have go (>1.11) installed.
- install [libelektra](https://libelektra.org/), make sure the package-config env variable is set correctly.
- now run `go build` in the elektrad folder with go module enabled (GO111MODULE=on).

## Running

The output of `go build` is a binary, you can simply run it with:

```
./elektrad
```

## Flags

`-port 33333` - change the port the server uses.

## API

By default, `elektrad` runs on [http://localhost:33333](http://localhost:33333)

- API documentation: https://elektrad.docs.apiary.io/
- API blueprint: [elektrad.apib](https://master.libelektra.org/doc/api_blueprints/elektrad.apib)
