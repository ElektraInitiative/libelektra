- infos/maintainer = Tomislav Makar <tmakar23@gmail.com>

# Elektrad

## Introduction

A server that provides an [HTTP API](http://docs.elektrad.apiary.io) to access
Elektra remotely, built using [Go](https://golang.org).

## Lifetime of KDB handles

Instantiating a KDB Handle for every request is expensive, espescially for big KDB databases, and prevents handling of conflicts. To mitigate this issue sessions with an associated handle are created. One hour after the last request these sessions are destroyed and the KDB handle is closed.

## Source structure

`*_handler.go` files contain the HTTP handler functions.  
`*_handler_test.go` files contain the corresponding handler tests.  
`temp-elektra.pc.in` that CMAKE leverages to create an intermediate pkg-config file that tells the GO compiler where it can find the Elektra header files and symbols during the build step.
`middleware.go` contains the HTTP middleware - such as user session (and caching of Elektra handles) management.  
`router.go` is responsible for setting up the API routes.  
`main.go` is the entry point of the server.

## Compiling

You can compile `elektrad` manually or via CMake.

If `go-elektra` fails to compile checkout the [README.md](../../bindings/go-elektra/README.md) for troubleshooting.

### Manually

- make sure you have go (>1.13) installed.
- install [libelektra](https://libelektra.org/).
- now run `go build` in the elektrad folder with Go modules enabled (GO111MODULE=on).

The output of `go build` is a binary, you can simply run it with:

```sh
./elektrad
```

### With CMake

Compile Elektra as described in the [COMPILE document](/doc/COMPILE.md), make sure to include the `web` and `kdb` tool using the `-DTOOLS` flag, e.g. `-DTOOLS="kdb;web"`.

The binary is located at `build-dir/src/tools/web/elektrad` and symlinked to `build-dir/bin/elektrad`.

### Installing

You can install Elektra as described in the [install documentation](/doc/INSTALL.md).

## To Run

To launch elektrad run the command

```sh
kdb run-elektrad
```

### Flags

`-port 33333` - change the port the server uses.

## API

By default, `elektrad` runs on [http://localhost:33333](http://localhost:33333)

- API documentation: https://elektrad.docs.apiary.io/
- API blueprint: [elektrad.apib](https://master.libelektra.org/doc/api_blueprints/elektrad.apib)
