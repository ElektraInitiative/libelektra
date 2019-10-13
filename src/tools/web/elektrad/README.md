# Elektrad

## Introduction

A server that provides an [HTTP API](http://docs.elektrad.apiary.io) to access
Elektra remotely, built using [Go](https://golang.org).

## Compiling

You can compile `elektrad` manually or via CMake.

If [go-elektra](https://github.com/ElektraInitiative/go-elektra) fails to compile checkout the [README.md](https://github.com/ElektraInitiative/go-elektra/blob/master/README.md) for troubleshooting.

### Manually

- make sure you have go (>1.11) installed.
- install [libelektra](https://libelektra.org/).
- now run `go build` in the elektrad folder with Go modules enabled (GO111MODULE=on).

The output of `go build` is a binary, you can simply run it with:

```sh
./elektrad
```

### With CMake

Compile Elektra as described in the [COMPILE document](/doc/COMPILE.md), make sure to include the `web` and `kdb` tool using the `-DTOOLS` flag, e.g. `-DTOOLS="kdb;web"`.

Since this package is leveraging pkg-config files we need an intermediate `elektra.pc` file, which is generated from `temp-elektra.pc.in` in the building phase of Elektra to locate the header files and symbols while Elektra is not installed this machine. 

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
