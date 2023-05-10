# Go Bindings for Elektra

This folder contains Go bindings for the low-level API for Elektra as found in `kdb.h`.

Go-Elektra leverages [cgo](https://golang.org/cmd/cgo/) to call the C functions of the Elektra library.

## Prerequisites

- Go (version >1.13) and
- libelektra installed must be available.

## Build

Run

`go install ./kdb`

or

`go build ./kdb`

## Run Tests

Prerequisite: Elektra and Go installed on your machine.

To execute all tests:

`go test ./...`

Execute tests of a package, e.g. kdb:

`go test ./kdb`

## Run Benchmarks

The [benchmarks](kdb/benchmark_test.go) contains several benchmarks, every function that starts with `Benchmark` is a separate benchmark,
e.g. `BenchmarkKeySetInternalCallbackIterator`.

To run the benchmarks enter the following command from the root folder of this package:

```sh
go test ./kdb -bench=.
```

It is also possible to filter certain benchmarks

```sh
go test ./kdb -bench="^(BenchmarkKeySetSliceRangeIterator)\$"
```

## Use Elektra

### In your Application

First `go get` the package like you are used to with Go. If you want to use your newest changes you have made to `go-elektra` (changes are not yet on master) it works the same due to the local module reference we are using in `go.mod`.

`go get github.com/ElektraInitiative/libelektra/go-elektra`

Here is an example how you can use Elektra in your Go application.
Before you start create a key via the `kdb` command-line tool:

`kdb set user:/go/elektra 'Hello World!'`

Save the following code to a file, e.g.: `elektra.go` and run it via

`GO111MODULE=on go run elektra.go`

Error handling was omitted for brevity.

```go
package main

import (
	"fmt"
	"os"

	"github.com/ElektraInitiative/libelektra/go-elektra/kdb"
)

func main() {
	ks := kdb.NewKeySet()
	defer ks.Close()

	keyName := "/go/elektra"

	handle := kdb.New()

	// Open the handle, this is a separate step since there can be different implementations of the KDB interface.
	err := handle.Open()
	if err != nil {
		fmt.Println("Error while opening the database", err)
		os.Exit(1)
	}
	defer handle.Close()

	parentKey, err := kdb.NewKey("user:/")
	if err != nil {
		fmt.Println("Error while creating new key", err)
		os.Exit(1)
	}
	defer parentKey.Close()

	_, err = handle.Get(ks, parentKey)
	if err != nil {
		fmt.Println("Error while getting parent key and all keys below", err)
	}

	foundKey := ks.LookupByName(keyName)

	if foundKey == nil {
		fmt.Printf("Key %q not found, please run the following command to create it:\nkdb set user:/go/elektra 'Hello World!'\n", keyName)
	} else {
		value := foundKey.String()
		fmt.Printf("Value of %q is: %s\n", keyName, value)
	}
}
```

### Test examples

The test files (`*_test.go`) are also a good source if you want to get to know how to use these bindings.

- [kdb tests](kdb/kdb_test.go)
- [keyset tests](kdb/keyset_test.go)
- [key tests](kdb/key_test.go)

## Documentation

The documentation can be viewed on [godoc.org](https://godoc.org/github.com/ElektraInitiative/libelektra/go-elektra/kdb).

## Troubleshooting

### Package Elektra was not found in the Pkg-config Search Path

First make sure that libelektra is installed.

Go-Elektra leverages [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)
to compile the Elektra library.

You need to set the `PKG_CONFIG_PATH` to the installation folder of Elektra, e.g.: if libelektra is installed to `/usr/local` you need to
set the environment variable `PKG_CONFIG_PATH=/usr/local/lib/pkgconfig`.

### Invalid Flag in Pkg-config --libs: elektra/build/lib

If you get an error message like this you most likely have whitespace in your build path.
It appears that go currently does not support whitespaces in package-config (issues https://github.com/golang/go/issues/7906,
https://github.com/golang/go/issues/16455).

### Cannot find package "github.com/ElektraInitiative/libelektra/go-elektra/kdb"

Make sure your version of Go is > `1.13` and either set the ENV variable `GO111MODULE=on` or run `go mod init` in the folder containing
your go code.
