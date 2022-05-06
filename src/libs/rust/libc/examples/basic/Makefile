GCC_BIN ?= $(shell which gcc)
CARGO_BIN ?= $(shell which cargo)

EXAMPLE_ROOT = $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
ELEKTRA_INCLUDES = $(EXAMPLE_ROOT)../../resources/headers
LIBRARY_PATH = $(EXAMPLE_ROOT)../../target/release

run: clean build
	LD_LIBRARY_PATH=$(LIBRARY_PATH) ./basic

clean:
	$(CARGO_BIN) clean
	rm -f basic

build:
	$(CARGO_BIN) build --release
	$(GCC_BIN) -o basic main.c -I$(ELEKTRA_INCLUDES)  -L$(LIBRARY_PATH) -lelektra_rust