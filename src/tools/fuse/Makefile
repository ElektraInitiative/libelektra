.PHONY: build clean install

# Use this makefile to install elektra_fuse in pips default install location (used in the docker dev-environment),
# and cmake to install it along Elektra.


build: clean
	python3 ./setup.py bdist_wheel

install:
	pip3 install --no-cache-dir ./dist/elektra_fuse-1.0.0-py3-none-any.whl

uninstall:
	pip3 uninstall --yes --no-cache-dir elektra_fuse

clean:
	rm -rf build/ dist/ src/elektra_fuse.egg-info/ 2> /dev/null

