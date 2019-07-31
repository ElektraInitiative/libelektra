# Trying out Elektra with Vagrant

This Vagrantfile provisions a machine based on ubuntu/artful32 with all tools to build Elektra.

If [Vagrant](https://www.vagrantup.com/) is installed on your machine change to the directory containing the file you are currently reading and build a box for vagrant with

```sh
$ vagrant up
```

This will take some time.

When the machine is running, access it with

```sh
$ vagrant ssh
```

In this SSH session you can interact with the machine.

By default Vagrant synchronizes the folder on the host machine containing the vagrantfile with the folder `/vagrant` in the VM.

```sh
# add build directory & cd to it
$ mkdir /vagrant/build
$ cd /vagrant/build

# some exports to keep everything in the build directory
$ export INSTALL_DIR="$PWD/install"
$ export SYSTEM_DIR="$PWD/kdbsystem"
$ export PATH=$PATH:"$PWD/bin"
$ export LD_LIBRARY_PATH="$PWD/lib"
$ export GTEST_ROOT="/opt/gtest"


# configure
$ cmake -DENABLE_COVERAGE=OFF -DENABLE_OPTIMIZATIONS=OFF -DENABLE_DEBUG=ON -DENABLE_LOGGER=ON -DBUILD_STATIC=OFF -DCMAKE_INSTALL_PREFIX="$INSTALL_DIR" -DKDB_DB_SYSTEM="$SYSTEM_DIR" -DPLUGINS="ALL" -DTOOLS="ALL" ..

# build
$ make -j4

# run Elektra's test suite
$ make run_all

# run the memory checks with valgrind
$ make run_memcheck
```

When you are done leave the VM with `CTRL-D` or `exit`.

You can either shut the VM down with `vagrant halt` or delete it with `vagrant destroy`.
