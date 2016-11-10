Elektra provides a global key database where configuration of all
applications can be integrated.

There are different forms of integration.  In this tutorial we will
discuss a light form of integration that can be achieved without
elektrifying applications.

The idea is to manipulate the configuration values directly in the
configuration files via the key database.  Thus the applications will
read the configuration file, changes in the key database will be picked
up by applications.

Goals of this Tutorial:

- [ ] Understanding Backends
- [ ] Understanding Mounting
- [ ] Understanding Plugins
- [ ] Understanding get/set

## Limitations

The approach is not ideal when the application itself also changes the
configuration thus Elektra is bypassed then.  But th Furthermore one
cannot easily change the configuration format file format (it must be
in the format the application understands) thus one loses quite some
flexibility.

But many applications are only reading the configuration file and for
many setups it is acceptable not being able to change the file format.
The heart of the approach is the so called mounting of configuration files
into the key database.  Note that mounting requires root permissions.

    sudo kdb mount /etc/hosts system/hosts hosts

1. The first argument is the configuration file we want to mount, 2. the
second which path it should have in the key database, and 3. the third
is the plugin that can read and write this configuration format.

We can verify that the file is mounted via:

    kdb file system/hosts

After mounting a file, we can modify the keys below `system/hosts`.
We need again to be root, because we modify `/etc/hosts`.

    sudo kdb set system/hosts/ipv4/mylocalhost 127.0.0.33

These changes are automatically reflected in `/etc/hosts`:

    cat /etc/hosts 127.0.0.33      mylocalhost 127.0.0.1       localhost
    ...

Thus we wrote using the format used by all applications, applications
will now pick up these changes:

    ping mylocalhost
