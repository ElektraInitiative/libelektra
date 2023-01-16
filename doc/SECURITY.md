# Security

Security is a very important point in libraries. In most use
cases there is nearly no point of danger in using Elektra.
But some a very security related, especially when you use
a daemon or some kind of distributed configuration.

## Access Permissions

We only use access permissions from the kernel, we do
not add an additional layer (or daemon). So configuration
file access is as secure as with direct access to
configuration files.

## Namespaces

Elektra by default guarantees that configuration from
specific namespaces come from respective paths in the
file system:

- `dir`-namespace: from current working directory
- `user`-namespace: from users home directory
- `system` or `spec`-namespace: no restrictions

## Environment Variables

Environment variables are usually avoided, but instead
Elektra itself is used to configure Elektra.
The core is not allowed to use any environment variables.

For some plugins, however, Environment variables are
used for better integration in systems. This might
be a security risk.

## Compiler Options

Can be changed using standard CMake ways.
Some hints:

https://wiki.debian.org/Hardening

## Memory Leaks

We use Valgrind (`--tool=memcheck`) and ASAN (clang+gcc) to
make sure that Elektra does not suffer memory leaks and
incorrect memory handling.
