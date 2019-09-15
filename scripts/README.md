## Scripts Index

There should be no scripts top-level but only in sub directories.

### Scripts For Users

These files are installed on the target system.

- [kdb](kdb): for scripts to be used with `kdb <script>`.
- [ffconfig](ffconfig): to configure firefox.
- [completion](completion): for shell completion files.

The completion files need [INSTALL_SYSTEM_FILES](/doc/COMPILE.md) to be installed.

### Scripts For Elektra Developers

- [dev](dev): for development scripts.
- [cmake](cmake): everything shared for the CMake build system.
- [randoop](randoop): For [automatic unit test generation for Java](https://randoop.github.io/randoop/).

### Scripts For Build Server

- [jenkins](jenkins)
- [docker](docker)
- [vagrant](vagrant)
