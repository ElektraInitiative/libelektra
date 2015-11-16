kdb(1) -- key database access tools
===================================

Elektra provides a universal and secure framework to store configuration
parameters in a global, hierarchical key database.

The core is a small library implemented in C. The plugin-based framework fulfills many
configuration-related tasks to avoid any unnecessary code duplication
across applications while it still allows the core to stay without any
external dependency. Elektra abstracts from cross-platform-related issues
with an consistent API, and allows applications to be aware of other
applications' configurations, leveraging easy application integration.

## Overview

The man pages can also be viewed online at:
http://doc.libelektra.org/api/current/html/pages.html

And the page you are currently reading at:
http://doc.libelektra.org/api/current/html/md_doc_help_kdb.html

Concepts are in man page section 7 and are prefixed with `elektra-`.
You should start reading [elektra-introduction(7)](elektra-introduction.md).

Tools are in man page section 1 and are prefixed with `kdb-`.
You should start reading [kdb-introduction(1)](kdb-introduction.md).

Documentation of plugins is available using the
[kdb-info(1)](kdb-info.md) tool.
Run `kdb list` for a list of plugins.

The man pages are also displayed when invoking a kdb-tool with `--help`.
