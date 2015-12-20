elektra-glossary(7) -- glossary of Elektra
==========================================

## Introduction

- **Configurations**:
  contain user preferences or other application
  settings.

- **Configuration storage**:
  makes this information permanent.
  The application will read the configuration
  at every start, but it is only stored
  if a user changes settings.

- **Key databases**:
  are used because of these constraints.
  They can do fast key lookups and the keys can be structured
  hierarchically by defining separators in the key names.
  Unlike SQL databases, the key name is the only primary key; there are
  no foreign keys, and no query language exists.

- **Global key database**:
  provides global access to all key databases
  of all applications in a system that wants to access a key database.

- To **elektrify** an application:
  to change the application so that it uses Elektra afterwards.

## Technical Concepts

- [Backends](elektra-backends.md):
  A collection of **plugins** to be **mounted**.
  A **backend** typically is responsible to read and write a configuration file.

- [Bootstrapping](elektra-bootstrapping.md):
  To read the mounting configuration and mount during `kdbOpen()`.

- [Cascading](elektra-cascading.md):
  To consider multiple places to look for a key.

- [Contracts](elektra-contracts.md):
  Contracts state the purpose, functionality and requirements of **plugins**.

- [Mounting](elektra-mounting.md):
  To persistently and permanently include a **backend** in the **global key database**.

- [Namespaces](elektra-namespaces.md):
  Allow us to have multiple keys for the same purpose and otherwise the same key name.

- [Plugins](elektra-plugins-framework.md):
  The unit of implementation for a feature.


## Details

- [Sync Flag](elektra-sync-flag.md):
  Marks keys that were changed and need to be written out to disc.

- [Null Keys, Null Values](elektra-values.md):
  The absence of keys or values.

- **pop**:
  used in ksPop() and @ref KDB_O_POP means to remove
  a key from a keyset.

- **delete**:
  or abbr. del, used in keyDel(), ksDel() and @ref KDB_O_DEL means to free a key or keyset. The memory
  can be used for something else afterwards.

- **remove**:
  means that the key/value information in the physical database will be removed permanently.
