# elektra-glossary(7) -- glossary of Elektra

- **Configuration settings**:
  customize applications towards the users'
  needs. It fulfills following properties:

  - It is provided by the execution environment.
  - It can be changed by the maintainer, user, or system administrator of the software.
  - It consists of a key name, a **configuration value**, and potentially **metadata**.

- A **configuration file**:
  is a file containing configuration settings.

- **Configuration storage**:
  makes configuration settings persistent.
  The application will read the configuration
  at every start from the configuration storage,
  but it is only stored if a user changes settings.

- **Key databases**:
  are used for configuration storages because of these constraints.
  They can do fast key lookups and the keys can be structured
  hierarchically by defining separators in the key names.

- **Global key database**:
  provides global access to all configuration storages
  of all applications in a system.
  Abbreviated as `KDB`.

- **LibElektra**:
  is a set of [libraries](/src/libs/) to access configuration parameters in a global,
  hierarchical key database.

- **SpecElektra**:
  is a [specification language](/doc/METADATA.ini) that allows us to describe the
  content of the global key database.

- **Elektra**:
  is a framework consisting of LibElektra, SpecElektra,
  and a collection of tools.

- To **elektrify** an application:
  to change the application so that it uses LibElektra afterwards.

- **Elektra Initiative**:
  is a community that develops LibElektra, expands SpecElektra,
  improves Elektra's tooling and helps to elektrify applications.

- **Option**, more specifically **Command-line option**:
  is a special argument passed on the command-line. **Short options**
  are single characters prefixed with '-'; **Long options** are
  arbitrarily long and start with '--'.

- **Module**:
  The parts Elektra is composed of, i.e. either lib, plugin, backend, tool.

- **Class**:
  A group of functions that logically belong together, working on the same type of objects.
  A library may implement several classes.
  E.g., `KDB`, `Key` and `KeySet` are the most important classes.

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
  The **mountpoint** is the key where the backend is mounted to.
  All keys of the backend are below that key.

- [Key name](/doc/KEYNAMES.md):
  All keys in the KDB have a name.
  This name is the keys unique identifier and follows a particular structure.
  For more information take look at the [keyname documentation](/doc/KEYNAMES.md).

- [Key name part](/doc/KEYNAMES.md):
  Key names consist of a series parts (and a namespace).

- [Key base name](/doc/KEYNAMES.md):
  The last part of a key name.

- [Key dir name](/doc/KEYNAMES.md):
  The key name obtained by omitting both namespace and base name from a key name.

- [Namespaces](elektra-namespaces.md):
  Allow us to have multiple keys for the same purpose and otherwise the same key name.

- [Plugins](/src/plugins):
  The unit of implementation for a feature.

- [Metadata](elektra-metadata.md):
  Allows us to describe configuration settings.

- **persistent name/value/metadata**:
  How it is actually stored, i.e. the state returned by and passed to the `storage` plugins.

- **transient name/value/metadata**:
  How it is at runtime, i.e. what is returned by `kdbGet` and passed to `kdbSet`.

- **intermediate name/value/metadata**:
  Any state inbetween the two.

## Details

- Null Value:
  The absence of a value, i.e. `keyValue (key) == NULL`.

- **pop**:
  Used in `ksPop()` and `KDB_O_POP` means to remove a key from a keyset.

- **delete**:
  or abbr. del, used in `keyDel()`, `ksDel()` and `KDB_O_DEL` means to free a key or keyset. The memory
  can be used for something else afterwards.

- **remove**:
  Means that the key-value information in the physical database will be removed permanently.
  Also used to describe removing a particular key from a keyset.
