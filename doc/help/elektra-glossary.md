elektra-glossary(7) -- glossary of Elektra
==========================================

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

- **pop**: used in ksPop() and @ref KDB_O_POP means to remove
  a key from a keyset.

- **delete**: or abbr. del, used in keyDel(), ksDel() and @ref KDB_O_DEL means to free a key or keyset. The memory
  can be used for something else afterwards.

- **remove**:  means that the key/value information in the physical database will be removed permanently.

- To **elektrify** an application:
  to change the application so that it uses Elektra afterwards.
