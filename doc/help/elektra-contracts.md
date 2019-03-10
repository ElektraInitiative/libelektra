# elektra-contracts(7) -- contracts for plugins

Each plugin in a backend can cause run-time errors. Additionally,
the chaining of the plugins can introduce further run-time errors.
For example, a plugin can modify keys so that the next plugin cannot
process these keys anymore. Or a plugin can omit changes to the keys
that are required by the next plugin. To deal with such situations in
a controlled way, each plugin exports a contract that describes
the interaction with other parts of the backend. A `KeySet` contains
the description.

Mounting of backends actually takes place at run-time, we will refer
to it as mount-time. The time when applications access the key
database, however, will be called run-time.

The contract checker revises contracts of plugins during the
mount-time. Afterwards, at run-time, no such type errors can occur.
`kdb mount` implements the contract checker. It can refuse to add a
plugin to the backend because of a conflict or constraint. As long as
not all contracts are satisfied `kdb mount` waits for more plugins to
be attached.

The content of the contract is well-specified in
[CONTRACT.ini](/doc/CONTRACT.ini).

Plugins are the only place to export contracts, but they are not the only
party. It is also possible that the administrator extends the requirements
to every backend, for example, if notification is required. This implies
that every backend has to provide a specific additional service that
will be checked using contracts. Plugins are also involved in contracts
with Elektra’s core. This topic will be discussed in
[elektra-algorithm](/doc/dev/algorithm.md)
in which the algorithm used by Elektra’s core is explained.

## Assertions

Contracts as introduced by Meyer define
preconditions and postconditions on routines.
Assertions handle these conditions by checking them before entering or
after leaving a routine. Because most of this can happen only at run
time, it often leaves the user
alone with an exception.

Elektra goes a step further. Instead of exiting the normal control flow
when a precondition is not met, it is the responsibility of a special
plugin to handle the situation and make sure that the precondition is met
afterwards. Sometimes this is not possible. In these cases, the plugins
check the necessary conditions and return with error code when they are
not met. As we will see, these situations do not occur very often.

Because of the modular approach, we can have several checkers, and
correcting and checking can be combined. Plugins can work together to
reach a certain goal.

`KeySet` and `Key` already handle most parts of checking pre- and
postconditions imposed on data structures. Elektra’s core provides
preconditions and weak postconditions for the plugins.

## SEE ALSO

- [CONTRACT.ini](/doc/CONTRACT.ini)
- [plugins-ordering](/doc/dev/plugins-ordering.md)
