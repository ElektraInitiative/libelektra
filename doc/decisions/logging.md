# Logging

## Problem

Both code comments and assertions are unfortunately not very popular.
A quite efficient way to still get some documentation about the code
are logging statements. In Elektra they are currently inconsistent
and unusable. Thus there is an urge for this decision.

## Constraints

- this decision is irrelevant for plugins and bindings that are not
  written in C/C++. In any case, however, logging must be disabled
  by default.
- should completely compile away with ENABLE_LOGGER=OFF
- should support minimalistic, compile-time filtering
  (per modules and verbosity level?) and some sinks (stderr, syslog
  or files)

## Assumptions

- run-time problems are checked via assertions, not logged
- opinions about if logging should be to stderr or files differ
- filtering with grep is not enough
- per default there should be no output
- with ENABLE_LOGGER=ON only warnings and errors should be shown on stderr
- other sinks like syslog and file may log more (they are not immediately
  visible and distracting)
- performance is not so important (because logging is usually turned off
  anyway)

## Considered Alternatives

- log similar to the warnings/error system work, discarded because
  of the run-time overhead and no use case why end users should see
  log statements.
- C++ logging library (boost, apache,..), discarded because C++
  should not be in core
- libraries needed static initializing: problematic, logging should
  just work, even if application does not initialize anything
- using syslog: no info from which source file the logging statement
  comes from
- using journald: adds deps problematic for non-linux
- zlog: incompatible licence (LGPL)

## Decision

Provide a Macro

```c
ELEKTRA_LOG (int module, const char *msg, ...);
```

that calls

```c
elektraLog ([as above], const char * function, const char * file,
            const int line, ...);
```

and adds current function, file and line to `elektraLog`'s arguments.

`elektraLog` is implemented in a separate `log.c` file. If someone
needs filtering, logging to different sources or similar, he/she
simply modifies `log.c`.

### Severity

The severity passed to `ELEKTRA_LOG_` should be as in syslog's priority,
except the error conditions which are not needed (asserts should be used
in these situations).

So we would have:

- `ELEKTRA_LOG_WARNING`: warning conditions
- `ELEKTRA_LOG_NOTICE`: normal, but significant, condition
- `ELEKTRA_LOG_INFO`: informational message
- `ELEKTRA_LOG_DEBUG`: debug-level message

### Modules

To add a new module, one simply adds his/her module to `elektramodules.h` via
`#define`:

```c
#define ELEKTRA_MODULE_<NAME> <SEQNUMBER>
```

The module name `<NAME>` shall be consistent with module names used in
`module:` of `src/error/specification`.

## Rationale

A more complex system seems to be overkill. Thus libraries should not have
any effects other than what is described by their API, logging should nearly
always be disabled.

A more "hackable" logger seems to be more suitable for individual needs.
Having a separate `log.c` means that the logger can be changed without the
need to recompile anything but a single file. It also removes the dependency
of `stdio.h` from every individual file to a single place.

Thus logging is very easy to use (only include `elektralog.h` and use
`ELEKTRA_LOG`) and still very flexible (with modules, severity, file, line
and function information nearly everything someone wants from a logging
system can be achieved in `log.c`.

## Implications

The current VERBOSE would be turned off forever and the code within VERBOSE
needs to be migrated to `ELEKTRA_LOG`.

## Related Decisions

- assertions

## Notes
