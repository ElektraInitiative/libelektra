# Error handling

You might want to read
[about data structures first](data-structures.md).

## Terminology

It is sometimes unavoidable that errors
occur that ultimately have an impact for the user. One example for such an
error is that hard disc space is exhausted. For a library it
is necessary to pass information about the facts and circumstances to the
user, because the user wants to be informed why a requested action failed.
So Elektra gathers all information in these situations. We call this
resulting information **error information** or
**warning information** depending on the severity.

If the error is critical and ultimately causes a situation in which
the post conditions cannot be fulfilled we say that Elektra is in a
**faulty state**. Such a faulty state will change the control flow
inside Elektra completely. Elektra is unable to resolve the problem
without assistance. After cleaning up resources, a faulty state leads
to immediate return from the function with an **error code**. As a
user expects from a library, Elektra never calls `exit()` or something
similar, regardless of how fatal the error is. In this situation error
information should be set.

On the other hand, for many problems the work can go on with reasonable
defaults. Nevertheless, the user will be warned that a problem occurred
using the **warning information**. These situations do not influence
the control flow. But applications can choose to react differently in
the presence of warning information. They may not be interested in any
warning information at all. It is no problem if warning information is
ignored because they are stored and remain accessible in a circular
buffer. The implementation prevents an overflow of the buffer.
Instead, the oldest warnings are overwritten.

When error or warning information is presented to the user, it is called
_error message_ or _warning message_. The user may reply
to this message in which way to continue.

### Error vs. Warning Information

When an error in a faulty state occurs, the error information must
still hold the original error information. So even if a new problem would
cause a faulty state otherwise, the error information must be
omitted or transformed to warning information. In some places only
the addition of warning information is possible:

- The main purpose of `kdbClose()` is to free the
  handle. This operation is always successful and is carried out even
  if some resources cannot be freed. Therefore, in `kdbClose()`,
  setting error information is prohibited. Warning information is, however,
  very useful to tell the user the circumstance that some actions during
  cleanup failed.

- Also in `kdbOpen()`, only adding warning information is allowed.
  If `kdbOpen()` is not able to open a plugin, the affected backend will be
  dropped. The user is certainly interested why that happened. But
  we decided not to make this a faulty state, because the application might
  not even access the faulty part of the key hierarchy. An exception
  to this rule is if `kdbOpen()` fails to open the default backend.
  This situation will induce a faulty state.

- In `kdbSet()`, the clean-up of resources involves calling
  plugins. But during this process Elektra is in a faulty state, so only
  adding of warning information is allowed. This ensures that the original
  error information is passed unchanged to the user.

On the other hand, any access to the key database can produce
warning information. Plugins are allowed to yield warning
information at any place and for any reason. For example, the storage
plugin reading a configuration file with an old syntax, is free to report
the circumstance as warning information. Warning information is also
useful when more than one fact needs to be reported.

## Error Information

### Reporting Errors

Reporting errors is a critical task. Users expect different aspects:

- The **user of the application** does not want to see any
  error message at all. If it is inevitable, he or she wants little,
  but very concrete information, about what he or she needs to do.
  The message should be short and concise. Some error information
  may already be captured by the application, but others like “no
  more free disk space” have to be displayed. Conflicts should also
  be presented to the user. It is a good idea to ask how to proceed
  if a diversity of possible reactions exists. In case of conflicts,
  the user may have additional knowledge about the other program which
  has caused the problem. The user is more likely to decide correctly
  by which strategy the configuration shall be restored.
- The **user of the library** wants more detailed information.
  Categories of how severe the error is can help to decide how to proceed.
  Even more important is the information if it makes sense to try the
  same action again. If, for example, an unreliable network connection
  or file system is used, the same action can work in a second try.
- A **developer of the library** (Library refers to both Elektra’s core and plugins.)
  wants full information about anything needed to be able to reproduce
  and locate potential bugs. Ideally the error information should
  even mention the file and line where the error occurred. This can
  help developers to decide if there is a bug inside Elektra or if the
  problem lies somewhere else.
- Vast information is needed to support
  correct error handling in _other programming languages_.
  In languages supporting exceptions, class name, inheritance
  or interface information may be necessary. Language specific extensions
  are, however, not limited to exceptions. Other ways of handling
  errors are continuations or `longjmp` in C. A plugin is free to add,
  for example, `jmp_buf` information to the error information.

It is certainly not a good idea to put all this previously mentioned
information into a single string. Elektra chooses another way described
in the next chapter.

### Metadata

As stated above, a library always informs the user about what
has happened, but does not print or log anything itself. One way
to return error information is to add a parameter containing the
error information. In the case of Elektra, all `KDB` methods have a
key as parameter. This key is additionally passed to every plugin.
The idea is to add the error and warning information as metadata to
this key. This approach does not limit flexibility, because a key can
hold a potentially unlimited number of metakeys.

The error information is categorized in metadata as follows:

- [error] indicates that a faulty state is present. The value
  of the metakey contains the name of all the subkeys that are used for
  error indication. Metakeys do not guarantee any particular order on
  iteration. Instead, the user can find out the information by looking at
  this metavalue.

Additional metakeys yield all the details.

- [error/number] yields a unique number for every error.

- [error/description] is a description for the error to be displayed
  to the user. For example, the metavalue can hold the text “could not
  write to file”.

- [error/reason] specifies the reason of the error. The human
  readable message is in the metavalue of `error/reason`. It states why
  the error occurred. One example for it is ''no disc space available''.

- [error/module] indicates the name of the specific module or plugin.

- [error/file] yields the source file from where the error information
  comes.

- [error/line] represents the exact line of that source file.

Beside errors, Elektra can also emit warnings metadata. While only a single error can be set
on a specific error key, warnings can be up to 100 entries (#0 - #\_99):

- [warnings] indicate that at least one warning is present. The value of the metakey contains
  the number of warnings which can be accessed.

Additional metakeys yield all the details. The warnings are stored in a special array format
which range from 0 to \_99. E.g., the first warning number can be accessed by getting the key `warnings/#0/number`.
The following metadata is available and have the same semantics as the error metadata:
`[warnings/<number>/number]`, `[warnings/<number>/description]`,
`[warnings/<number>/reason]`, `[warnings/<number>/module]`,
`[warnings/<number>/file]`, `[warnings/<number>/line]`

If there are more than 100 warnings, the information will be overwritten from the start again.

As we see, the system is powerful because any other text or information
can be added in a flexible manner by using additional metakeys.

### Error Specification

The error specification in Elektra is written in simple colon-separated
text files. Each entry has a unique identifier and all the information
we already discussed above. No part of Elektra ever reads this file
directly. Instead, it is used to generate source code which contains
everything needed to add a particular error or warning information.
With that file we achieved a central place for error-related information.
All other locations are automatically generated instead of having
error-prone duplicated code. This principle is called “Don't repeat
yourself”.

In Elektra’s core and plugins, C macros are responsible for setting
the error information and adding warning information. In C only a
macro is able to add the file name and line number of the source code.
In language bindings other code may be generated.

### Sources of Errors

`Key` and `KeySet` functions cannot expose more error information than the
error code they return. But, of course, errors are also possible in these
functions. Typically, errors concern invalid key names or null pointers.
These problems are mostly programming errors with only local effects.

The most interesting error situations, however, all occur in `KDB`.
The error system described here is dedicated to the four main `KDB`
functions: `kdbOpen()`, `kdbGet()`, `kdbSet()` and `kdbClose()`.
The place where the configuration is checked and made persistent is
the source of most error information. At this specific place a large
variety of errors can happen ranging from opening, locking up and saving
the file. Sometimes in plugins, nearly every line needs to deal with
an error situation.

## Exceptions

Exceptions are a mechanism of the language and not just an implementation
detail. Exceptions are not intended to force the
user to do something, but to enrich the possibilities. In this section,
we discuss two issues related to exceptions. On the one hand, we will
see how Elektra supports programming languages that provide exceptions.
On the other hand, we will see how the research in exceptions helps
Elektra to provide more robustness.

### Language Bindings

C does not know anything about exceptions. But Elektra was designed
so that both plugins and applications can be written in languages
that provide exceptions. One design goal of Elektra’s error system is to transport
exception-related information in a language neutral way from the plugins
to the applications. To do so, a language binding of the plugin needs
to catch every exception and transform it into error information and
return an error code.

Elektra recognizes the error code, stops the processing of plugins,
switches to a faulty state and gives all the plugins a chance to do the
necessary cleanups. The error information is passed to the application
as usual. If the application is written in C or does not want to
deal with exceptions for another reason, we are finished because the
application gets the error information inside metadata as expected.
But, if the application is written in another language, the binding
translates the error code to an exception and throws it. It is worth
noting that the source and target language do not need to be the same.

Such a system needs a central specification of error information.
We already introduced such a specification file in error
specification. The exception classes and converters can be generated
from it. An exception converter is either a long sequence
of try-catch statements that transforms every known exception into an
appropriate metakeys. Each exception thrown by the plugin has to be
caught. Alternatively, a converter can be a long switch statement for
every error number. In every case the appropriate exception is thrown.

The motivation for using exceptions is that in C every return value has
to be checked. On the other hand, the C++ exception mechanism allows
the programmer to throw an exception in any place and catch it where it
is needed. So in C++ the code is not cluttered with parts responsible for
error handling. Instead, in a single place all exceptions of a plugin can
be transformed to Elektra’s error or warning information. The code for
this place can be generated automatically using an exception converter.

Applications not written in C can also benefit from an exception
converter. Instead of using the metadata mechanism, the error information
can be converted to the exception mechanism already used for that
application. We see that Elektra is minimally invasive in this regard.

### Exception Safety

We can learn from the way languages define the semantics for
**exception safety**. Exception safety is a concept which
ensures that all resources are freed regardless of the chosen return
path. **Basic guarantees** make sure
that some invariants remain on an object even in exceptional cases.
On the other hand, **strong guarantees** assure
that the investigated operation is successful or has no effect at all.
Methods are said to be exception safe if the object remains in a valid
state. The idea of exception safety is to ensure that no resource
leaking is possible. `kdbSet()` written in C++ would look like:

```c++
try {
	plugin[1].set(); // may throw
	plugin[2].set(); // may throw
	plugin[3].set(); // may throw
	...

	plugin[PLUGIN_COMMIT].set(); // now all changes are committed
} catch (...) {
	// error situation, roll back the changes
	plugin[1].error(); // does not throw
	plugin[2].error(); // does not throw
	plugin[3].error(); // does not throw
	...

	// now all changes are rolled back
	return -1;
} // now do all actions on success after commit
plugin[POSTCOMMIT].set(); // does not throw
...
return 1; // commit successful
```

This pseudocode is much clearer than the corresponding C code. Let us
explain the guarantee Elektra provides using this example. One by one
plugin gets its chance to process the configuration. If any plugin fails,
the semantics resemble that of a thrown exception. All other plugins
will not be executed. Instead, the plugins get a chance to recover from
the faulty state. In this catch part, the plugins are not allowed to
yield any error information, but they are allowed to add warnings.

Continue reading [with the algorithm](algorithm.md).
