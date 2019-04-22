# Error Categorization

In this document we start to explain how to categorize errors.
Many errors are unique and can be difficult to categorize so this
document will act as a guideline.

## Categorization Mindset

Errors along with their unique code only exist because they can be **reacted
differently on programmatically**. Imagine a method call which returns a `Timeout` error. Naturally
a senseful reaction would be to retry at a later point in time. So
the reaction here would be to retry with a time based approach. On the other hand
it does not make sense to differentiate between `No Write Permission` and `No Read Permission` as
the application just knows that it simply cannot access the desired resource and tells
the user to grant it. One could argue that an application could always react differently
based on the error (eg. give yourself write or read permission) but such detailed granularity
is rarely needed and would come with a lot of maintenance effort. In the previous versions of Elektra
we were facing many duplicated errors because too many existed and developers simply cannot know each
and every code.

So if you feel the need for a new category you should first ask yourself the following question:

> Can I react differently if I would know every error X in category Y?

If not it will most likely fit into an existing category. Categories are leaf based, so you cannot
put an error into a node such as `Permanent errors` (see below).

## Error categorization Guideline

Now we will investigate each category in more detail and when to put an error/warning in there.

The current structure looks like this:

- Permanent errors ("01000")
  - Resource ("01100")
  - Parsing ("01200")
  - Installation ("01300")
  - Logical ("01400")
- Conflict ("02000")
- Timeout ("03000")
- Validation ("04000")
  - Syntactic ("04100")
  - Semantic ("04200")

### Permanent errors ("01000")

The main category `Permanent Errors` refer to such errors which cannot be fixed by retry
at all. `Permanent Errors` are subdivided into

- Resource
- Parsing
- Installation
- Logical

#### Resource ("01100")

`Resource Errors` are all kinds of errors which are either permission related, existence related
or missing resources such as memory, RAM, etc.
Examples are missing files, insufficient permissions or out of memory.
This category forces users or applications to provide additional resources, create a missing file/ directory etc
or provide the relevant permissions for Elektra to work.

#### Parsing ("01200")

`Parsing Errors` are reserved for parsers for xml/yaml/ini etc. and indicate errors which
were investigated during parsing. Such errors show unexpected encounters like missing line endings,
illegal characters or wrong encodings.
Users or applications will have to sanitize the input and retry again.

#### Installation ("01300")

`Installation Errors` are those errors which are related to Elektra's installation such as
wrong plugin names, missing backends, initialization errors, misconfiguration of Elektra etc.
Users will have to reconfigure, reinstall, recompile (with other settings) Elektra in order to
get rid of this error. Applications will most likely not be able to handle such errors by themselves.

#### Logical ("01400")

`Logical Errors` indicate a bug in Elektra such as internal errors, assertion failures or errors
which should not happen such as going into a `default` branch when you are assured that all cases
are covered. Usually such errors come with a message to report such failures to Elektra's bugtracker.
Applications cannot handle such errors themselves other than reporting it to Elektra and hope
that the developers investigate and fix the issue.

### Conflict ("02000")

`Conflict Errors` are errors which indicate temporary problems which can be handled by handled by
updating the current state. Examples are the need for calling `kdbGet` before `kdbSet`, or pulling changes
from git before pushing them. These kind of errors are usually in resolver plugins when the state of the file
has changed without the system knowing. Applications interacting with such an error usually know
how to handle them once approached such as the `highlevel API`.

### Timeout ("03000")

`Timeout Errors` are easy to categorize as the reaction simply suggests to wait for a short period and retry again.
Examples are lost connections to a server.

### Validation ("04000")

`Validation Errors` are heavily used for Elektra's `configuration specification` feature and
should tell users that their given input does not match a certain pattern/ type/ expected semantic/ etc.

Validation errors can either be syntactic or semantic.

#### Syntactic ("04100")

`Syntactic Errors` are errors which tell users or applications that the current format is not valid.
Examples are wrong date formats or invalid notations such as polish prefix notation. Also path related errors
like missing slashes come into this category. Applications should try a different format and retry pushing
it to Elektra.

#### Semantic ("04200")

`Semantic Errors` are a bit more tricky to solve programmatically. Examples are if users provide a `String`
where Elektra expects an `Integer` or are given server port is already in use in which the specification says
it has to be available. Applications should try a different value and retry again.
