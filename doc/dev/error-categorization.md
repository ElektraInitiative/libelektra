# Error Categorization

This document explains how to categorize errors and should act as a form of guideline.
The categorization aims to reduce
duplicated errors and the maintenance effort for errors.

## Categorization Mindset

Errors along with their unique code only primarily exist because they can be **reacted
differently on programmatically**.
To get an idea of programmatic reactions take a method call which returns a `Timeout` error. Naturally
a senseful reaction would be to retry at a later point in time. So
the reaction here would be to retry with a time based approach. On the other hand
it does not make sense to differentiate between `No Write Permission` and `No Read Permission` as
the application just knows that it simply cannot access the desired resource and tells
the user to grant it.

Categories are leaf based, so you cannot put an error
into a node (branch) such as `Permanent errors` (see below).

If you feel for a new category,
please forge a design decision document and make a PR to Elektra's repo.

## Error categorization Guideline

Now we will investigate each category in more detail and when to put an error/warning in there.

For a complete structural overview please visit the corresponding
[Design decision Document](../decisions/error_codes.md)

### Permanent errors ("C01000")

The branch category `Permanent Errors` refer to such errors which cannot be fixed by retry
at all. `Permanent Errors` are subdivided into

- Resource
- Installation
- Logical

#### Resource ("C01100")

`Resource Errors` as a branch category are all kinds of errors which are either permission related, existence related
or missing resources such as memory, RAM, etc.
Examples are missing files, insufficient permissions or out of memory.
This category forces users or applications to provide additional resources, create a missing file/ directory etc
or to provide the relevant permissions.

##### Memory Allocation ("C01110")

`Memory Allocation Errors` are errors which come from failed `elektraMalloc` calls primarily as no
more memory could be allocated for the application. Such errors will gain special handling
in future releases.

##### General Resource ("C01120")

`General Resource Errors` are all kind of permission and existence errors. Examples are
missing files/ directories or insufficient permission to execute certain commands (eg. you
would require sudo permissions). Reactions are fixing the permissions or creating the file/directory
and retry the operation.

#### Installation ("C01200")

`Installation Errors` are those errors which are related to a wrong installation such as
wrong plugin names, missing backends, initialization errors, misconfiguration of Elektra etc.
Installation errors might also be non-Elektra specific but also from dependent library/applications
such as gpg.
Also plugin configuration errors belong to `Installation Errors` as this happens during
mounting.
Users will have to reconfigure, reinstall, recompile (with other settings) Elektra in order to
get rid of this error or fix the installation of the corresponding library/application.

#### Logical ("C01300")

`Logical Errors` is a branch category in which you indicate a bug in Elektra
such as internal errors, assertion failures, not implemented features or errors
which should not happen such as going into a `default` branch when you are assured that all cases
are covered. Usually such errors come with a message to report such failures to Elektra's bugtracker.
Applications cannot handle such errors themselves.

##### Assertion ("C01310")

`Assertion Errors` are such errors which indicate a flaw in the internal logic such as going into a `default`
branch which you do never expect to happen.
This category might be used in the future to automatically
issue a bug to our bugtracker. As of now if you have to use this error please add a message
indicating that this bug should be reported.

##### Interface ("C01320")

`Interface Errors` errors indicate a wrong usage of Elektra's API. An example would be to pass a NULL pointer to
`kdbGet`. Also violations of the backend belong into this category. Compared to semantic validation errors,
this category has its focus on detecting wrong usages of the API instead of a "retry with a different value" approach.

##### Plugin Misbehavior ("C01330")

`Broken Plugin Errors` errors indicate that a plugin does not behave in an intended way. Unrecognized commands,
unkown return codes, plugin creation errors, etc. belong to this category. Also uncaught exceptions belong here because
Elektra expects all exceptions to be caught.

### Conflicting State ("C02000")

`Conflicting State Errors` are errors where the current state is incompatible with the attempted operation.
Examples are the need for calling `kdbGet` before `kdbSet`. Another example would be to try to push you changes
into a git repository where the remote branch has already changed.
These kind of errors are usually in resolver plugins when the state of the file
has changed without the system knowing. Such errors demand a special synchronization action before retrying.

### Timeout ("C03000")

`Timeout Errors` are easy to categorize as the reaction simply suggests to wait for a short period and retry again.
Examples are lost connections to a server.

### Validation ("C04000")

`Validation Errors` are heavily used for Elektra's `configuration specification` feature and
should tell users that their given input does not match a certain pattern/type/expected semantic etc.

Validation errors can either be syntactic or semantic.

#### Syntactic ("C04100")

`Syntactic Errors` are errors which tell users or applications that the current format is not valid.
Examples are wrong date formats or missing closing brackets `]` inside of a regular expression when it is checked.
Also path related errors
like missing slashes come into this category. Also parsing errors fall under syntactic errors such as
unexpected encounters like missing line endings, illegal characters or wrong encodings. Also transformation
and conversion errors are to be categorizes here because the format of the given input does not allow such
actions. Users should try a different value and retry setting it with Elektra.

#### Semantic ("C04200")

`Semantic Errors` are a bit more tricky to solve programmatically. Examples are references to
non-existent keys (`reference` plugin), setting values to keys which require to be empty (`required` plugin),
wrong provided values in a specification if you restricted the values (`enum` plugin), etc.

The provided value differs from the developers/ administrators (specification writer) intention,
even though syntactically everything is valid. Users should try a different value and retry again.

### Out of Range ("C05000")

`Out of Range Errors` are operations which attempt to be past a valid range. Examples are trying to set
an Elektra Array entry to `-1` or trying to set a port outside of a valid range. There is quite some overlap
in semantic/syntactic/resource errors but these errors are primarily focused on Elektra arrays. It's use is for callers
who are iterating (or continually incrementing/ decrementing) through a space and can easily detect when they are
done by looking for this error.

## Underlying design decision document

- [Decision Document](../decisions/error_codes.md)
  The underlying design decision document.
