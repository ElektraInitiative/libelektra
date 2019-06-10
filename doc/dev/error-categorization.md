# Error Categorization

This document explains how to categorize errors and should act as a form of
guideline. The categorization aims to reduce duplicated errors and the
maintenance effort for errors.

## Categorization Mindset

Errors along with their unique code only primarily exist because they can be
**reacted differently on programmatically**. To get an idea of programmatic
reactions take a method call which returns a `Timeout` error. Naturally a
senseful reaction would be to retry at a later point in time. So the reaction
here would be to retry with a time based approach. On the other hand it does not
make sense to differentiate between `No Write Permission` and `No Read Permission`
as the application just knows that it simply cannot access the desired resource
and tells the user to grant it.

Categories are hierarchically structured. In some categories you cannot put an
error such as `Permanent errors` (see below) because they are too general and
developers should choose a more specific category. Please choose the most
specific category as possible when trying to assign an error to a category.

If you feel for a new category, please forge a design decision document and make
a PR to Elektra's repo.

## Error categorization Guideline

Now we will investigate each category in more detail and when to put an
error/warning in there.

For a complete structural overview please visit the corresponding [design
decision document](../decisions/error_codes.md)

### Permanent errors ("C01000")

The branch category `Permanent Errors` refer to such errors which cannot be
fixed by retry at all. `Permanent Errors` are subdivided into

- Resource
- Installation
- Logical

#### Resource ("C01100")

`Resource Errors` are all kind of permission, existence and resource errors
which are essential for Elektra to operate. Resource errors is a branch category
which also allows for errors to be put in. Compared to validation errors this
category has its focus the underlying system resources whereas validation errors
for usages with specifications. Examples are missing files/ directories or
insufficient permission to execute certain commands (eg. you would require sudo
permissions). Reactions are fixing the permissions, remount, creating the
file/directory and retry the operation. Compared to validation errors,
administrators would change the specification or retry with a different value.

##### Memory Allocation ("C01110")

`Memory Allocation Errors` are special resource errors which come from failed
`elektraMalloc` calls primarily as no more memory could be allocated for the
application. Errors with not enough hard disc space do not belong here but into
`Resource`. Such errors will gain special handling in future releases and users
cannot deal with such errors as of now.

#### Installation ("C01200")

`Installation Errors` are errors that are related to a wrong installation such
as wrong plugin names, missing backends, initialization errors, misconfiguration
of Elektra etc. Installation errors might also be non-Elektra specific but also
from dependent library/applications such as gpg. Also plugin configuration
errors belong to `Installation Errors` as this happens during mounting. Users
will have to reconfigure, reinstall, recompile (with other settings) Elektra in
order to get rid of this error or fix the installation of the corresponding
library/application.

#### Logical ("C01300")

`Logical Errors` is a branch category in which you indicate a logical flaw in
the code such as internal errors, not implemented features, passing illegal
parameters to functions, plugins which do not behave accordingly (wrong return
codes, uncaught exceptions) or errors which should not happen such as going into
a `default` branch when you are assured that all cases are covered. Usually such
errors come with a message to report such failures to Elektra's bugtracker.
Applications cannot handle such errors themselves.

##### Internal ("C01310")

`Internal Errors` are such errors which indicate a flaw or bug in the internal
logic of Elektra. Examples are going into a `default` branch which you do never
expect to happen. Another use case is if you use an external library and have to
catch a generic exception. If you can however, catch the most specific
exceptions and convert them into the appropriate category. If you have to use
this error please add a message indicating that this bug should be reported.

##### Interface ("C01320")

`Interface Errors` indicate a wrong usage of Elektra's API. Compared to
`internal` errors this category does not indicate a bug but a misuse. An example
would be to pass a NULL pointer to `kdbGet`. Also violations of the backend
belong into this category. Compared to semantic validation errors, this category
has its focus on detecting wrong usages of the API instead of a "retry with a
different value" approach. Also validation errors focus on specifications of
configurations whereas this category tries to handle specifications for APIs.
Users should investigate the concrete reason and might use a different, more
appropriate method or change their passed values before retrying.

##### Plugin Misbehavior ("C01330")

`Plugin Misbehavior Errors` indicate that a plugin does not behave in an
intended way. Unrecognized commands, unknown return codes, plugin creation
errors, etc. belong to this category. Also uncaught exceptions belong here
because Elektra expects all exceptions to be caught. For wrong plugin versions
please use `Installation` errors. Users can try to remount, recompile the plugin
under different options or use a different plugin (e.g., switching to `mini`
instead of `ini`).

### Conflicting State ("C02000")

`Conflicting State Errors` are errors where the current state is incompatible
with the attempted operation. These kind of errors are usually in resolver
plugins when the state of the file has changed without the system knowing. An
example would be to try to push your changes into a git repository where the
remote branch has already changed. Try to synchronize your internal state and
retry to get rid of this error. Examples are the need for calling `kdbGet`
before `kdbSet`.

### Validation ("C03000")

`Validation Errors` are heavily used for Elektra's `configuration specification`
feature and should tell users that their given input does not match a certain
pattern/type/expected semantic etc.

Validation errors can either be syntactic or semantic.

#### Syntactic ("C03100")

`Syntactic Errors` are errors which tell users or applications that the current
format is not valid. Examples are wrong date formats or missing closing brackets
`]` inside of a regular expression when it is checked. Also path related errors
like missing slashes come into this category. Parsing errors are also associated
with syntactic errors such as unexpected encounters like missing line endings,
illegal characters or wrong encodings. Also transformation and conversion errors
are to be categorizes here because the format of the given input does not allow
such actions. Since syntactic errors demand a specific format and structure,
also structural validation errors belong here. Users should try a different
value/format and retry setting it with Elektra.

#### Semantic ("C03200")

`Semantic Errors` are errors which indicate a misunderstanding of the intended
meaning between a user's/developer's/administrator's way of seeing a setting and
the application's one. The main focus of this category is to enforce
specifications compared to other categories. So if users provide input which do
not match prespecified criterias even though syntactically everything is valid,
this category should be used. Examples are references to non-existent keys
(`reference` plugin), setting values to keys which require to be empty
(`required` plugin), wrong provided values in a specification if you restricted
the values (`enum` plugin), etc.

Users should try a different value or fix the underlying specification and retry
again.

## Underlying design decision document

- [Decision Document](../decisions/error_codes.md) The underlying design
  decision document.
