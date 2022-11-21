# Error codes

## Problem

The current error concept has disadvantages in following regards:

- A lot of redundant errors:
  At the moment, each new plugin introduces new error codes which led to about 210+ error codes.
  Many of those errors are duplicated because developers did not know or search for a similar error which is already present.
  This concept should group similar errors together so that there is one coherent and consistent state again.
- Hard to manage specification file:
  Since every developer adds its own error individually, a lot of merge conflicts happen which makes contributing to the codebase unpleasant.
  Additionally, if you want to reuse any error you have to scrape to the whole file with ~1300+ lines.
  As there is no senseful ordering or scheme behind the errors (since they grew by time), it is a hassle to find the correct error code.
  The new concept should standardize errors, making it easy to categorize errors from new plugins and avoid merge conflicts.
- No sensible way for application developers to use error codes from Elektra:
  If developers of plugins/ external tools using Elektra want to react to errors, they have to be very specific. At the moment there is
  no possibility to catch all errors easily which force a certain behavior. Eg. if there happens a temporary recoverable error, developers have to
  catch for every specific error code rather than a general hierarchical error. The new concept should make it easy to react to errors as they are
  sensefully grouped together and are hierarchically structured.

## Constraints

- Error codes/numbers must stay but can be changed to another format (eg. Strings)
- Supporting multiple programming languages
- Supporting Elektra's Plugin System

## Assumptions

## Considered Alternatives

- Removing the specification file without requiring error numbers
- Adding the key of the occurred error to the API which permits reading information from additional metadata such as an error message provided by a specification author.
  Reason against: The description of the key should already provide such information.
  Doing it in an extra key would imply redundant information.
- Removal of warnings with error codes from the specification file.

Various projects and standards:

- [GStreamer](https://github.com/GStreamer/gstreamer):
  This project uses 4 domain type errors which are suited to their project:
  CORE, LIBRARY, RESOURCE or STREAM. Every domain type has further sub error codes which are numbered from 1-x where 1 is a
  general purpose error "FAILED" which should be used instead of inventing a new error code (additional enum). You can see an example
  of enum errors [here](https://github.com/GStreamer/gstreamer/blob/a7db80f9a98287f012108845e121f6f6fb62171b/gst/gsterror.h#L63-L80)
- [Apache httpd](https://github.com/apache/httpd):
  This project does not use any error codes at all. They solely rely on the printed message and pass various other information along like
  file, line, level, etc. The primary function they use can be seen [here](https://github.com/apache/httpd/blob/1acebd4933e5315c669605c3c9222ed8bb0ee9ea/include/http_log.h#L378-L403)
- [Jenkins](https://github.com/jenkinsci/jenkins):
  Since Jenkins is a java project they have inheritance of errors by nature. They mostly use reaction based Exception such as
  `MissingDependency`, `RestartRequired`, `FormFillValidation`, `BootFailure`, etc. Some exceptions even have more concrete
  exceptions such as a `NoTempDir` which inherits from `BootFailure`. A very similar approach will be implemented by Elektra,
  except that it is a C project and will use error codes.
- [Postgresql](https://github.com/postgres/postgres):
  Postgres has one of the most advanced error concepts among all investigated projects. It also uses one bigger [specification file](https://github.com/postgres/postgres/blob/master/src/backend/utils/errcodes.txt) which is parsed and generates multiple header files. Also noteworthy is that they once had multiple files containing error codes and
  merged them into a single one (commit [#ddfe26f](https://github.com/postgres/postgres/commit/ddfe26f6441c24660595c5efe5fd0bd3974cdc5c)). Errors are a string
  made up of 5 chars, where the first two chars indicate a certain class. This follows the SQLSTATE conventions.
  Currently they have 43 classes which all come from SQLSTATE. Postgres also throws additional errors but have to subclass it to one of the current 43 classes and have a special naming convention which have to start with a `P` in the subclass.
- [etcd](https://github.com/etcd-io/etcd):
  Etcd's approach for errors are tightly coupled to the programming language Go as well as the [gRPC](https://grpc.io/) standard which currently has
  [16 codes](https://pkg.go.dev/google.golang.org/grpc/codes?utm_source=godoc) defined. Some of these errors are similar or identical to those which will be used in Elektra.
  Every error of etcd is associated with one of these categories and gets its own error message which is specified in [this](https://github.com/etcd-io/etcd/blob/master/etcdserver/api/v3rpc/rpctypes/error.go) file. This concept though does not allow easy subclassing which might be useful (eg. further split FailedPrecondition into more specific errors like semantic and syntactic errors)
- [Windows Registry](https://docs.microsoft.com/en-us/windows/desktop/sysinfo/registry):
  The registry does not use any specific error concept but takes the standard [Win32 Error Codes](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/18d8fbe8-a967-4f1c-ae50-99ca8e491d2d). These are neither hierarchical nor have any special ordering. Basically it is the same as Elektra has now except for no duplicated
  errors.
- macOS X plist:
  Just like Windows, plist uses standard macOS X errors which is a [huge catalog](http://krypted.com/lists/comprehensive-list-of-mac-os-x-error-codes/) of unordered
  return codes as integers.
- [SNMP Standard](http://www.snmp.com/protocol/):
  Being a standard network protocol, error codes are very specific to the domain itself. A list can be found [here](https://docs.microsoft.com/en-us/windows/desktop/snmp/snmp-error-codes) and would not meet the needs of Elektra at all.
- POSIX:
  Returning a non-zero value and retrieving the concrete information from `errno` would not suffice for Elektra as it is too simple. It would not solve any of our current
  problems like having excessive uncategorized codes for errors.

## Decision

All "fatal" errors will be converted to "errors" as the distinction is not relevant.

Unused errors will be removed from the specification.

Errors will be categorized into logical groups with subgroups.
Each error will be made up of 5 characters, where the first 2 character indicate the highest level and character 3 to 5 will be used for subgrouping. Errors are prepended with the letter `C` which is the abbreviation for "Code".

- Permanent errors C01000
  - Resource C01100
    - Out of Memory C01110
  - Installation C01200
  - Logical C01300
    - Internal C01310
    - Interface C01320
    - Plugin Misbehavior C01330
- Conflicting State C02000
- Validation C03000
  - Syntactic C03100
  - Semantic C03200

To see an explanation of the categories along with a guideline on how to categorize please see the [Error Codes Guideline](../../dev/error-categorization.md)

## Rationale

The grouping of errors will allow developers to filter for specific as well as more general errors to correctly react to them programmatically.
The new concept will permit additional subgrouping of errors in case it might be needed in the future.

Splitting/merging/rearranging any category should only be done by a decision (such as this file here).
Elektra developers should not be able to generate a new category as they wish because it would lead to the same proliferation of errors as we had before.

These categories are chosen because they can help developers to react programmatically and cover the majority of use cases to our present knowledge.
If there is ever the need for another reaction based category, it can be extended very easily.

## Implications

The specification file will stay but should be untouched in most of the cases in the future.
Also the C++ code generation file which uses the specification will stay as it is easier to change categories.

Current errors will be migrated.

## Related Decisions

- [Error Message Format](error_message_format.md)
  Shows the new format of the error message
- [Error Codes Guideline](../../dev/error-categorization.md)
  Shows how to categorize errors

## Notes
