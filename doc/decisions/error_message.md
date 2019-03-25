# Error message & handling concept

## Problem

The current error concept has disadvantages in following regards:

* Too verbose error message
    Currently for every error, 9 lines are shown in which most of them are not relevant to end users/administrators. One goal
    is to reduce the verbosity of such messages and let users/administrators see only information they need.
* A lot of redundant errors
    At the moment, each new plugin introduces new error codes which led to about 210+ error codes. Many of those errors
    are duplicated because developers did not know or search for a similar error which is already present. This concept should
    group similar errors together so that there is one coherent and consistent state again.
* Hard to manage specification file
    Since every developer adds its own error individually, a lot of merge conflicts happen which makes contributing to the codebase
    unpleasant. Additionally, if you want to reuse any error you have to scrape to the whole file with ~1300+ lines. As there is no
    senseful ordering or scheme behind the errors (since they grew by time), it is a hassle to find the correct error code. 
    The new concept should standardize errors, making it easy to categorize errors from new plugins and avoid merge conflicts. 
* No senseful way for application developers to use error codes from elektra
    If developers of plugins/ external tools using elektra want to react to errors, they have to be very specific. At the moment there is
    no possibility to catch all errors easily which force a certain behavior. Eg. if there happens a temporary recoverable error, developers have to
    catch for every specific error code rather than a general hierarchical error. The new concept should make it easy to react to errors as they are
    sensefully grouped together and are hierarchically structured.

## Constraints

* Error numbers must stay because they are more reliable to check against than strings
* Supporting multiple programming languages
* Plugin System

## Assumptions

## Considered Alternatives

* Removing the specification file without requiring error numbers
* Possible variations on what message should be displayed, 
eg. to keep the mountpoint information or on how wordings should be (with or without 
"Sorry, ...", coloring of certain parts of a message, etc.)
* Adding the key of the occurred error to the API which permits reading information from
additional metadata such as an error message provided by a specification author. 
Reason against: The description of the key should already provide such information.
Doing it in an extra key would imply redundant information.
* Removal of warnings with error codes from the specification file.

Various FLOSS projects:
* [GStreamer](https://github.com/GStreamer/gstreamer): 
    This project uses 4 domain type errors which are suited to their project:
    CORE, LIBRARY, RESOURCE or STREAM. Every domain type has further sub error codes which are numbered from 1-x where 1 is a
    general purpose error "FAILED" which should be used instead of inventing a new error code (additional enum). You can see an example
    of enum errors [here](https://github.com/GStreamer/gstreamer/blob/a7db80f9a98287f012108845e121f6f6fb62171b/gst/gsterror.h#L63-L80)
* [Apache httpd](https://github.com/apache/httpd):
    This project does not use any error codes at all. They solely rely on the printed message and pass various other information along like
    file, line, level, etc. The primary function they use can be seen [here](https://github.com/apache/httpd/blob/1acebd4933e5315c669605c3c9222ed8bb0ee9ea/include/http_log.h#L378-L403)
* [Jenkins](https://github.com/jenkinsci/jenkins):
    Since Jenkins is a java project they have inheritance of errors by nature. They mostly use reaction based Exception such as
    `MissingDependency`, `RestartRequired`, `FormFillValidation`, `BootFailure`, etc. Some exceptions even have more concrete
    exceptions such as a `NoTempDir` which inherits from `BootFailure`. A very similar approach will be implemented by Elektra,
    except that it is a C project and will use error codes.
* [Postgresql](https://github.com/postgres/postgres):
    Postgres has one of the most advanced error concepts among all investigated projects. It also uses one bigger [specification file](https://github.com/postgres/postgres/blob/master/src/backend/utils/errcodes.txt) which is parsed and generates multiple header files. Also noteworthy is that they once had multiple files containing error codes and
    merged them into a single one (commit [#ddfe26f](https://github.com/postgres/postgres/commit/ddfe26f6441c24660595c5efe5fd0bd3974cdc5c)). Errors are a string
    made up of 5 chars, where the first two chars indicate a certain class. This follows the SQLSTATE conventions. 
    Currently they have 43 classes which all come from SQLSTATE. Postgres also throws additional errors but have to subclass it to one of the current 43 classes and have a special naming convention which have to start with a `P` in the subclass.
* [etcd](https://github.com/etcd-io/etcd): 
    Etcd's approach for errors are tightly coupled to the programming language Go as well as the [gRPC](https://grpc.io/) standard which currently has 
    [16 codes](https://godoc.org/google.golang.org/grpc/codes) defined. Some of these errors are similar or identical to those which will be used in elektra.
    Every error of etcd is associated with one of these categories and gets its own error message which is specified in [this](https://github.com/etcd-io/etcd/blob/master/etcdserver/api/v3rpc/rpctypes/error.go) file. This concept though does not allow easy subclassing which might be useful (eg. further split FailedPrecondition into more specific errors like semantic and syntactic errors)

## Decision

The error message has the current format:

```
The command kdb set failed while accessing the key database with the info:
Sorry, the error (#121) occurred ;(
Description: validation failed
Reason: Validation of key "<key>" with string "<value>" failed.
Ingroup: plugin
Module: enum
At: ....../src/plugins/enum/enum.c:218
Mountpoint: <parentKey>
Configfile: ...../<file>.25676:1549919217.284067.tmp
```

The new default error message will look like this:
```
Sorry, plugin <PLUGIN> issued error #<NR>:
Validation of key "<key>" with string "<value>" failed.
```
The error #<NR> will be the color red while <PLUGIN> will be the color blue.

Optionally a third line indicating a solution can be added. Eg. for a permission related error there would be a third line:
```
Possible Solution: Retry the command as sudo (sudo !!)
```

To avoid losing information, the user can use the command line argument `-v` (verbose) to show
`Mountpoint`, `Configfile` in addition to the current error message.
Furthermore a developer can use the command line argument `-d` (debug) 
to show `At` for debugging purposes.

All "fatal" errors will be converted to "errors" as the distinguishment is not relevant.

Unused marked errors will be removed from the specification.

The remaining ~130 errors will be categorizes into logical groups with subgroups.
Each group and subgroup will receive a range of numbers such as in the HTTP protocol.

* Permanent errors (1-500)
    * Resource (1-50)
    * Parsing (51 - 100)
    * Installation (101 - 150)
    * Logical (151 - 200)
* Temporary (501 - 1000)
    * Conflict (501 - 600)
    * Timeout (601 - 701)
* Validation (1001 - 1500)
    * Syntactic (1001 - 1100)
    * Semantic (1101 - 1200)

## Rationale

The new error message is much more succinct which gives end users more relevant information.
Furthermore the solution approach still holds all necessary information if requested by users.

A new metakey `error/solution` will be implemented and attached to the parentkey via the new API.
`Ingroup`, `Description` and `Module` will be removed from the error message as they provide no useful
information at all.

The grouping of errors will allow developers to filter for specific as well as more general errors to correctly
react to them programmatically. Even though there are currently just 8 categories they will reserved up to 1500 error numbers.
This will permit additional subgrouping of errors in case it might be needed in the future. Imagine the case where
"Recourse" errors is too general because developers saw a need for splitting the errors in "permission" and "existence" errors.
They can simply take the current 1-50 numbers and make "permission" range from 1-25 and "existence" to 26-50. This will also allow
backwards compatibility by applications just checking for recourse errors in general.
Splitting/merging/rearranging any category should only be done by a decision (such as this file here) because elektra developers
should not be able to generate a new category as they wish because it would lead to the same proliferation of errors as we have now.

Warnings will be removed from the specification file. Any current warning will use the function
```
elektraAddWarning(Key * parentKey, const char * message)
```
Note that no error number is present anymore as it is not needed.

The API for the errors will be extended as following:
```
// Already present
ELEKTRA_SET_ERROR (nr, parentKey, message);
ELEKTRA_SET_ERRORF(nr, parentKey, message, ...);

// Extended API
ELEKTRA_SET_ERROR_WITH_SOLUTION(nr, parentKey, solution, message)
ELEKTRA_SET_ERROR_WITH_SOLUTIONF(nr, parentKey, solution, message, ...)
```

The vararg will affect the message and not the solution such as before.


## Implications

The specification file will stay but should be untouched in most of the cases in the future. Also the C++ code generation
file which uses the specification will stay as it is easier to change categories. The elektra warning part will be removed though.

Current errors will be migrated. The migration of each and every error can be seen here: [google docs](https://docs.google.com/spreadsheets/d/1-vXNZ7pN9wlMFByIMLbFt_HieyJpop0UyksbgAvmGK4/edit?usp=sharing).

## Related decisions


## Notes


