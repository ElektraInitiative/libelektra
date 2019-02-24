# Error Message

## Problem

1. Too many errors in the [specification](src\error\specification) (210 errors/warnings)
2. Many duplicate or redundant errors
3. Description/ Reason are very similar
4. Specification file modularity (1327 LOC)
5. Verbose error message
6. strerror sometimes is not part of the reason, sometimes (not) in quotes, sometimes \n is at the end

Furthermore:

1. Some errors have the same text but are duplicated because one is `warning` and the other one is `error`/`fatal` 
(#153 + #154, #128 + #129, #120 + #41, #44 + 45 , #66 + #67, etc.)
2. Some errors might be duplicated just because they belong to different plugins (or the dev neglected to lookup a similar in the specification) 
(#29 + #57 + #72 + #169, #10 + #61, etc.)
3. Many errors are very similar or could be summarized and are differentiated via the reason field 
(#54 + #55, #31 - #33, #93 + #7 + #17, #74 + #75, #77 + #10 + #61, etc.)
4. Some errors are marked as unused (#22 + #48 + #58 + #66 + #67 + #68)
5. In many cases the `Description` is not very relevant at all (#199, #196, #187, etc.)

## Constraints

## Assumptions

From now on I will take the following error message as example:

```
1. The command kdb set failed while accessing the key database with the info:
2. Sorry, the error (#121) occurred ;(
3. Description: validation failed
4. Reason: Validation of key "<key>" with string "<value>" failed.
5. Ingroup: plugin
6. Module: enum
7. At: ....../src/plugins/enum/enum.c:218
8. Mountpoint: <parentKey>
9. Configfile: ...../<file>.25676:1549919217.284067.tmp
```

The lines are numbered and will be referred in the following sections.

Generally I would suggest that this error message is way too much information for a user. We can agree on the fact 
that #4 is by far the most relevant line.

### Line 9: Configfile

Reasons to keep it:
* If users want to see where the concrete file is located 

Reasons against it:
* Very verbose line
* I highly doubt that most common users will require this information (unnecessary)

Suggestions:
Remove it for standard error messages. Maybe a `kdb set -v` could include such information for developers, users. 
If users want to know the location they furthermore can use `kdb file` on the concrete setting.

### Line 8: Mountpoint

Reasons to keep it:
* If users want to see the parent key of their configuration
* If I remember correctly: all error information is saved to this key, so users could investigate further error
information which is saved to this key (in the metadata)

Reasons against it:
* Another extra line in the error message.
* I highly doubt that most common users will require this information (unnecessary)

Suggestions:
Remove it for standard error messages. Maybe a `kdb set -v` could include such information for developers, users.
Alternatively there could be a command for users to see the mountpoint, eg. `kdb get-mountpoint <key>`

### Line 7: At

Reasons to keep it:
* Debug purposes

Reasons against it:
* Another extra line in the error message.
* I highly doubt that most common users will require this information (unnecessary)

Suggestions:
Remove it for standard error messages. Maybe a `kdb set -v` could include such information for developers, users.
Alternatively, another possibility would be to have a command which reveals the last error message with more detail: 
`kdb last-message`.

### Line 6: Module

Reasons to keep it:
* See the concrete plugin which causes the failure
* Prohibits error masking (eg. `stat`ing a file happens in more plugins, devs will know where the error occurs)

Reasons against it:
* Another extra line in the error message.

Suggestions:
The need for plugin distinguishment in the error message should not be that high in my opinion. Maybe it would make sense to include it in line #1.

### Line 5: Ingroup

Reasons to keep it:
* Another higher layer of distinguishment for the error location

Reasons against it:
* Extra line & unnecessary

Suggestion:
Having both ingroup and module is too much information. Also again in most cases users should get the relevant information out of the reason line.
As of now there are 6 different "ingroup"s. I would suggest to remove it.

### Line 4: Reason

The most relevant line out of all, no question to keep it or not.
Still I would remove the prefix `Reason: ` because of the reduced error message length in the new format. It makes it clear
that this line is the reason. Probably we could substitute it with the plugin which called the error: 
`<plugin>: Could not set ...`

### Line 3: Description

Reasons to keep it:
* Gives users a quicker overview about the error (eg. permission error/ resource error/ etc. or another type of categorization such as Markus
suggested with temporary/permanent/conflict/etc.)
* Can indicate the type of solution a user has to approach

Reasons against it:
* Extra line
* What to do with "uncategorizable" errors
* Maintenance effort: introduce a new category once there are enough similar errors and change all old errors to this category?
* Awareness of developers: All devs have to know all categories and choose the correct one. Devs should also be prohibited to throw their error
into the "uncategorized" category due to lazyness.

Suggestion:
This line is by far the most discussable one. I thought about categorizations such as
* Permission/ Resource/ Parse/ External(C++, Ruby, Java)/.../ Miscellaneous:
Most errors are resource errors (could not stat, no connection, could not close/open/find etc.) This again would not give users
a particularly useful error indication since he/she has to lookup in the reason anyway. Even subdividing the `Resource` category
would lead to the same problem. No matter how I look at it, the user
will have to read the concrete reason in most cases which makes the Description line not that valuable any longer.
* temporary/permanent/conflict/validation:
This seems to be a better approach because it includes a possible solution for the user. How to categorize certain errors though
seems impossible because there can be multiple reasons where to categorize an error. Take `Could not open file` as an example:
if it is on a NFS and it is offline, *temporary* would be correct. If the file has too restrictive permissions, *permanent* would be more
appropriate since the admin has to fix it. If the file is currently in use and semlocked by the system, *conflict* would be appropriate.
* core kdb error/plugin error:
Not much value for the user
* differentiation to core features of elektra: Specification/ Notification/ KDB Core/ Bindings/ ...
An error is again difficult to put into one bucket since some errors might be affected by multiple features (could not stat file for example
which is used by multiple features).
This approach would lead to the same problem as we have now (many errors, difficult to maintain)

Personally I do not see how the benefit of a categorization outweighs the drawbacks because in by far the most cases users will have to look
into the reason in more detail anyway. It should be enough for users to deal with the error just by the reason.

As a result I would suggest to remove the description line.

### Line 2: Error code

Reasons to keep it:
* Probably could be used to categorize errors such it is done in the http protocol
* Good to use for shellrecorder scripts/unit tests to test for errors
* Possibility for automation (eg. if a certain error number occurred you could automatically trigger certain actions)

Reasons against it:
* Extra line
* String comparison is needed to check for which error it is (unit tests, shellrecorder). Maybe that is not really a problem.

Suggestion:
Giving an error a number like it is done in C programs as return code seems to be outdated for elektra, especially when
the reason is given anyway. Personally I never read the error number because it has not much value.
The *reasons to keep* will also be very limited when the number of errors are significantly reduced.
I would also suggest to remove it.

### Line 1: General sentence

All in all this sentence can stay because it gives the user a first hint on what happened.
I would though change the wording and include more relevant information in it.
I would change it like this:
"Module <plugin> issued a <error/warning/fatal> while accessing the key database with the info:"
where plugin should have a different color (blue?) and warning/error/fatal (yellow, red, dark red) too depending on the severity.

With this line 5/6 can still be present but in a much more efficient and succinct way.

### Error/Warning/Fatal etc. splitting in the specification file

I would suggest to remove this concept from the specification and let the developer use a respective method to indicate if it is a warning/error/etc.
The macros ELEKTRA_SET_ERROR, ELEKTRA_ADD_WARNING ... should be enough to make developers cautious of wrong method usage.
What I have seen is that the return code is the only matter if it is an error or not.

### Additional information which is not present

Some errors can also guide users to fix the error by suggesting a possible solution:
* Retry as sudo
* Fix file permissions
* Check if the file exists
* etc.
Developers though have to be very careful when suggesting solutions because it can easily mislead users. For many cases though, 
this can help users a lot.
I would suggest an optional extra line (which is only printed if the developer adds a metafield `solution` to the parentkey):
`Possible Solution: ...`

## Considered Alternatives

The alternatives are written in the respective subsections in the *Assumptions* area. There are simply too many possible
subsolutions (eg. which message to keep, wording, etc.) to write them all.

## Decision

With the new concept we would not require a `specification` file at all which helps developers immensely but also keeps the
most relevant information for the users. An error like this:

```
1. The command kdb set failed while accessing the key database with the info:
2. Sorry, the error (#121) occurred ;(
3. Description: validation failed
4. Reason: Validation of key "<key>" with string "<value>" failed.
5. Ingroup: plugin
6. Module: enum
7. At: ....../src/plugins/enum/enum.c:218
8. Mountpoint: <parentKey>
9. Configfile: ...../<file>.25676:1549919217.284067.tmp
```

would be changed to this:
```
1. Sorry, plugin enum issued a error while accessing the key database with the info:
2. Validation of key "<key>" with string "<value>" failed.
```

with optionally a third line indicating a solution. Eg. for a permission related error there would be a third line:
```
3. Possible Solution: Retry the command as sudo (sudo !!)
```

Additionally the `kdb set` command gets another command line option (`-v`, `--verbose`) which prints out more detailed information.
The `configfile`, `mountpoint` and `At` will be printed out in addition if the parameter is present:

```
kdb set -v /some/wrong/enum wrrong
1. Sorry, plugin enum issued a error while accessing the key database with the info:
2. Validation of key "<key>" with string "<value>" failed.
3. At: ...../enum.c:332
4. Mountpoint: /some/wrong
5. Configfile: ...../<file>.25676:1549919217.284067.tmp
```

## Rationale

## Implications

Benefits:
* Much more relevant information in the error message
* Ease of development:
no merge conflicts, no extra line in the `specification` file leading to chaos (such as it is now)
New plugins will not have to choose an existent error category or implement a new one which could potentially be wrong
* Less maintenance effort:
No consolidation, splitting, etc. of categories needed in future versions
No need for splitting the `specification` file because it is too large

Drawbacks:
* Possible small information loss by removing the description, `At` and `mountpoint`:
`At` could be kept for devs by extending the `kdb set` command with an option `-v`, `--debug`, etc.
The same applies for the `mountpoint`.
Description comes with too many drawbacks to be kept
* Possible misleading the user with the `Solution` line

The benefits do outweigh the drawbacks. I would suggest an extra method like the following:
```
// If a solution can be applied
elektraErrorWithSolution(Key parentKey, const * char message, const * char solution)

// If it is not advisable to have a solution
elektraError(Key parentKey, const * char message)
```

## Related decisions

## Notes