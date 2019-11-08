# Introduction

After the huge effort to harmonize and standardize our error message & code system, there is a great likelihood
that any newly added message will again violate these standards. The only remedy as of now are good PR reviews
which again are prone to human errors. I want to present common pitfalls I have seen when correcting
these messages and afterwards show potential remedies which are open for discussion.

# Problems

1. Inconsistent Error Messages

   1. Too advanced vocabulary

      Many error and warning messages included words which are too advanced for normal users.
      Examples are stat, normalize, glob, uncommon acronyms, etc.

   2. Forgetting single quotes around %s expressions

      As written in the error message design guideline, string formatters with %s have to be surrounded by single quotes so users
      know where they start and end as variables with spaces do not make it clear otherwise.

   3. Inconsistent sentence pattern

      The error message design guideline states that sentences have to start with an upper case letter and should not contain a dot at the end.
      Also exclamation marks should not be used as it is interpreted as shouting to the user.

   4. Leaking internals

      Some messages I have seen state which internal method has failed. E.g., in LCDproc there was a message `xosd: xosd_set_font() failed`. This though should be of no relevance for the user.
      "Internal" means variable names and method names.

2. Forgetting crucial error information

   'errno' usually contains a very important message for the user but might have been forgotten by the developer to add.

3. Forgetting key or key value information in error messages

   In many cases the key name (keyName) was not given by the error message writer. This though is needed for users to locate the invalid configuration setting value

# Potential remedies

## Problem 1

These problems should be checked by the build server and let the build fail if something is not conforming. This would require a test
that grabs all error messages and analyzes them for certain points. 1.2. and 1.3. are very easy to check by providing a regular
expression that has to match.

Another way to address 1.2 and 1.3 beforehand are message templates which could be used by developers. Personally I think this is difficult
to maintain and new developers will surely do not see and use it (right away). Elektra already has many things which developers should know
when they want to participate and I think this should be kept as few as possible.
For out of memory errors though I would suggest to rewrite the method signature to not provide any text as parameter but take a standard error text which contains the errno as this is clearly identical in all cases.

1.1 is more difficult to address as it would require some sort of blacklist which gets expanded as reviews say which words are too advanced. This is though a larger project as every word has to be stemmed in the error message and then compared to a list with also stemmed
words inside. Otherwise decent error message would trigger a build fail (e.g., "Could not generate statistics" would trigger a fail if the word "stat" was on the blacklist) or illegal words pass the build server (e.g., "Normalizing did not work" if "normalize" was on the blacklist).

1.4 is probably the least important problem as this problem happened quite rarely. This would require a grep for all method names and see
if it is not contained in the error message.

The one problem I see here is how to get all error messages in the src folder.
I have forged the following perl regex which could be used to get most of the error messages:
`grep -rP 'ELEKTRA_(SET|ADD)_[A-Z_]+_(WARNING|ERROR)[F]*\s*\(\n*.*?"\K(.*\n*)(?=")' src/**`
This though only gets 562 error messages while actually 671 are present. The difference comes from variables which are inserted
that contain the actual error message. We would probably need to patch those messages to not use variables anymore but the varargs.

Errnos or library error messages may always violate these standards and are impossible to catch but the proposed solution above greatly
improves the current situation with inconsistencies in the future.

## Problem 2

Since errno is a global thread local variable we may also access it in [here](https://github.com/Piankero/libelektra/blob/2fc587f163d1fef087977d7187b8295aadba6dfa/src/libs/elektra/errors.c#L85) and append it to the error message (e.g., Additional information: strerror(errno)). Alternatively this information could also be set in an extra metadata and only be shown if the command line arguments -v or -d are used. This would avoid message duplication if the developer already provided the errno himself.

## Problem 3

To remedy this problem I would suggest to rewrite the method signature to additionally provide the key which caused the problem beside the
parent key. With the -v or -d argument, the additional information can then be shown to help the user/admin locate the error. This especially
counts towards validation errors but in some cases
there is no problematic key (elektraOpen) and the parent key should be provided twice.
