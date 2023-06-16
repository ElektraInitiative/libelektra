# Implement new commands in C

Here we'll go over the basic requirements to implement a new command in C and integrate it in to the `kdb` CLI framework.

## 1. Files

First we need a header as well as a c file for the new command in `src/tools/kdb`.

### Header

The `<name>.h` has to look something like this:

```c
/**
 * @file
 *
 * @brief Header for ... command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_..._H
#define ELEKTRA_KDB_..._H

#include <kdb.h>

/**
 * Adds options specification of complete command to @spec
 *
 * @param spec the base spec where the commands spec should be added
 */
void add...Spec (KeySet * spec);

/**
 * Executes the ... command
 *
 * @param options cli options and arguments as specified in addCompleteSpec()
 * @param errorKey key where errors and warnings should be saved
 *
 * @retval 0 ... command ran without errors
 * @retval 1 errors occurred, keyGetMeta (errorKey, "error/reason") for info
 *
 */
int exec... (KeySet * options, Key * errorKey);

#endif // ELEKTRA_KDB_..._H
```

### `C` file

The `<name>.c` has to implement the two functions already described in the header file, in addition to those `COMMAND_NAME` has to be defined before `#include <command.h>`.
This is important because macros defined in `command.h` depend on `COMMAND_NAME` being defined.

```c
#define COMMAND_NAME "..." // your command name
...
#include <command.h>
...
```

In addition to `command.h`, it should also `#include <colors.h>`.
`command.h` and `colors.h` contain all the needed helper macros and functions.

#### Specification

`void add...Spec (KeySet * spec)` will contain the specification (for `elektraGetOpts`) of the new command.
It has to append it to the passed `KeySet * spec`, the framework will call this function to register its specification.
The function could look something like this:

```c
void addGetSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Get the value of an individual key.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/all", KEY_META, "description", "Consider all of the keys", KEY_META,
				   "opt", "a", KEY_META, "opt/long", "all", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}
```

The `ADD_BASIC_OPTIONS` at the end adds options that are common across all commands to its spec, this includes things like `verbose: -v` or `debug: -d`.

#### Execution

The `int exec... (KeySet * options, Key * errorKey)` receives two parameters, `options` and `errorKey`.
Where `options` contains the passed options (as parsed by `elektraGetOpts`) and arguments and `errorKey` is where any errors or warnings have to be written to so the framework can properly print them.
It should start with the `GET_BASIC_OPTIONS` macro, this sets everything up needed for logging(and logging level), basic options and colored printing.
So afterward `CLI_PRINT` and `CLI_ERROR_PRINT` can be used.
When returning for the `exec` function the `RETURN (n)` macro should be used, it makes sure things allocated by `GET_BASIC_OPTIONS` are freed properly.
A basic version of this function could look something like this:

```c
int ret = 0;
	GET_BASIC_OPTIONS

	bool all = false;
	tmp = GET_OPTION_KEY (options, "all");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "all"), &all);
		keyDel (tmp);
	}

    ...

	Key * toLookUp = getKeyFromOptions (GET_OPTION (options, "name"), errorKey, verbose);
	if (toLookUp == NULL)
	{
		RETURN (2)
	}

    ...

	if (found == NULL)
	{
		CLI_ERROR_PRINT (CLI_LOG_NONE, "Did not find key '%s'", RED (keyName (toLookUp)));
		ret = 11;
		goto cleanup;
	}

	if (keyGetNamespace (found) == KEY_NS_DEFAULT)
	{
		CLI_PRINT (CLI_LOG_VERBOSE, "The key was not found in any other namespace, taking the %s\n", BOLD ("default"));
	}

	...

cleanup:
	...

	RETURN (ret)
```

For getting keys from arguments the helper function `getKeyFromOptions` can the used.
It will resolve bookmarks and make sure the argument is a valid keyname.

## 2. Output

All output in the `exec` function should be done using either `CLI_PRINT` or `CLI_ERROR_PRINT` with the appropriate logging level.
Available logging levels are `CLI_LOG_NONE`, `CLI_LOG_VERBOSE` and `CLI_LOG_DEBUG`.
By using `CLI_PRINT` or `CLI_ERROR_PRINT` logging levels are handled automatically.

## 3. Errors/Warning

If errors occur during the execution of your command, they should be set in `errorKey` and not printed directly.
This could look something like this:

```c
ELEKTRA_SET_INTERNAL_ERROR (errorKey, "could not copy key meta to errorKey");
```

These error setting macros are available in `kdberrors.h`.
By setting the like this, they are later printed properly formatted by the framework.

## 4. "registering" the new command

You just have to `#include` your header file in [`main.c`](/src/tools/kdb/main.c) and add an element to the `command subcommands[]` array.

```c
command subcommands[] = {
	...
	{ "your-new-command-name", add...Spec, exec... },
};
```

## _5. Validate_

After following these steps the new command will show up when typing `kdb --help` and `kdb <your-cmd-name> --help` will show a usage message based on the specification you have provided in the `add...Spec` function.
