/**
 * @file
 *
 * @brief Support library used by plugin gopts.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbopts.h>

#include <stdlib.h>
#include <string.h>

#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbmeta.h>

#include <kdbassert.h>
#include <kdberrors.h>

#ifdef _WIN32
static const char SEP_ENV_VALUE = ';';
#else
static const char SEP_ENV_VALUE = ':';
#endif

// Meta key storing which command an option/argument belongs to.
static const char * const META_COMMAND_KEY = "command/key";

struct OptionData
{
	Key * specKey;
	const char * metaKey;
	const char * hasArg;
	const char * kind;
	const char * flagValue;
	const char * argName;
	bool hidden;
};

struct Specification
{
	KeySet * options;
	KeySet * keys;
	KeySet * argIndices;
	KeySet * commands;
	bool useSubcommands;
};

/**
 * Get value of meta key with name @p meta of Key @p key as string.
 * @param key Key to retrieve meta value from.
 * @param meta Name of meta key.
 * @return NULL if the meta value is NULL or an empty string. Otherwise the meta value.
 */
static inline const char * keyGetMetaString (const Key * key, const char * meta)
{
	const Key * mk = keyGetMeta (key, meta);
	const char * value = mk == NULL ? NULL : keyString (mk);
	return value != NULL && value[0] == '\0' ? NULL : value;
}

/**
 * Get value of meta key identified by @p lookup of the Key @p key as string.
 * @param key Key to retrieve meta value from.
 * @param lookup A key pointer identifying the meta key to retrieve
 * @return NULL if the meta value is NULL or an empty string. Otherwise the meta value.
 */
static inline const char * keyGetMetaStringByKey (Key * key, Key * lookup)
{
	const Key * mk = ksLookup (keyMeta (key), lookup, KDB_O_DEL);
	const char * value = mk == NULL ? NULL : keyString (mk);
	return value != NULL && value[0] == '\0' ? NULL : value;
}

static int addProcKey (KeySet * ks, const Key * key, Key * valueKey);
static KeySet * parseEnvp (const char ** envp);

static KeySet * parseArgs (Key * command, KeySet * optionsSpec, bool useSubcommands, int argc, const char ** argv, int * endArg,
			   Key * errorKey);
static void setOption (Key * option, const char * value, bool repeated);

static Key * splitEnvValue (const Key * envKey);

static KeySet * ksMetaGetSingleOrArray (Key * key, const char * metaName);

char * generateUsageLine (const char * progname, Key * command, const Key * commandArgs);
static char * generateOptionsList (KeySet * keysWithOpts, Key * command);
static char * generateCommandsList (KeySet * keysWithOpts, Key * commandKey);
static char * generateArgsList (KeySet * keysWithOpts, Key * command);
static char * generateEnvsList (KeySet * keysWithOpts);
static bool optionOrArgBelongsToCommand (const Key * command, const Key * optionOrArg);

static bool processSpec (struct Specification * spec, KeySet * ks, Key * specParent, Key * errorKey);
static bool processOptions (struct Specification * spec, Key * command, Key * specKey, Key ** keyWithOpt, Key * errorKey);
static bool readOptionData (struct OptionData * optionData, Key * key, const char * metaKey, Key * errorKey);
static bool processShortOptSpec (struct Specification * spec, struct OptionData * optionData, Key * command, Key ** keyWithOpt,
				 char ** shortOptLine, Key * errorKey);
static bool processLongOptSpec (struct Specification * spec, struct OptionData * optionData, Key * command, Key ** keyWithOpt,
				char ** longOptLine, Key * errorKey);
static bool processEnvVars (KeySet * usedEnvVars, Key * specKey, Key ** keyWithOpt, Key * errorKey);
static bool processArgs (Key * command, Key * specKey, KeySet * argIndices, Key ** keyWithOpt, Key * errorKey);

static int writeOptionValues (KeySet * ks, Key * keyWithOpt, KeySet * options, Key * errorKey);
static int writeEnvVarValues (KeySet * ks, Key * keyWithOpt, KeySet * envValues, Key * errorKey);
static int writeArgsValues (KeySet * ks, Key * keyWithOpt, Key * command, KeySet * argIndices, KeySet * args, Key * errorKey);

static bool parseLongOption (Key * command, KeySet * optionsSpec, KeySet * options, int argc, const char ** argv, int * index,
			     Key * errorKey);
static bool parseShortOptions (Key * command, KeySet * optionsSpec, KeySet * options, int argc, const char ** argv, int * index,
			       Key * errorKey);

static int writeOptions (Key * command, Key * commandKey, Key * commandArgs, bool writeArgs, bool * argsWritten, KeySet * options,
			 struct Specification * spec, KeySet * ks, const char * progname, const char ** envp, Key * parentKey);

/**
 * This functions parses a specification of program options, together with a list of arguments
 * and environment variables to extract the option values.
 *
 * The options have to be defined in the metadata of keys in the spec namespace. If an option value
 * is found for any of the given keys, a new key with the same path but inside the proc namespace
 * will be inserted into @p ks. This enables a cascading lookup to find these values.
 *
 * Take look at https://www.libelektra.org/tutorials/command-line-options for information on how exactly
 * the specification works.
 *
 * NOTE: Per default option processing DOES NOT stop, when a non-option string is encountered in @p argv.
 * If you want processing to stop, set the metadata `posixly = 1` on @p parentKey.
 *
 *
 * @param ks	    The KeySet containing the specification for the options.
 * @param argc	    The number of strings in argv.
 * @param argv	    The arguments to be processed.
 * @param envp	    A list of environment variables. This needs to be a null-terminated list of
 * 		    strings of the format 'KEY=VALUE'.
 * @param parentKey The parent key below which the function will search for option specifications.
 *                  Also used for error reporting. The key will be translated into the spec namespace
 *                  automatically, e.g. 'user:/test/parent' will be translated into 'spec:/test/parent',
 *                  before checking against spec keys.
 *
 * @retval 0	on success, this is the only case in which @p ks will be modified
 * @retval -1	on error, the error will be set as metadata in @p errorKey
 * @retval 1	if the help option `--help` was found, use elektraGetOptsHelpMessage() access the
 * 		generated help message
 */
int elektraGetOpts (KeySet * ks, int argc, const char ** argv, const char ** envp, Key * parentKey)
{
	Key * specParent = keyDup (parentKey, KEY_CP_ALL);
	// Translate key to spec namespace
	keySetNamespace (specParent, KEY_NS_SPEC);

	struct Specification spec;
	if (!processSpec (&spec, ks, specParent, parentKey))
	{
		keyDel (specParent);
		return -1;
	}

	Key * command = keyNew ("/", KEY_END);
	Key * commandKey = keyNew (keyName (specParent), KEY_END);
	Key * commandArgs = keyNew ("/", KEY_END);

	keyDel (specParent);

	if (spec.useSubcommands)
	{
		int lastEndArg = 0;
		while (lastEndArg < argc)
		{
			int endArg = -1;
			KeySet * options =
				parseArgs (command, spec.options, true, argc - lastEndArg, argv + lastEndArg, &endArg, parentKey);

			if (options == NULL)
			{
				keyDel (command);
				keyDel (commandKey);
				keyDel (commandArgs);
				ksDel (spec.options);
				ksDel (spec.keys);
				ksDel (spec.argIndices);
				ksDel (spec.commands);
				return -1;
			}

			const Key * subCommand = NULL;
			if (endArg >= 0)
			{
				endArg += lastEndArg;

				Key * commandSpec = ksLookup (spec.keys, commandKey, 0);
				Key * commandLookup = keyNew ("meta:/command", KEY_END);
				keyAddBaseName (commandLookup, argv[endArg]);
				subCommand = ksLookup (keyMeta (commandSpec), commandLookup, KDB_O_DEL);
			}

			bool argsWritten = false;
			int result = writeOptions (command, commandKey, commandArgs, subCommand == NULL, &argsWritten, options, &spec, ks,
						   argv[0], envp, parentKey);
			ksDel (options);

			if (result != 0)
			{
				keyDel (command);
				keyDel (commandKey);
				keyDel (commandArgs);
				ksDel (spec.options);
				ksDel (spec.keys);
				ksDel (spec.argIndices);
				ksDel (spec.commands);
				return result;
			}

			if (subCommand == NULL && !argsWritten && endArg >= 0)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Unknown sub-command: %s", argv[endArg]);
				keyDel (command);
				keyDel (commandKey);
				keyDel (commandArgs);
				ksDel (spec.options);
				ksDel (spec.keys);
				ksDel (spec.argIndices);
				ksDel (spec.commands);
				return -1;
			}

			Key * procKey = keyNew ("proc:/", KEY_VALUE, "", KEY_END);
			keyAddName (procKey, strchr (keyName (commandKey), '/'));
			ksAppendKey (ks, procKey);

			if (subCommand == NULL)
			{
				keyDel (command);
				keyDel (commandKey);
				keyDel (commandArgs);
				ksDel (spec.options);
				ksDel (spec.keys);
				ksDel (spec.argIndices);
				ksDel (spec.commands);
				return 0;
			}

			keySetString (procKey, keyString (subCommand));

			keyAddBaseName (command, keyString (subCommand));
			keyAddBaseName (commandKey, keyString (subCommand));
			keyAddBaseName (commandArgs, argv[endArg]);

			lastEndArg = endArg;
		}

		ELEKTRA_ASSERT (0, "should be unreachable");
		return -2;
	}
	else
	{
		int endArg = 0;
		KeySet * options = parseArgs (command, spec.options, false, argc, argv, &endArg, parentKey);

		if (options == NULL)
		{
			keyDel (command);
			keyDel (commandKey);
			keyDel (commandArgs);
			ksDel (spec.options);
			ksDel (spec.keys);
			ksDel (spec.argIndices);
			ksDel (spec.commands);
			return -1;
		}

		int result = writeOptions (command, commandKey, commandArgs, true, NULL, options, &spec, ks, argv[0], envp, parentKey);
		keyDel (command);
		keyDel (commandKey);
		keyDel (commandArgs);
		ksDel (options);
		ksDel (spec.options);
		ksDel (spec.keys);
		ksDel (spec.argIndices);
		ksDel (spec.commands);
		return result;
	}
}

/**
 * Extracts the command whose help message was requested from the @p errorKey used in elektraGetOpts().
 * NOTE: this only works, if elektraGetOpts() returned 1.
 *
 * @param errorKey The same Key as passed to elektraGetOpts() as errorKey.
 * @param usage	   If this is not NULL, it will be used instead of the default usage line.
 * @param prefix   If this is not NULL, it will be inserted between the usage line and the options list.
 *
 * @return The command extracted from @p errorKey, or NULL if no command was found.
 * The returned string MUST NOT be freed with elektraFree(). It will be valid as long as @p errorKey is not keyDel()'ed.
 */
const char * elektraGetOptsHelpCommand (Key * errorKey)
{
	return keyGetMetaString (errorKey, "internal/libopts/help/command");
}

/**
 * Extracts the help message from the @p helpKey used in elektraGetOpts().
 *
 * @param helpKey  The same Key as passed to elektraGetOpts() as parentKey.
 * @param usage	   If this is not NULL, it will be used instead of the default usage line.
 * 		   Use elektraGetOptsHelpCommand() to check which command was invoked to get the right usage line.
 * @param prefix   If this is not NULL, it will be inserted between the usage line and the options list.
 *
 * @return The full help message extracted from @p helpKey, or NULL if no help message was found.
 * The returned string has to be freed with elektraFree().
 */
char * elektraGetOptsHelpMessage (Key * helpKey, const char * usage, const char * prefix)
{
	const char * command = elektraGetOptsHelpCommand (helpKey);

	Key * lookup;
	if (usage == NULL)
	{
		lookup = keyNew ("meta:/internal/libopts/help/usage", KEY_END);
		keyAddBaseName (lookup, command);
		usage = keyGetMetaStringByKey (helpKey, lookup);
	}

	if (usage == NULL)
	{
		return NULL;
	}

	lookup = keyNew ("meta:/internal/libopts/help/options", KEY_END);
	keyAddBaseName (lookup, command);
	const char * options = keyGetMetaStringByKey (helpKey, lookup);
	if (options == NULL)
	{
		options = "";
	}

	lookup = keyNew ("meta:/internal/libopts/help/commands", KEY_END);
	keyAddBaseName (lookup, command);
	const char * commands = keyGetMetaStringByKey (helpKey, lookup);
	if (commands == NULL)
	{
		commands = "";
	}

	lookup = keyNew ("meta:/internal/libopts/help/args", KEY_END);
	keyAddBaseName (lookup, command);
	const char * args = keyGetMetaStringByKey (helpKey, lookup);
	if (args == NULL)
	{
		args = "";
	}

	lookup = keyNew ("meta:/internal/libopts/help/envs", KEY_END);
	keyAddBaseName (lookup, command);
	const char * envs = keyGetMetaStringByKey (helpKey, lookup);
	if (envs == NULL)
	{
		envs = "";
	}

	return elektraFormat ("%s%s%s%s%s%s", usage, prefix == NULL ? "" : prefix, options, commands, args, envs);
}

// -------------
// static functions
// -------------

/**
 * Validate and process the specification set in the keys of @p ks, into @p spec.
 *
 * @param spec The target Specification struct.
 * @param ks The KeySet containing the specification.
 * @param specParent The parent key. All keys of the specification are below this keys.
 * @param errorKey Used to report errors.
 * @retval true on success.
 * @retval false on failure.
 */
bool processSpec (struct Specification * spec, KeySet * ks, Key * specParent, Key * errorKey)
{
	size_t specParentLen = strlen (keyName (specParent));

	// This block determines whether the spec uses sub-commands.
	bool useSubcommands = false;
	{
		Key * parent = ksLookupByName (ks, keyName (specParent), 0);
		if (parent != NULL)
		{
			const Key * commandMeta = keyGetMeta (parent, "command");
			const char * commandMetaString = keyString (commandMeta);
			if (commandMetaString != NULL && strlen (commandMetaString) == 0)
			{
				useSubcommands = true;
			}
			else if (commandMeta != NULL)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey, "On the parent key 'command' can only be set to an empty string. Offending key: %s",
					keyName (parent));
				return false;
			}
		}
	}

	KeySet * usedEnvVars = ksNew (0, KS_END);
	spec->options = ksNew (
		1, keyNew ("/long/help", KEY_META, "hasarg", "none", KEY_META, "kind", "single", KEY_META, "flagvalue", "1", KEY_END),
		KS_END);
	spec->keys = ksNew (0, KS_END);
	spec->argIndices = ksNew (0, KS_END);
	spec->commands = ksNew (0, KS_END);

	/**
	 * 1. Process all keys in the @p ks and
	 * 	a. Validate sub-commands (e.g., whether meta values are set correctly and the hierarchy of (sub-)commands is legal)
	 * 	b. Generate help text for each sub-command.
	 * 	c. Validate all options (long and short), arguments and environment variables, generate help texts for each and add them
	 * into the @spec.
	 */
	for (elektraCursor i = 0; i < ksGetSize (ks); ++i)
	{
		Key * cur = ksAtCursor (ks, i);

		// Keys that aren't in the spec namespace or below the parent key are ignored.
		if (keyGetNamespace (cur) != KEY_NS_SPEC || !keyIsBelowOrSame (specParent, cur))
		{
			continue;
		}

		bool isParentKey = strcmp (keyName (cur), keyName (specParent)) == 0;

		Key * keyWithOpt = NULL;

		// step 1a.) Validate sub-commands
		// If meta key "command" is set, the current key is a sub-command.
		const Key * commandMeta = keyGetMeta (cur, "command");
		if (commandMeta != NULL)
		{
			if (!useSubcommands)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey, "'command' can only be used, if it is set on the parent key as well. Offending key: %s",
					keyName (cur));
				ksDel (spec->options);
				ksDel (spec->argIndices);
				ksDel (spec->commands);
				ksDel (spec->keys);
				ksDel (usedEnvVars);
				return false;
			}

			const char * commandMetaString = keyString (commandMeta);
			if (commandMetaString == NULL || (strlen (commandMetaString) == 0 && !isParentKey))
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey,
					"'command' must be set to a non-empty string (except on the parent key). Offending key: %s",
					keyName (cur));
				ksDel (spec->options);
				ksDel (spec->argIndices);
				ksDel (spec->commands);
				ksDel (spec->keys);
				ksDel (usedEnvVars);
				return false;
			}

			if (keyWithOpt == NULL)
			{
				keyWithOpt = keyNew (keyName (cur), KEY_META, "command", "1", KEY_END);
			}

			// step 1b.)
			const char * optHelp = keyGetMetaString (cur, "opt/help");
			const char * description = keyGetMetaString (cur, "description");

			const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

			char * commandHelp = elektraFormat ("  %-28s%s", commandMetaString, help);
			keySetMeta (keyWithOpt, "command/help", commandHelp);
			elektraFree (commandHelp);

			if (!isParentKey)
			{
				Key * helpKey = keyNew (keyName (cur) + specParentLen, KEY_META, "hasarg", "none", KEY_META, "kind",
							"single", KEY_META, "flagvalue", "1", KEY_END);
				keyAddName (helpKey, "/long/help");
				ksAppendKey (spec->options, helpKey);
			}
		}

		Key * command = keyNew ("/", KEY_VALUE, keyName (specParent), KEY_END);
		if (useSubcommands && !isParentKey)
		{
			// Determine name of the parent of cur
			Key * curParent = keyNew (keyName (cur), KEY_END);
			if (strcmp (keyBaseName (curParent), "#") == 0)
			{
				keySetBaseName (curParent, NULL); // remove #
			}
			keySetBaseName (curParent, NULL);

			// Check if parent of current key exists in the KeySet
			Key * maybeCommand = ksLookup (ks, curParent, KDB_O_DEL);
			if (maybeCommand == NULL)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
									"The parent of this key (%s) must have the 'command' metakey set. "
									"Offending key: parent doesn't exist",
									keyName (cur));
				keyDel (keyWithOpt);
				keyDel (command);
				ksDel (spec->options);
				ksDel (spec->argIndices);
				ksDel (spec->commands);
				ksDel (spec->keys);
				ksDel (usedEnvVars);
				return false;
			}

			// Check if parent of current key has metakey "command" set
			const char * commandMetaString = keyGetMetaString (maybeCommand, "command");
			if (commandMetaString == NULL && strcmp (keyName (maybeCommand), keyName (specParent)) != 0)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey, "The parent of this key (%s) must have the 'command' metakey set. Offending key: %s",
					keyName (cur), keyName (maybeCommand));
				keyDel (keyWithOpt);
				keyDel (command);
				ksDel (spec->options);
				ksDel (spec->argIndices);
				ksDel (spec->commands);
				ksDel (spec->keys);
				ksDel (usedEnvVars);
				return false;
			}

			if (commandMeta != NULL)
			{
				// add sub-command to parent command
				Key * parentCommand = ksLookup (spec->keys, maybeCommand, 0);
				Key * subCommand = keyNew ("meta:/command", KEY_VALUE, keyBaseName (cur), KEY_END);
				keyAddBaseName (subCommand, keyString (commandMeta));
				if (ksLookup (keyMeta (parentCommand), subCommand, 0) != NULL)
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Duplicate sub-command '%s'. Offending key: %s",
										keyString (commandMeta), keyName (cur));
					keyDel (subCommand);
					keyDel (keyWithOpt);
					keyDel (command);
					ksDel (spec->options);
					ksDel (spec->argIndices);
					ksDel (spec->keys);
					ksDel (spec->commands);
					ksDel (usedEnvVars);
					return false;
				}

				ksAppendKey (keyMeta (parentCommand), subCommand);
			}

			keyAddName (command, keyName (maybeCommand) + specParentLen);
			keySetString (command, keyString (maybeCommand));

			if (commandMeta != NULL)
			{
				keySetMeta (command, "hassubcommands", "1");
			}
		}

		keyCopyAllMeta (command, ksLookup (spec->commands, command, KDB_O_CREATE));

		// step 1c.)
		if (!processOptions (spec, command, cur, &keyWithOpt, errorKey))
		{
			keyDel (command);
			keyDel (keyWithOpt);
			ksDel (spec->argIndices);
			ksDel (spec->options);
			ksDel (spec->keys);
			ksDel (spec->commands);
			ksDel (usedEnvVars);
			return false;
		}

		if (!processEnvVars (usedEnvVars, cur, &keyWithOpt, errorKey))
		{
			keyDel (command);
			keyDel (keyWithOpt);
			ksDel (spec->argIndices);
			ksDel (spec->options);
			ksDel (spec->keys);
			ksDel (spec->commands);
			ksDel (usedEnvVars);
			return false;
		}

		if (!processArgs (command, cur, spec->argIndices, &keyWithOpt, errorKey))
		{
			keyDel (command);
			keyDel (keyWithOpt);
			ksDel (spec->argIndices);
			ksDel (spec->options);
			ksDel (spec->keys);
			ksDel (spec->commands);
			ksDel (usedEnvVars);
			return false;
		}

		keyCopyAllMeta (ksLookup (spec->commands, command, KDB_O_CREATE), command);

		keyDel (command);

		if (keyWithOpt != NULL)
		{
			// Add the processed key to the KeySet
			ksAppendKey (spec->keys, keyWithOpt);
		}
	}
	ksDel (usedEnvVars);

	for (kdb_long_long_t i = 0; i < ksGetSize (spec->argIndices); i++)
	{
		Key * cur = ksAtCursor (spec->argIndices, i);
		const char * maxIndex = keyGetMetaString (cur, "index");

		char indexMetaName[ELEKTRA_MAX_ARRAY_SIZE + sizeof ("index/")] = "index/#0";
		kdb_long_long_t indexValue = 0;
		while (maxIndex != NULL && strcmp (indexMetaName + sizeof ("index"), maxIndex) < 0)
		{
			const char * meta = keyGetMetaString (cur, indexMetaName);
			if (meta == NULL)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey,
					"The values of 'args/index' must be continuous, but index " ELEKTRA_LONG_LONG_F
					" is missing in keys below: %s",
					indexValue, keyGetMetaString (cur, "key"));
				ksDel (spec->argIndices);
				ksDel (spec->options);
				ksDel (spec->keys);
				ksDel (spec->commands);
				return false;
			}

			++indexValue;
			elektraWriteArrayNumber (indexMetaName + sizeof ("index"), indexValue);
		}
	}

	spec->useSubcommands = useSubcommands;

	return true;
}

/**
 * Process the option specification for @p specKey.
 *
 * @retval true on success
 * @retval false on error
 */
bool processOptions (struct Specification * spec, Key * command, Key * specKey, Key ** keyWithOpt, Key * errorKey)
{
	const char * optHelp = keyGetMetaString (specKey, "opt/help");
	const char * description = keyGetMetaString (specKey, "description");

	const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

	KeySet * opts = ksMetaGetSingleOrArray (specKey, "opt");
	if (opts == NULL)
	{
		const char * longOpt = keyGetMetaString (specKey, "opt/long");
		if (longOpt == NULL)
		{
			return true;
		}

		// no other way to create Key with name "opt"
		Key * k = keyNew ("/", KEY_META, "opt", "", KEY_END);
		opts = ksNew (2, keyNew ("meta:/#", KEY_END), keyGetMeta (k, "opt"), KS_END);
		keyDel (k);
	}

	char * shortOptLine = elektraStrDup ("");
	char * longOptLine = elektraStrDup ("");

	for (elektraCursor it = 1; it < ksGetSize (opts); ++it) // skip count
	{
		Key * metaKey = ksAtCursor (opts, it);
		struct OptionData optionData;

		if (!readOptionData (&optionData, specKey, keyName (metaKey), errorKey))
		{
			ksDel (opts);
			elektraFree (shortOptLine);
			elektraFree (longOptLine);
			return false;
		}


		if (!processShortOptSpec (spec, &optionData, command, keyWithOpt, &shortOptLine, errorKey))
		{
			ksDel (opts);
			elektraFree (shortOptLine);
			elektraFree (longOptLine);
			return false;
		}

		if (!processLongOptSpec (spec, &optionData, command, keyWithOpt, &longOptLine, errorKey))
		{
			ksDel (opts);
			elektraFree (shortOptLine);
			elektraFree (longOptLine);
			return false;
		}
	}

	if (*keyWithOpt != NULL)
	{
		if (strlen (shortOptLine) > 2)
		{
			shortOptLine[strlen (shortOptLine) - 2] = '\0'; // trim ", " of end
		}

		if (strlen (longOptLine) > 2)
		{
			longOptLine[strlen (longOptLine) - 2] = '\0'; // trim ", " of end
		}

		char * optsLinePart = elektraFormat ("%s%s%s", shortOptLine, strlen (longOptLine) > 0 ? ", " : "", longOptLine);
		elektraFree (shortOptLine);
		elektraFree (longOptLine);

		size_t length = strlen (optsLinePart);
		if (length > 0)
		{
			char * optsLine;
			if (length < 30)
			{
				optsLine = elektraFormat ("  %-28s%s", optsLinePart, help);
			}
			else
			{
				optsLine = elektraFormat ("  %s\n  %30s%s", optsLinePart, "", help);
			}

			keySetMeta (*keyWithOpt, "opt/help", optsLine);
			elektraFree (optsLine);
		}
		elektraFree (optsLinePart);
	}
	else
	{
		elektraFree (shortOptLine);
		elektraFree (longOptLine);
	}

	ksDel (opts);

	return true;
}

/**
 * Read the option data (i.e. hasarg, flagvalue, etc.) for the option
 * given by @p metaKey 's name from @p key.
 * @retval true on success
 * @retval false on error
 */
bool readOptionData (struct OptionData * optionData, Key * key, const char * metaKey, Key * errorKey)
{
	// two slashes in string because array index is inserted in-between
	char metaBuffer[ELEKTRA_MAX_ARRAY_SIZE + sizeof ("meta:/opt//flagvalue") + 1];
	strncpy (metaBuffer, metaKey, ELEKTRA_MAX_ARRAY_SIZE + 3); // 3 = opt/ - null byte from ELEKTRA_MAX_SIZE
	strncat (metaBuffer, "/arg", 11);			   // 11 = remaining space in metaBuffer

	const char * hasArg = keyGetMetaString (key, metaBuffer);
	if (hasArg == NULL)
	{
		hasArg = "required";
	}

	strncpy (metaBuffer, metaKey, ELEKTRA_MAX_ARRAY_SIZE + 3); // 3 = opt/ - null byte from ELEKTRA_MAX_SIZE
	strncat (metaBuffer, "/arg/help", 11);			   // 11 = remaining space in metaBuffer

	const char * argNameMeta = keyGetMetaString (key, metaBuffer);

	strncpy (metaBuffer, metaKey, ELEKTRA_MAX_ARRAY_SIZE + 3); // 3 = opt/ - null byte from ELEKTRA_MAX_SIZE
	strncat (metaBuffer, "/flagvalue", 11);			   // 11 = remaining space in metaBuffer

	const char * flagValue = keyGetMetaString (key, metaBuffer);
	if (flagValue == NULL)
	{
		flagValue = "1";
	}
	else if (elektraStrCmp (hasArg, "none") != 0 && elektraStrCmp (hasArg, "optional") != 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
			errorKey,
			"The flagvalue metadata can only be used, if the opt/arg metadata is set to 'none' or "
			"'optional'. (key: %s)",
			keyName (key));
		return false;
	}

	strncpy (metaBuffer, metaKey, ELEKTRA_MAX_ARRAY_SIZE + 3); // 3 = opt/ - null byte from ELEKTRA_MAX_SIZE
	strncat (metaBuffer, "/hidden", 11);			   // 11 = remaining space in metaBuffer

	bool hidden = false;
	const char * hiddenStr = keyGetMetaString (key, metaBuffer);
	if (hiddenStr != NULL && elektraStrCmp (hiddenStr, "1") == 0)
	{
		hidden = true;
	}

	const char * kind = "single";
	if (elektraStrCmp (keyBaseName (key), "#") == 0)
	{
		kind = "array";
	}

	optionData->specKey = key;
	optionData->metaKey = metaKey;
	optionData->hasArg = hasArg;
	optionData->flagValue = flagValue;
	optionData->argName = argNameMeta;
	optionData->hidden = hidden;
	optionData->kind = kind;

	return true;
}

/**
 * Process a possible short option specification on @p keyWithOpt.
 * The specification will be added to @p optionsSpec. The option
 * string will be added to @p shortOptLine.
 *
 * @retval true on success
 * @retval false on error
 */
bool processShortOptSpec (struct Specification * spec, struct OptionData * optionData, Key * command, Key ** keyWithOpt,
			  char ** shortOptLine, Key * errorKey)
{
	Key * key = optionData->specKey;
	const char * hasArg = optionData->hasArg;
	const char * kind = optionData->kind;
	const char * flagValue = optionData->flagValue;
	const char * argName = optionData->argName;
	bool hidden = optionData->hidden;

	const char * shortOptStr = keyGetMetaString (optionData->specKey, optionData->metaKey);
	if (shortOptStr == NULL || shortOptStr[0] == '\0')
	{
		return true;
	}

	const char shortOpt = shortOptStr[0];

	if (shortOpt == '-')
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"Character '-' cannot be used as a short option. It would collide with the "
							"special string '--'. Offending key: %s",
							keyName (key));
		return false;
	}

	const char * commandName = command != NULL ? keyName (command) : "/";
	Key * shortOptSpec = keyNew (commandName, KEY_META, "key", keyName (key), KEY_META, "hasarg", hasArg, KEY_META, "kind", kind,
				     KEY_META, "flagvalue", flagValue, KEY_END);
	keyAddBaseName (shortOptSpec, "short");
	keyAddBaseName (shortOptSpec, (char[]){ shortOpt, '\0' });

	Key * existing = ksLookupByName (spec->options, keyName (shortOptSpec), 0);
	if (existing != NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"The option '-%c' has already been specified for the key '%s'. Additional key: %s",
							shortOpt, keyGetMetaString (existing, "key"), keyName (key));
		keyDel (shortOptSpec);
		return false;
	}

	ksAppendKey (spec->options, shortOptSpec);

	if (*keyWithOpt == NULL)
	{
		// Mark this option as belonging to command "command".
		*keyWithOpt = keyNew (keyName (key), KEY_META, META_COMMAND_KEY, keyName (command), KEY_END);
	}
	elektraMetaArrayAdd (*keyWithOpt, "opt", keyName (shortOptSpec));

	if (!hidden)
	{
		char * argString = NULL;
		if (elektraStrCmp (hasArg, "required") == 0)
		{
			argString = argName == NULL ? " ARG" : elektraFormat (" %s", argName);
		}

		char * newShortOptLine = elektraFormat ("%s-%c%s, ", *shortOptLine, shortOpt, argString == NULL ? "" : argString);
		elektraFree (*shortOptLine);
		if (argName != NULL)
		{
			elektraFree (argString);
		}
		*shortOptLine = newShortOptLine;
	}

	return true;
}

/**
 * Process a possible long option specification on @p keyWithOpt.
 * The specification will be added to @p optionsSpec. The option
 * string will be added to @p longOptLine.
 *
 * @retval true on success
 * @retval false on error
 */
bool processLongOptSpec (struct Specification * spec, struct OptionData * optionData, Key * command, Key ** keyWithOpt, char ** longOptLine,
			 Key * errorKey)
{
	Key * key = optionData->specKey;
	const char * hasArg = optionData->hasArg;
	const char * kind = optionData->kind;
	const char * flagValue = optionData->flagValue;
	const char * argName = optionData->argName;
	bool hidden = optionData->hidden;

	char * longMeta = elektraFormat ("%s/long", optionData->metaKey);
	const char * longOpt = keyGetMetaString (key, longMeta);
	elektraFree (longMeta);

	if (longOpt == NULL)
	{
		return true;
	}

	if (elektraStrCmp (longOpt, "help") == 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"Option 'help' cannot be used as a long option. It would collide with the "
							"help option '--help'. Offending key: %s",
							keyName (key));
		return false;
	}

	const char * commandName = command != NULL ? keyName (command) : "/";
	Key * longOptSpec = keyNew (commandName, KEY_META, "key", keyName (key), KEY_META, "hasarg", hasArg, KEY_META, "kind", kind,
				    KEY_META, "flagvalue", flagValue, KEY_END);
	keyAddBaseName (longOptSpec, "long");
	keyAddBaseName (longOptSpec, longOpt);

	Key * existing = ksLookupByName (spec->options, keyName (longOptSpec), 0);
	if (existing != NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"The option '--%s' has already been specified for the key '%s'. Additional key: %s",
							longOpt, keyGetMetaString (existing, "key"), keyName (key));
		keyDel (longOptSpec);
		return false;
	}

	ksAppendKey (spec->options, longOptSpec);

	if (*keyWithOpt == NULL)
	{
		// Mark this option as belonging to command "command".
		*keyWithOpt = keyNew (keyName (key), KEY_META, META_COMMAND_KEY, keyName (command), KEY_END);
	}
	elektraMetaArrayAdd (*keyWithOpt, "opt", keyName (longOptSpec));

	if (!hidden)
	{
		char * argString = NULL;
		if (elektraStrCmp (hasArg, "required") == 0)
		{
			argString = argName == NULL ? "=ARG" : elektraFormat ("=%s", argName);
		}
		else if (elektraStrCmp (hasArg, "optional") == 0)
		{
			argString = argName == NULL ? "=[ARG]" : elektraFormat ("=[%s]", argName);
		}


		char * newLongOptLine = elektraFormat ("%s--%s%s, ", *longOptLine, longOpt, argString == NULL ? "" : argString);
		elektraFree (*longOptLine);
		if (argName != NULL)
		{
			elektraFree (argString);
		}
		*longOptLine = newLongOptLine;
	}

	return true;
}

/**
 * Process possible environment variable specifications on @p keyWithOpt.
 * @retval true on success
 * @retval false on error
 */
bool processEnvVars (KeySet * usedEnvVars, Key * specKey, Key ** keyWithOpt, Key * errorKey ELEKTRA_UNUSED)
{
	const char * optHelp = keyGetMetaString (specKey, "opt/help");
	const char * description = keyGetMetaString (specKey, "description");

	const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

	KeySet * envVars = ksMetaGetSingleOrArray (specKey, "env");
	if (envVars == NULL)
	{
		return true;
	}

	char * envsLinePart = elektraStrDup ("");

	for (elektraCursor it = 1; it < ksGetSize (envVars); ++it) // skip count
	{
		Key * k = ksAtCursor (envVars, it);
		const char * envVar = keyString (k);
		if (envVar == NULL)
		{
			continue;
		}

		Key * envVarKey = keyNew ("/", KEY_META, "key", keyName (specKey), KEY_END);
		keyAddBaseName (envVarKey, envVar);

		Key * existing = ksLookup (usedEnvVars, envVarKey, 0);
		if (existing != NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "The environment variable '%s' has already been specified for the key '%s'. Additional key: %s",
				envVar, keyGetMetaString (existing, "key"), keyName (specKey));
			elektraFree (envsLinePart);
			keyDel (envVarKey);
			ksDel (envVars);
			return false;
		}

		ksAppendKey (usedEnvVars, envVarKey);

		if (*keyWithOpt == NULL)
		{
			*keyWithOpt = keyNew (keyName (specKey), KEY_END);
		}
		elektraMetaArrayAdd (*keyWithOpt, "env", keyName (envVarKey));

		char * tmp = elektraFormat ("%s%s, ", envsLinePart, envVar);
		elektraFree (envsLinePart);
		envsLinePart = tmp;
	}

	ksDel (envVars);

	if (strlen (envsLinePart) > 2)
	{
		envsLinePart[strlen (envsLinePart) - 2] = '\0'; // trim ", " of end
	}

	char * envsLine;
	if (strlen (envsLinePart) < 30)
	{
		envsLine = elektraFormat ("  %-28s%s", envsLinePart, help);
	}
	else
	{
		envsLine = elektraFormat ("  %s\n  %30s%s", envsLinePart, "", help);
	}

	keySetMeta (*keyWithOpt, "env/help", envsLine);
	elektraFree (envsLine);
	elektraFree (envsLinePart);
	return true;
}

bool processArgs (Key * command, Key * specKey, KeySet * argIndices, Key ** keyWithOpt, Key * errorKey)
{
	const char * optHelp = keyGetMetaString (specKey, "opt/help");
	const char * description = keyGetMetaString (specKey, "description");

	const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

	const char * argsMeta = keyGetMetaString (specKey, "args");
	if (argsMeta == NULL)
	{
		return true;
	}

	if (elektraStrCmp (argsMeta, "remaining") == 0)
	{
		if (elektraStrCmp (keyBaseName (specKey), "#") != 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args=remaining' can only be set on array keys (basename = '#'). Offending key: %s",
				keyName (specKey));
			return false;
		}

		if (*keyWithOpt == NULL)
		{
			// Mark this arg as belonging to command "command".
			*keyWithOpt = keyNew (keyName (specKey), KEY_META, META_COMMAND_KEY, keyName (command), KEY_END);
		}
		keySetMeta (*keyWithOpt, "args", "remaining");

		Key * dup = keyDup (specKey, KEY_CP_ALL);
		keySetBaseName (dup, NULL); // remove #

		const char * existing = keyGetMetaString (command, "remainingargskey");
		if (existing != NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'args=remaining' is already used on key '%s'. Offending key: %s",
								existing, keyName (specKey));
			keyDel (dup);
			return false;
		}

		keySetMeta (command, "remainingargs", keyBaseName (dup));
		keySetMeta (command, "remainingargskey", keyName (specKey));

		char * argName = elektraFormat ("%s...", keyBaseName (dup));
		char * argHelp = elektraFormat ("  %-28s%s", argName, help);
		keySetMeta (*keyWithOpt, "args/help", argHelp);
		elektraFree (argHelp);
		elektraFree (argName);
		keyDel (dup);
	}
	else if (elektraStrCmp (argsMeta, "indexed") == 0)
	{
		if (elektraStrCmp (keyBaseName (specKey), "#") == 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args=indexed' can only be set on non-array keys (basename != '#'). Offending key: %s",
				keyName (specKey));
			return false;
		}

		const Key * argsIndex = keyGetMeta (specKey, "args/index");
		if (argsIndex == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args=indexed' must be accompanied by 'args/index'. Offending key: %s", keyName (specKey));
			return false;
		}

		kdb_long_long_t indexValue;
		if (!elektraKeyToLongLong (argsIndex, &indexValue) || indexValue < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
								"'args/index' must be a non-negative integer not '%s'. Offending key: %s",
								keyString (argsIndex), keyName (specKey));
			return false;
		}

		if (*keyWithOpt == NULL)
		{
			// Mark this arg as belonging to command "command".
			*keyWithOpt = keyNew (keyName (specKey), KEY_META, META_COMMAND_KEY, keyName (command), KEY_END);
		}
		keySetMeta (*keyWithOpt, "args", "indexed");
		keySetMeta (*keyWithOpt, "args/index", keyString (argsIndex));

		Key * indexKey = ksLookup (argIndices, keyNew (keyName (command), KEY_END), KDB_O_DEL | KDB_O_CREATE);
		keySetMeta (indexKey, "key", keyString (command));

		char indexMetaName[ELEKTRA_MAX_ARRAY_SIZE + sizeof ("index/")];
		strcpy (indexMetaName, "index/");
		elektraWriteArrayNumber (indexMetaName + sizeof ("index"), indexValue);

		const char * maxIndex = keyGetMetaString (indexKey, "index");
		if (maxIndex == NULL || strcmp (maxIndex, indexMetaName + sizeof ("index")) < 0)
		{
			keySetMeta (indexKey, "index", indexMetaName + sizeof ("index"));
		}

		const char * existing = keyGetMetaString (indexKey, indexMetaName);
		if (existing != NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args/index=" ELEKTRA_LONG_LONG_F "' is already used by '%s'. Offending Key: %s", indexValue,
				existing, keyName (specKey));
			return false;
		}

		keySetMeta (indexKey, indexMetaName, keyName (specKey));

		char * argHelp = elektraFormat ("  %-28s%s", keyBaseName (specKey), help);
		keySetMeta (*keyWithOpt, "args/help", argHelp);
		elektraFree (argHelp);

		elektraMetaArrayAdd (command, "args", keyBaseName (specKey));
	}
	else
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "unknown value for 'args' metadata: '%s'. Offending key: %s", argsMeta,
							keyName (specKey));
		return false;
	}

	return true;
}

/**
 * Add keys to the proc namespace in @p ks for all options specified
 * on @p keyWithOpt. The env-vars are taken from @p envValues.
 * @retval -1 in case of error
 * @retval 0 if no key was added
 * @retval 1 if keys were added to @p ks
 */
int writeOptionValues (KeySet * ks, Key * keyWithOpt, KeySet * options, Key * errorKey)
{
	bool valueFound = false;

	KeySet * optMetas = elektraMetaArrayToKS (keyWithOpt, "opt");
	if (optMetas == NULL)
	{
		return 0;
	}

	bool shortFound = false;

	for (elektraCursor it = 1; it < ksGetSize (optMetas); ++it) // skip count
	{
		Key * optMeta = ksAtCursor (optMetas, it);
		Key * optLookup = keyNew (keyString (optMeta), KEY_END);
		Key * optKey = ksLookup (options, optLookup, KDB_O_DEL);
		bool isShort = strncmp (keyString (optMeta), "/short", 6) == 0;
		if (shortFound && !isShort)
		{
			// ignore long options, if a short one was found
			continue;
		}

		int res = addProcKey (ks, keyWithOpt, optKey);
		if (res == 0)
		{
			valueFound = true;
			if (isShort)
			{
				shortFound = true;
			}
		}
		else if (res < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "The option '%s%s' cannot be used, because another option has already been used for the key '%s'",
				isShort ? "-" : "--", isShort ? (const char[]){ keyBaseName (optKey)[0], '\0' } : keyBaseName (optKey),
				keyName (keyWithOpt));
			ksDel (optMetas);
			return -1;
		}
	}

	ksDel (optMetas);

	return valueFound ? 1 : 0;
}

/**
 * Add keys to the proc namespace in @p ks for everything that is specified
 * by the 'env' metadata on @p keyWithOpt. The env-vars are taken from @p envValues.
 * @retval -1 in case of error
 * @retval 0 if no key was added
 * @retval 1 if keys were added to @p ks
 */
int writeEnvVarValues (KeySet * ks, Key * keyWithOpt, KeySet * envValues, Key * errorKey)
{
	bool valueFound = false;

	KeySet * envMetas = elektraMetaArrayToKS (keyWithOpt, "env");
	if (envMetas == NULL)
	{
		return 0;
	}

	for (elektraCursor it = 1; it < ksGetSize (envMetas); ++it) // skip count
	{
		Key * envMeta = ksAtCursor (envMetas, it);
		Key * envKey = ksLookupByName (envValues, keyString (envMeta), 0);

		bool isArray = strcmp (keyBaseName (keyWithOpt), "#") == 0;
		Key * envValueKey;
		if (envKey == NULL)
		{
			envValueKey = NULL;
		}
		else if (isArray)
		{
			envValueKey = splitEnvValue (envKey);
		}
		else
		{
			envValueKey = keyNew (keyName (envKey), KEY_VALUE, keyString (envKey), KEY_END);
		}

		int res = addProcKey (ks, keyWithOpt, envValueKey);
		if (res < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey,
				"The environment variable '%s' cannot be used, because another variable has "
				"already been used for the key '%s'.",
				keyBaseName (envKey), keyName (keyWithOpt));
			keyDel (envValueKey);
			ksDel (envMetas);
			return -1;
		}
		else if (res == 0)
		{
			valueFound = true;
		}
		keyDel (envValueKey);
	}
	ksDel (envMetas);

	return valueFound ? 1 : 0;
}

/**
 * Add keys to the proc namespace in @p ks for everything that is specified
 * by the 'args' metadata on @p keyWithOpt. The args are taken from @p args.
 * @retval -1 in case of error
 * @retval 0 if no key was added
 * @retval 1 if keys were added to @p ks
 */
int writeArgsValues (KeySet * ks, Key * keyWithOpt, Key * command, KeySet * argIndices, KeySet * args, Key * errorKey)
{
	const char * argsMeta = keyGetMetaString (keyWithOpt, "args");
	if (argsMeta == NULL)
	{
		return 0;
	}

	if (strcmp (argsMeta, "remaining") == 0)
	{
		Key * argIndex = ksLookup (argIndices, command, 0);
		KeySet * indices = elektraMetaArrayToKS (argIndex, "index");
		elektraCursor firstRemainingArg = argIndex == NULL ? 0 : ksGetSize (indices) - 1; // -1 because of parent
		ksDel (indices);

		Key * procKey = keyNew ("proc:/", KEY_END);
		keyAddName (procKey, strchr (keyName (keyWithOpt), '/'));

		Key * insertKey = keyDup (procKey, KEY_CP_NAME);

		for (elektraCursor i = firstRemainingArg; i < ksGetSize (args); ++i)
		{
			Key * arg = ksAtCursor (args, i);
			elektraArrayIncName (insertKey);

			Key * k = keyDup (insertKey, KEY_CP_NAME);
			keySetString (k, keyString (arg));
			ksAppendKey (ks, k);
		}

		keySetBaseName (procKey, NULL); // remove #
		if (strcmp (keyBaseName (insertKey), "#") != 0)
		{
			keySetMeta (procKey, "array", keyBaseName (insertKey));
		}
		ksAppendKey (ks, procKey);
		keyDel (insertKey);
		return 1;
	}
	else if (strcmp (argsMeta, "indexed") == 0)
	{
		kdb_long_long_t index;
		elektraKeyToLongLong (keyGetMeta (keyWithOpt, "args/index"), &index);

		char arrayIndex[ELEKTRA_MAX_ARRAY_SIZE];
		elektraWriteArrayNumber (arrayIndex, index);

		Key * argKey = keyNew (keyName (command), KEY_END);
		keyAddBaseName (argKey, "args");
		keyAddBaseName (argKey, arrayIndex);
		Key * arg = ksLookup (args, argKey, KDB_O_DEL);
		if (arg == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "Expected at least " ELEKTRA_LONG_LONG_F " non-option arguments, but only got %zd", index + 1,
				ksGetSize (args));
			return -1;
		}

		Key * procKey = keyNew ("proc:/", KEY_END);
		keyAddName (procKey, strchr (keyName (keyWithOpt), '/'));
		keySetString (procKey, keyString (arg));
		ksAppendKey (ks, procKey);
		return 1;
	}
	else
	{
		return -1;
	}
}

/**
 * Taken a key whose value is an environment variable like array (like $PATH)
 * and turn it into a Key with either a string value or a metadata array 'values'
 * containing the value of the variable.
 */
Key * splitEnvValue (const Key * envKey)
{
	Key * valueKey = keyNew (keyName (envKey), KEY_END);

	char * envValue = elektraStrDup (keyString (envKey));
	char * curEnvValue = envValue;

	char * c = strchr (curEnvValue, SEP_ENV_VALUE);
	if (c == NULL)
	{
		elektraMetaArrayAdd (valueKey, "values", curEnvValue);
	}
	else
	{
		char * lastEnvValue = curEnvValue;
		while (c != NULL)
		{
			*c = '\0';

			elektraMetaArrayAdd (valueKey, "values", curEnvValue);

			curEnvValue = c + 1;
			lastEnvValue = curEnvValue;
			c = strchr (curEnvValue, SEP_ENV_VALUE);
		}
		elektraMetaArrayAdd (valueKey, "values", lastEnvValue);
	}

	elektraFree (envValue);

	return valueKey;
}

/**
 * @retval 0 if a proc key was added to ks
 * @retval -1 on pre-existing value, except if key is an array key, or replace == true then 0
 * @retval 1 on NULL pointers and failed insertion
 */
int addProcKey (KeySet * ks, const Key * key, Key * valueKey)
{
	if (ks == NULL || key == NULL || valueKey == NULL)
	{
		return 1;
	}

	Key * procKey = keyNew ("proc:/", KEY_END);
	keyAddName (procKey, strchr (keyName (key), '/'));

	bool isArrayKey = elektraStrCmp (keyBaseName (procKey), "#") == 0;
	if (isArrayKey)
	{
		keySetBaseName (procKey, NULL); // remove # (for lookup)
	}


	Key * existing = ksLookupByName (ks, keyName (procKey), 0);
	if (existing != NULL)
	{
		const char * value = isArrayKey ? keyGetMetaString (existing, "array") : keyString (existing);
		if (value != NULL && strlen (value) > 0)
		{
			keyDel (procKey);
			return -1;
		}
	}

	if (isArrayKey)
	{
		Key * insertKey = keyDup (procKey, KEY_CP_NAME);
		keyAddBaseName (insertKey, "#");
		KeySet * values = elektraMetaArrayToKS (valueKey, "values");
		if (values == NULL)
		{
			keyDel (procKey);
			keyDel (insertKey);
			return 1;
		}

		for (elektraCursor it = 1; it < ksGetSize (values); ++it) // skip count
		{
			Key * cur = ksAtCursor (values, it);
			elektraArrayIncName (insertKey);

			Key * k = keyDup (insertKey, KEY_CP_NAME);
			keySetString (k, keyString (cur));
			ksAppendKey (ks, k);
		}

		keySetMeta (procKey, "array", keyBaseName (insertKey));
		keyDel (insertKey);
		ksDel (values);
	}
	else
	{
		keySetString (procKey, keyString (valueKey));
	}

	return ksAppendKey (ks, procKey) > 0 ? 0 : 1;
}

/**
 * Parses env-vars from envp into an internal format.
 */
KeySet * parseEnvp (const char ** envp)
{
	KeySet * ks = ksNew (0, KS_END);

	const char ** cur = envp;
	while (*cur != NULL)
	{
		const char * eq = strchr (*cur, '=');
		Key * key = keyNew ("/", KEY_VALUE, eq + 1, KEY_END);
		size_t len = eq - *cur;
		char * name = elektraMemDup (*cur, len + 1);
		name[len] = '\0';
		keyAddBaseName (key, name);
		ksAppendKey (ks, key);
		elektraFree (name);

		cur++;
	}

	return ks;
}

/**
 * Parses command-line arguments (options and parameters) from argc/argv into an internal format.
 */
KeySet * parseArgs (Key * command, KeySet * optionsSpec, bool useSubcommands, int argc, const char ** argv, int * endArg, Key * errorKey)
{
	const char * posixlyStr = keyGetMetaString (errorKey, "posixly");
	bool posixly = false;
	if (posixlyStr != NULL && elektraStrCmp (posixlyStr, "1") == 0)
	{
		posixly = true;
	}

	if (useSubcommands)
	{
		posixly = true;
	}

	Key * argKey = keyNew (keyName (command), KEY_END);
	keyAddName (argKey, "/args/#");

	KeySet * options = ksNew (0, KS_END);
	int i;
	for (i = 1; i < argc; ++i)
	{
		const char * cur = argv[i];
		if (cur[0] == '-')
		{
			// possible option
			if (cur[1] == '-')
			{
				if (cur[2] == '\0')
				{
					// end of options
					++i; // skip --
					break;
				}

				if (!parseLongOption (command, optionsSpec, options, argc, argv, &i, errorKey))
				{
					keyDel (argKey);
					ksDel (options);
					return NULL;
				}

				continue;
			}

			if (!parseShortOptions (command, optionsSpec, options, argc, argv, &i, errorKey))
			{
				keyDel (argKey);
				ksDel (options);
				return NULL;
			}
		}
		else
		{
			// not an option
			if (posixly)
			{
				*endArg = i;
				break;
			}

			elektraArrayIncName (argKey);
			Key * newArgKey = keyDup (argKey, KEY_CP_NAME);
			keySetString (newArgKey, cur);
			ksAppendKey (options, newArgKey);
		}
	}

	// collect rest of argv
	for (; i < argc; ++i)
	{
		elektraArrayIncName (argKey);
		Key * newArgKey = keyDup (argKey, KEY_CP_NAME);
		keySetString (newArgKey, argv[i]);
		ksAppendKey (options, newArgKey);
	}

	Key * argsParent = keyNew (keyName (command), KEY_VALUE, keyBaseName (argKey), KEY_END);
	keyAddBaseName (argsParent, "args");
	ksAppendKey (options, argsParent);
	keyDel (argKey);

	return options;
}

bool parseShortOptions (Key * command, KeySet * optionsSpec, KeySet * options, int argc, const char ** argv, int * index, Key * errorKey)
{
	int i = *index;
	for (const char * c = &argv[i][1]; *c != '\0'; ++c)
	{

		Key * shortOpt = keyNew (keyName (command), KEY_END);
		keyAddBaseName (shortOpt, "short");
		keyAddBaseName (shortOpt, (char[]){ *c, '\0' });

		Key * optSpec = ksLookupByName (optionsSpec, keyName (shortOpt), 0);

		if (optSpec == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Unknown short option: -%c", keyBaseName (shortOpt)[0]);
			keyDel (shortOpt);
			return false;
		}

		const char * hasArg = keyGetMetaString (optSpec, "hasarg");
		const char * kind = keyGetMetaString (optSpec, "kind");
		const char * flagValue = keyGetMetaString (optSpec, "flagvalue");

		bool repeated = elektraStrCmp (kind, "array") == 0;

		Key * option = ksLookupByName (options, keyName (shortOpt), 0);
		if (option == NULL)
		{
			option = keyNew (keyName (shortOpt), KEY_META, "key", keyGetMetaString (optSpec, "key"), KEY_END);
			ksAppendKey (options, option);
		}
		else if (!repeated)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "This option cannot be repeated: -%c", keyBaseName (shortOpt)[0]);
			keyDel (shortOpt);
			return false;
		}

		bool last = false;
		if (elektraStrCmp (hasArg, "required") == 0)
		{
			if (*(c + 1) == '\0')
			{
				if (i >= argc - 1)
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Missing argument for short option: -%c",
										keyBaseName (shortOpt)[0]);
					keyDel (shortOpt);
					return false;
				}
				// use next as arg and skip
				setOption (option, argv[++i], repeated);
				*index = i;
			}
			else
			{
				// use rest as argument
				setOption (option, c + 1, repeated);
				last = true;
			}
		}
		else
		{
			// use flag value
			setOption (option, flagValue, repeated);
		}
		keyDel (shortOpt);

		keySetMeta (option, "short", "1");
		ksAppendKey (options, option);

		if (last)
		{
			break;
		}
	}

	return true;
}

bool parseLongOption (Key * command, KeySet * optionsSpec, KeySet * options, int argc, const char ** argv, int * index, Key * errorKey)
{
	int i = *index;
	Key * longOpt = keyNew (keyName (command), KEY_END);
	keyAddBaseName (longOpt, "long");

	char * opt = elektraStrDup (&argv[i][2]);
	char * eq = strchr (opt, '=');
	size_t argStart = 0;
	if (eq != NULL)
	{
		// mark end of option
		*eq = '\0';
		argStart = eq - opt + 3;
	}

	keyAddBaseName (longOpt, opt);
	elektraFree (opt);

	// lookup spec
	Key * optSpec = ksLookupByName (optionsSpec, keyName (longOpt), 0);

	if (optSpec == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Unknown long option: --%s", keyBaseName (longOpt));
		keyDel (longOpt);
		return false;
	}

	const char * hasArg = keyGetMetaString (optSpec, "hasarg");
	const char * kind = keyGetMetaString (optSpec, "kind");
	const char * flagValue = keyGetMetaString (optSpec, "flagvalue");

	bool repeated = elektraStrCmp (kind, "array") == 0;

	Key * option = ksLookupByName (options, keyName (longOpt), 0);
	if (option == NULL)
	{
		option = keyNew (keyName (longOpt), KEY_META, "key", keyGetMetaString (optSpec, "key"), KEY_END);
		ksAppendKey (options, option);
	}
	else if (!repeated)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "This option cannot be repeated: --%s", keyBaseName (longOpt));
		keyDel (longOpt);
		return false;
	}
	else if (keyGetMetaString (option, "short") != NULL)
	{
		keyDel (longOpt);
		// short option found already ignore long version
		return true;
	}

	if (elektraStrCmp (hasArg, "required") == 0)
	{
		// extract argument
		if (argStart > 0)
		{
			// use '=' arg
			setOption (option, &argv[i][argStart], repeated);
		}
		else
		{
			if (i >= argc - 1)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Missing argument for long option: --%s",
									keyBaseName (longOpt));
				keyDel (longOpt);
				return false;
			}
			// use next as arg and skip
			setOption (option, argv[++i], repeated);
			*index = i;
		}
	}
	else if (elektraStrCmp (hasArg, "optional") == 0)
	{
		if (argStart > 0)
		{
			// only use '=' argument
			setOption (option, &argv[i][argStart], repeated);
		}
		else if (flagValue != NULL)
		{
			// use flag value
			setOption (option, flagValue, repeated);
		}
	}
	else
	{
		if (argStart > 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "This option cannot have an argument: --%s",
								keyBaseName (longOpt));
			keyDel (longOpt);
			return false;
		}

		// use flag value
		setOption (option, flagValue, repeated);
	}
	keyDel (longOpt);

	return true;
}

void setOption (Key * option, const char * value, bool repeated)
{
	if (repeated)
	{
		elektraMetaArrayAdd (option, "values", value);
	}
	else
	{
		keySetString (option, value);
	}
}

/**
 * Writes the options from parseArgs into keys in the proc namespace
 */
int writeOptions (Key * command, Key * commandKey, Key * commandArgs, bool writeArgs, bool * argsWritten, KeySet * options,
		  struct Specification * spec, KeySet * ks, const char * progname, const char ** envp, Key * parentKey)
{
	// Check if help message should be generated
	Key * helpKey = keyNew (keyName (command), KEY_END);
	keyAddName (helpKey, "/long/help");

	// Generate help message
	if (ksLookup (options, helpKey, KDB_O_DEL) != NULL)
	{
		char * lastSlash = strrchr (progname, '/');
		if (lastSlash != NULL)
		{
			progname = lastSlash + 1;
		}

		char * usage = generateUsageLine (progname, ksLookup (spec->commands, command, 0), commandArgs);
		char * optionsText = generateOptionsList (spec->keys, command);
		char * commandsText = generateCommandsList (spec->keys, commandKey);
		char * argsText = generateArgsList (spec->keys, command);
		char * envsText = generateEnvsList (spec->keys);

		keySetMeta (parentKey, "internal/libopts/help/usage", usage);
		keySetMeta (parentKey, "internal/libopts/help/options", optionsText);
		keySetMeta (parentKey, "internal/libopts/help/commands", commandsText);
		keySetMeta (parentKey, "internal/libopts/help/args", argsText);
		keySetMeta (parentKey, "internal/libopts/help/envs", envsText);

		elektraFree (usage);
		elektraFree (optionsText);
		elektraFree (commandsText);
		elektraFree (argsText);
		elektraFree (envsText);
		return 1;
	}
	else // Don't generate help message
	{
		KeySet * envValues = parseEnvp (envp);

		Key * argsParent = keyNew (keyName (command), KEY_END);
		keyAddBaseName (argsParent, "args");
		KeySet * args = elektraArrayGet (argsParent, options);
		keyDel (argsParent);

		for (elektraCursor it = 0; it < ksGetSize (spec->keys); ++it)
		{
			Key * keyWithOpt = ksAtCursor (spec->keys, it);
			if (spec->useSubcommands)
			{
				Key * checkKey = keyDup (keyWithOpt, KEY_CP_ALL);
				if (strcmp (keyBaseName (keyWithOpt), "#") == 0)
				{
					keySetBaseName (checkKey, NULL); // remove #
				}

				int result = keyIsDirectlyBelow (commandKey, checkKey);
				keyDel (checkKey);

				if (result != 1)
				{
					continue;
				}
			}

			if (keyGetMeta (keyWithOpt, "command") != NULL)
			{
				Key * procKey = keyNew ("proc:/", KEY_VALUE, "", KEY_END);
				keyAddName (procKey, strchr (keyName (keyWithOpt), '/'));
				ksAppendKey (ks, procKey);
			}

			int result = writeOptionValues (ks, keyWithOpt, options, parentKey);
			if (result < 0)
			{
				ksDel (envValues);
				ksDel (args);
				return -1;
			}
			else if (result > 0)
			{
				continue;
			}

			if (writeArgs)
			{
				result = writeArgsValues (ks, keyWithOpt, command, spec->argIndices, args, parentKey);
				if (result < 0)
				{
					ksDel (envValues);
					ksDel (args);
					return -1;
				}
				else if (result > 0)
				{
					if (argsWritten != NULL)
					{
						*argsWritten = true;
					}
					continue;
				}
			}

			result = writeEnvVarValues (ks, keyWithOpt, envValues, parentKey);
			if (result < 0)
			{
				ksDel (envValues);
				ksDel (args);
				return -1;
			}
		}

		ksDel (envValues);
		ksDel (args);

		return 0;
	}
}

KeySet * ksMetaGetSingleOrArray (Key * key, const char * metaName)
{
	const Key * k = keyGetMeta (key, metaName);
	if (k == NULL)
	{
		return NULL;
	}

	const char * value = keyString (k);
	if (value[0] != '#')
	{
		// add dummy key to mimic elektraMetaArrayToKS
		return ksNew (2, keyNew ("meta:/#", KEY_END), k, KS_END);
	}

	Key * testKey = keyDup (k, KEY_CP_NAME);
	keyAddBaseName (testKey, keyString (k));

	const Key * test = keyGetMeta (key, keyName (testKey));
	keyDel (testKey);

	if (test == NULL)
	{
		// add dummy key to mimic elektraMetaArrayToKS
		return ksNew (2, keyNew ("meta:/#", KEY_END), k, KS_END);
	}

	return elektraMetaArrayToKS (key, metaName);
}

/**
 * Generate usage line for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateUsageLine (const char * progname, Key * command, const Key * commandArgs)
{
	size_t commandStringSize = keyGetUnescapedNameSize (commandArgs) - 2;
	char * commandString = elektraMalloc (commandStringSize);
	memcpy (commandString, ((const char *) keyUnescapedName (commandArgs)) + 2, commandStringSize);

	for (char * p = commandString; p < commandString + commandStringSize; ++p)
	{
		*p = *p == '\0' ? ' ' : *p;
	}
	commandString[commandStringSize - 1] = '\0';

	KeySet * args = elektraMetaArrayToKS (command, "args");

	char * indexedArgs;

	if (ksGetSize (args) <= 0)
	{
		indexedArgs = elektraStrDup ("");
	}
	else
	{
		size_t argsTotalSize = 0;
		for (elektraCursor i = 1; i < ksGetSize (args); ++i) // start at one to skip size
		{
			argsTotalSize += strlen (keyString (ksAtCursor (args, i))) + 3; // + 3 for space and []
		}

		indexedArgs = elektraMalloc (argsTotalSize + 1);
		char * pos = indexedArgs;
		size_t size = argsTotalSize + 1;
		for (elektraCursor i = 1; i < ksGetSize (args); i++) // start at one to skip size
		{
			*pos++ = '<';
			pos = memccpy (pos, keyString (ksAtCursor (args, i)), '\0', size);
			*(pos - 1) = '>';
			*pos++ = ' ';
		}
		*(pos - 1) = '\0';
	}

	bool hasSubCommands = keyGetMeta (command, "hassubcommands") != NULL;
	const char * remainingArgs = keyGetMetaString (command, "remainingargs");

	char * usage;
	if (hasSubCommands)
	{
		if (ksGetSize (args) > 0)
		{
			if (remainingArgs != NULL)
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] [COMMAND [...]|%s [<%s>...]]\n", progname,
						       commandStringSize > 1 ? " " : "", commandString, indexedArgs, remainingArgs);
			}
			else
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] [COMMAND [...]|%s]\n", progname,
						       commandStringSize > 1 ? " " : "", commandString, indexedArgs);
			}
		}
		else
		{
			if (remainingArgs != NULL)
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] [COMMAND [...]|[<%s>...]]\n", progname,
						       commandStringSize > 1 ? " " : "", commandString, remainingArgs);
			}
			else
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] [COMMAND [...]]\n", progname,
						       commandStringSize > 1 ? " " : "", commandString);
			}
		}
	}
	else
	{
		if (ksGetSize (args) > 0)
		{
			if (remainingArgs != NULL)
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] %s [<%s>...]\n", progname,
						       commandStringSize > 1 ? " " : "", commandString, indexedArgs, remainingArgs);
			}
			else
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] %s\n", progname, commandStringSize > 1 ? " " : "",
						       commandString, indexedArgs);
			}
		}
		else
		{
			if (remainingArgs != NULL)
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...] [<%s>...]\n", progname, commandStringSize > 1 ? " " : "",
						       commandString, remainingArgs);
			}
			else
			{
				usage = elektraFormat ("Usage: %s%s%s [OPTION...]\n", progname, commandStringSize > 1 ? " " : "",
						       commandString);
			}
		}
	}

	ksDel (args);
	elektraFree (indexedArgs);
	elektraFree (commandString);
	return usage;
}


/**
 * Generate options list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateOptionsList (KeySet * keysWithOpts, Key * command)
{
	char * optionsList = elektraStrDup ("");

	for (elektraCursor it = 0; it < ksGetSize (keysWithOpts); ++it)
	{
		Key * cur = ksAtCursor (keysWithOpts, it);
		if (!optionOrArgBelongsToCommand (command, cur))
		{
			continue;
		}

		const char * optLine = keyGetMetaString (cur, "opt/help");
		if (optLine != NULL)
		{
			char * newOptionsList = elektraFormat ("%s\n%s", optionsList, optLine);
			elektraFree (optionsList);
			optionsList = newOptionsList;
		}
	}

	char * newOptionsList = elektraFormat (
		"\nOPTIONS\n"
		"  --help                      Print this help message"
		"%s\n",
		optionsList);
	elektraFree (optionsList);
	optionsList = newOptionsList;
	return optionsList;
}

/**
 * Generate commands list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateCommandsList (KeySet * keysWithOpts, Key * commandKey)
{
	char * commandsList = elektraStrDup ("");

	for (elektraCursor it = 0; it < ksGetSize (keysWithOpts); ++it)
	{
		Key * cur = ksAtCursor (keysWithOpts, it);
		Key * checkKey = keyDup (cur, KEY_CP_NAME);
		if (strcmp (keyBaseName (cur), "#") == 0)
		{
			keySetBaseName (checkKey, NULL); // remove #
		}

		int result = keyIsDirectlyBelow (commandKey, checkKey);
		keyDel (checkKey);

		if (result != 1)
		{
			continue;
		}

		const char * cmdLine = keyGetMetaString (cur, "command/help");
		if (cmdLine != NULL)
		{
			char * newCommandsList = elektraFormat ("%s\n%s", commandsList, cmdLine);
			elektraFree (commandsList);
			commandsList = newCommandsList;
		}
	}

	if (strlen (commandsList) == 0)
	{
		return commandsList;
	}

	char * newCommandsList = elektraFormat ("\nCOMMANDS%s\n", commandsList);
	elektraFree (commandsList);
	commandsList = newCommandsList;

	return commandsList;
}

/**
 * Generate args (parameters) list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateArgsList (KeySet * keysWithOpts, Key * command)
{
	char * argsList = elektraStrDup ("");

	for (elektraCursor it = 0; it < ksGetSize (keysWithOpts); ++it)
	{
		Key * cur = ksAtCursor (keysWithOpts, it);
		if (!optionOrArgBelongsToCommand (command, cur))
		{
			continue;
		}

		const char * argLine = keyGetMetaString (cur, "args/help");
		if (argLine != NULL)
		{
			char * newArgsList = elektraFormat ("%s\n%s", argsList, argLine);
			elektraFree (argsList);
			argsList = newArgsList;
		}
	}

	if (strlen (argsList) == 0)
	{
		return argsList;
	}

	char * newArgsList = elektraFormat ("\nPARAMETERS%s\n", argsList);
	elektraFree (argsList);
	argsList = newArgsList;

	return argsList;
}

/**
 * Generate env-var list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateEnvsList (KeySet * keysWithOpts)
{
	char * envsList = elektraStrDup ("");

	for (elektraCursor it = 0; it < ksGetSize (keysWithOpts); ++it)
	{
		Key * cur = ksAtCursor (keysWithOpts, it);
		const char * optLine = keyGetMetaString (cur, "env/help");
		if (optLine != NULL)
		{
			char * newEnvsList = elektraFormat ("%s\n%s", envsList, optLine);
			elektraFree (envsList);
			envsList = newEnvsList;
		}
	}

	if (strlen (envsList) == 0)
	{
		return envsList;
	}

	char * newEnvsList = elektraFormat ("\nENVIRONMENT VARIABLES%s\n", envsList);
	elektraFree (envsList);
	envsList = newEnvsList;

	return envsList;
}

/**
 * Determines whether the option or argument belongs to the given command.
 * @param command The Key of the command.
 * @param optionOrArg The key of the option/arg.
 * @return True, if it belongs, false otherwise.
 */
bool optionOrArgBelongsToCommand (const Key * command, const Key * optionOrArg)
{
	const Key * commandKey = keyGetMeta (optionOrArg, META_COMMAND_KEY);
	const char * commandKeyString = commandKey != NULL ? keyString (commandKey) : NULL;
	return commandKeyString != NULL && strcmp (keyName (command), commandKeyString) == 0;
}
