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
static char * const META_COMMAND_KEY = "command/key";

struct OptionData
{
	ElektraKey * specKey;
	const char * metaKey;
	const char * hasArg;
	const char * kind;
	const char * flagValue;
	const char * argName;
	bool hidden;
};

struct Specification
{
	ElektraKeyset * options;
	ElektraKeyset * keys;
	ElektraKeyset * argIndices;
	ElektraKeyset * commands;
	bool useSubcommands;
};

/**
 * Get value of meta key with name @p meta of Key @p key as string.
 * @param key Key to retrieve meta value from.
 * @param meta Name of meta key.
 * @return NULL if the meta value is NULL or an empty string. Otherwise the meta value.
 */
static inline const char * keyGetMetaString (const ElektraKey * key, const char * meta)
{
	const ElektraKey * mk = elektraKeyGetMeta (key, meta);
	const char * value = mk == NULL ? NULL : elektraKeyString (mk);
	return value != NULL && value[0] == '\0' ? NULL : value;
}

/**
 * Get value of meta key identified by @p lookup of the Key @p key as string.
 * @param key Key to retrieve meta value from.
 * @param lookup A key pointer identifying the meta key to retrieve
 * @return NULL if the meta value is NULL or an empty string. Otherwise the meta value.
 */
static inline const char * keyGetMetaStringByKey (ElektraKey * key, ElektraKey * lookup)
{
	const ElektraKey * mk = elektraKeysetLookup (elektraKeyMeta (key), lookup, ELEKTRA_KDB_O_DEL);
	const char * value = mk == NULL ? NULL : elektraKeyString (mk);
	return value != NULL && value[0] == '\0' ? NULL : value;
}

static int addProcKey (ElektraKeyset * ks, const ElektraKey * key, ElektraKey * valueKey);
static ElektraKeyset * parseEnvp (const char ** envp);

static ElektraKeyset * parseArgs (ElektraKey * command, ElektraKeyset * optionsSpec, bool useSubcommands, int argc, const char ** argv, int * endArg,
			   ElektraKey * errorKey);
static void setOption (ElektraKey * option, const char * value, bool repeated);

static ElektraKey * splitEnvValue (const ElektraKey * envKey);

static ElektraKeyset * ksMetaGetSingleOrArray (ElektraKey * key, const char * metaName);

char * generateUsageLine (const char * progname, ElektraKey * command, const ElektraKey * commandArgs);
static char * generateOptionsList (ElektraKeyset * keysWithOpts, ElektraKey * command);
static char * generateCommandsList (ElektraKeyset * keysWithOpts, ElektraKey * commandKey);
static char * generateArgsList (ElektraKeyset * keysWithOpts, ElektraKey * command);
static char * generateEnvsList (ElektraKeyset * keysWithOpts);
static bool optionOrArgBelongsToCommand (const ElektraKey * command, const ElektraKey * optionOrArg);

static bool processSpec (struct Specification * spec, ElektraKeyset * ks, ElektraKey * specParent, ElektraKey * errorKey);
static bool processOptions (struct Specification * spec, ElektraKey * command, ElektraKey * specKey, ElektraKey ** keyWithOpt, ElektraKey * errorKey);
static bool readOptionData (struct OptionData * optionData, ElektraKey * key, const char * metaKey, ElektraKey * errorKey);
static bool processShortOptSpec (struct Specification * spec, struct OptionData * optionData, ElektraKey * command, ElektraKey ** keyWithOpt,
				 char ** shortOptLine, ElektraKey * errorKey);
static bool processLongOptSpec (struct Specification * spec, struct OptionData * optionData, ElektraKey * command, ElektraKey ** keyWithOpt,
				char ** longOptLine, ElektraKey * errorKey);
static bool processEnvVars (ElektraKeyset * usedEnvVars, ElektraKey * specKey, ElektraKey ** keyWithOpt, ElektraKey * errorKey);
static bool processArgs (ElektraKey * command, ElektraKey * specKey, ElektraKeyset * argIndices, ElektraKey ** keyWithOpt, ElektraKey * errorKey);

static int writeOptionValues (ElektraKeyset * ks, ElektraKey * keyWithOpt, ElektraKeyset * options, ElektraKey * errorKey);
static int writeEnvVarValues (ElektraKeyset * ks, ElektraKey * keyWithOpt, ElektraKeyset * envValues, ElektraKey * errorKey);
static int writeArgsValues (ElektraKeyset * ks, ElektraKey * keyWithOpt, ElektraKey * command, ElektraKeyset * argIndices, ElektraKeyset * args, ElektraKey * errorKey);

static bool parseLongOption (ElektraKey * command, ElektraKeyset * optionsSpec, ElektraKeyset * options, int argc, const char ** argv, int * index,
			     ElektraKey * errorKey);
static bool parseShortOptions (ElektraKey * command, ElektraKeyset * optionsSpec, ElektraKeyset * options, int argc, const char ** argv, int * index,
			       ElektraKey * errorKey);

static int writeOptions (ElektraKey * command, ElektraKey * commandKey, ElektraKey * commandArgs, bool writeArgs, bool * argsWritten, ElektraKeyset * options,
			 struct Specification * spec, ElektraKeyset * ks, const char * progname, const char ** envp, ElektraKey * parentKey);

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
int elektraGetOpts (ElektraKeyset * ks, int argc, const char ** argv, const char ** envp, ElektraKey * parentKey)
{
	elektraCursor initial = elektraKeysetGetCursor (ks);

	ElektraKey * specParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	// Translate key to spec namespace
	elektraKeySetNamespace (specParent, ELEKTRA_NS_SPEC);

	struct Specification spec;
	if (!processSpec (&spec, ks, specParent, parentKey))
	{
		elektraKeyDel (specParent);
		elektraKeysetSetCursor (ks, initial);
		return -1;
	}

	ElektraKey * command = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * commandKey = elektraKeyNew (elektraKeyName (specParent), ELEKTRA_KEY_END);
	ElektraKey * commandArgs = elektraKeyNew ("/", ELEKTRA_KEY_END);

	elektraKeyDel (specParent);

	if (spec.useSubcommands)
	{
		int lastEndArg = 0;
		while (lastEndArg < argc)
		{
			int endArg = -1;
			ElektraKeyset * options =
				parseArgs (command, spec.options, true, argc - lastEndArg, argv + lastEndArg, &endArg, parentKey);

			if (options == NULL)
			{
				elektraKeyDel (command);
				elektraKeyDel (commandKey);
				elektraKeyDel (commandArgs);
				elektraKeysetDel (spec.options);
				elektraKeysetDel (spec.keys);
				elektraKeysetDel (spec.argIndices);
				elektraKeysetDel (spec.commands);
				elektraKeysetSetCursor (ks, initial);
				return -1;
			}

			const ElektraKey * subCommand = NULL;
			if (endArg >= 0)
			{
				endArg += lastEndArg;

				ElektraKey * commandSpec = elektraKeysetLookup (spec.keys, commandKey, 0);
				ElektraKey * commandLookup = elektraKeyNew ("meta:/command", ELEKTRA_KEY_END);
				elektraKeyAddBaseName (commandLookup, argv[endArg]);
				subCommand = elektraKeysetLookup (elektraKeyMeta (commandSpec), commandLookup, ELEKTRA_KDB_O_DEL);
			}

			bool argsWritten = false;
			int result = writeOptions (command, commandKey, commandArgs, subCommand == NULL, &argsWritten, options, &spec, ks,
						   argv[0], envp, parentKey);
			elektraKeysetDel (options);

			if (result != 0)
			{
				elektraKeyDel (command);
				elektraKeyDel (commandKey);
				elektraKeyDel (commandArgs);
				elektraKeysetDel (spec.options);
				elektraKeysetDel (spec.keys);
				elektraKeysetDel (spec.argIndices);
				elektraKeysetDel (spec.commands);
				elektraKeysetSetCursor (ks, initial);
				return result;
			}

			if (subCommand == NULL && !argsWritten && endArg >= 0)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Unknown sub-command: %s", argv[endArg]);
				elektraKeyDel (command);
				elektraKeyDel (commandKey);
				elektraKeyDel (commandArgs);
				elektraKeysetDel (spec.options);
				elektraKeysetDel (spec.keys);
				elektraKeysetDel (spec.argIndices);
				elektraKeysetDel (spec.commands);
				elektraKeysetSetCursor (ks, initial);
				return -1;
			}

			ElektraKey * procKey = elektraKeyNew ("proc:/", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
			elektraKeyAddName (procKey, strchr (elektraKeyName (commandKey), '/'));
			elektraKeysetAppendKey (ks, procKey);

			if (subCommand == NULL)
			{
				elektraKeyDel (command);
				elektraKeyDel (commandKey);
				elektraKeyDel (commandArgs);
				elektraKeysetDel (spec.options);
				elektraKeysetDel (spec.keys);
				elektraKeysetDel (spec.argIndices);
				elektraKeysetDel (spec.commands);
				elektraKeysetSetCursor (ks, initial);
				return 0;
			}

			elektraKeySetString (procKey, elektraKeyString (subCommand));

			elektraKeyAddBaseName (command, elektraKeyString (subCommand));
			elektraKeyAddBaseName (commandKey, elektraKeyString (subCommand));
			elektraKeyAddBaseName (commandArgs, argv[endArg]);

			lastEndArg = endArg;
		}

		ELEKTRA_ASSERT (0, "should be unreachable");
		return -2;
	}
	else
	{
		int endArg = 0;
		ElektraKeyset * options = parseArgs (command, spec.options, false, argc, argv, &endArg, parentKey);

		if (options == NULL)
		{
			elektraKeyDel (command);
			elektraKeyDel (commandKey);
			elektraKeyDel (commandArgs);
			elektraKeysetDel (spec.options);
			elektraKeysetDel (spec.keys);
			elektraKeysetDel (spec.argIndices);
			elektraKeysetDel (spec.commands);
			elektraKeysetSetCursor (ks, initial);
			return -1;
		}

		int result = writeOptions (command, commandKey, commandArgs, true, NULL, options, &spec, ks, argv[0], envp, parentKey);
		elektraKeyDel (command);
		elektraKeyDel (commandKey);
		elektraKeyDel (commandArgs);
		elektraKeysetDel (options);
		elektraKeysetDel (spec.options);
		elektraKeysetDel (spec.keys);
		elektraKeysetDel (spec.argIndices);
		elektraKeysetDel (spec.commands);
		elektraKeysetSetCursor (ks, initial);
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
const char * elektraGetOptsHelpCommand (ElektraKey * errorKey)
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
char * elektraGetOptsHelpMessage (ElektraKey * helpKey, const char * usage, const char * prefix)
{
	const char * command = elektraGetOptsHelpCommand (helpKey);

	ElektraKey * lookup;
	if (usage == NULL)
	{
		lookup = elektraKeyNew ("meta:/internal/libopts/help/usage", ELEKTRA_KEY_END);
		elektraKeyAddBaseName (lookup, command);
		usage = keyGetMetaStringByKey (helpKey, lookup);
	}

	if (usage == NULL)
	{
		return NULL;
	}

	lookup = elektraKeyNew ("meta:/internal/libopts/help/options", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (lookup, command);
	const char * options = keyGetMetaStringByKey (helpKey, lookup);
	if (options == NULL)
	{
		options = "";
	}

	lookup = elektraKeyNew ("meta:/internal/libopts/help/commands", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (lookup, command);
	const char * commands = keyGetMetaStringByKey (helpKey, lookup);
	if (commands == NULL)
	{
		commands = "";
	}

	lookup = elektraKeyNew ("meta:/internal/libopts/help/args", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (lookup, command);
	const char * args = keyGetMetaStringByKey (helpKey, lookup);
	if (args == NULL)
	{
		args = "";
	}

	lookup = elektraKeyNew ("meta:/internal/libopts/help/envs", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (lookup, command);
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
bool processSpec (struct Specification * spec, ElektraKeyset * ks, ElektraKey * specParent, ElektraKey * errorKey)
{
	size_t specParentLen = strlen (elektraKeyName (specParent));

	// This block determines whether the spec uses sub-commands.
	bool useSubcommands = false;
	{
		ElektraKey * parent = elektraKeysetLookupByName (ks, elektraKeyName (specParent), 0);
		if (parent != NULL)
		{
			const ElektraKey * commandMeta = elektraKeyGetMeta (parent, "command");
			const char * commandMetaString = elektraKeyString (commandMeta);
			if (commandMetaString != NULL && strlen (commandMetaString) == 0)
			{
				useSubcommands = true;
			}
			else if (commandMeta != NULL)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey, "On the parent key 'command' can only be set to an empty string. Offending key: %s",
					elektraKeyName (parent));
				return false;
			}
		}
	}

	ElektraKeyset * usedEnvVars = elektraKeysetNew (0, ELEKTRA_KS_END);
	spec->options = elektraKeysetNew (
		1, elektraKeyNew ("/long/help", ELEKTRA_KEY_META, "hasarg", "none", ELEKTRA_KEY_META, "kind", "single", ELEKTRA_KEY_META, "flagvalue", "1", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	spec->keys = elektraKeysetNew (0, ELEKTRA_KS_END);
	spec->argIndices = elektraKeysetNew (0, ELEKTRA_KS_END);
	spec->commands = elektraKeysetNew (0, ELEKTRA_KS_END);

	/**
	 * 1. Process all keys in the @p ks and
	 * 	a. Validate sub-commands (e.g., whether meta values are set correctly and the hierarchy of (sub-)commands is legal)
	 * 	b. Generate help text for each sub-command.
	 * 	c. Validate all options (long and short), arguments and environment variables, generate help texts for each and add them
	 * into the @spec.
	 */
	for (elektraCursor i = 0; i < elektraKeysetGetSize (ks); ++i)
	{
		ElektraKey * cur = elektraKeysetAtCursor (ks, i);

		// Keys that aren't in the spec namespace or below the parent key are ignored.
		if (elektraKeyGetNamespace (cur) != ELEKTRA_NS_SPEC || !elektraKeyIsBelowOrSame (specParent, cur))
		{
			continue;
		}

		bool isParentKey = strcmp (elektraKeyName (cur), elektraKeyName (specParent)) == 0;

		ElektraKey * keyWithOpt = NULL;

		// step 1a.) Validate sub-commands
		// If meta key "command" is set, the current key is a sub-command.
		const ElektraKey * commandMeta = elektraKeyGetMeta (cur, "command");
		if (commandMeta != NULL)
		{
			if (!useSubcommands)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey, "'command' can only be used, if it is set on the parent key as well. Offending key: %s",
					elektraKeyName (cur));
				elektraKeysetDel (spec->options);
				elektraKeysetDel (spec->argIndices);
				elektraKeysetDel (spec->commands);
				elektraKeysetDel (spec->keys);
				elektraKeysetDel (usedEnvVars);
				return false;
			}

			const char * commandMetaString = elektraKeyString (commandMeta);
			if (commandMetaString == NULL || (strlen (commandMetaString) == 0 && !isParentKey))
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey,
					"'command' must be set to a non-empty string (except on the parent key). Offending key: %s",
					elektraKeyName (cur));
				elektraKeysetDel (spec->options);
				elektraKeysetDel (spec->argIndices);
				elektraKeysetDel (spec->commands);
				elektraKeysetDel (spec->keys);
				elektraKeysetDel (usedEnvVars);
				return false;
			}

			if (keyWithOpt == NULL)
			{
				keyWithOpt = elektraKeyNew (elektraKeyName (cur), ELEKTRA_KEY_META, "command", "1", ELEKTRA_KEY_END);
			}

			// step 1b.)
			const char * optHelp = keyGetMetaString (cur, "opt/help");
			const char * description = keyGetMetaString (cur, "description");

			const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

			char * commandHelp = elektraFormat ("  %-28s%s", commandMetaString, help);
			elektraKeySetMeta (keyWithOpt, "command/help", commandHelp);
			elektraFree (commandHelp);

			if (!isParentKey)
			{
				ElektraKey * helpKey = elektraKeyNew (elektraKeyName (cur) + specParentLen, ELEKTRA_KEY_META, "hasarg", "none", ELEKTRA_KEY_META, "kind",
							"single", ELEKTRA_KEY_META, "flagvalue", "1", ELEKTRA_KEY_END);
				elektraKeyAddName (helpKey, "/long/help");
				elektraKeysetAppendKey (spec->options, helpKey);
			}
		}

		ElektraKey * command = elektraKeyNew ("/", ELEKTRA_KEY_VALUE, elektraKeyName (specParent), ELEKTRA_KEY_END);
		if (useSubcommands && !isParentKey)
		{
			// Determine name of the parent of cur
			ElektraKey * curParent = elektraKeyNew (elektraKeyName (cur), ELEKTRA_KEY_END);
			if (strcmp (elektraKeyBaseName (curParent), "#") == 0)
			{
				elektraKeySetBaseName (curParent, NULL); // remove #
			}
			elektraKeySetBaseName (curParent, NULL);

			// Check if parent of current key exists in the KeySet
			ElektraKey * maybeCommand = elektraKeysetLookup (ks, curParent, ELEKTRA_KDB_O_DEL);
			if (maybeCommand == NULL)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
									"The parent of this key (%s) must have the 'command' metakey set. "
									"Offending key: parent doesn't exist",
									elektraKeyName (cur));
				elektraKeyDel (keyWithOpt);
				elektraKeyDel (command);
				elektraKeysetDel (spec->options);
				elektraKeysetDel (spec->argIndices);
				elektraKeysetDel (spec->commands);
				elektraKeysetDel (spec->keys);
				elektraKeysetDel (usedEnvVars);
				return false;
			}

			// Check if parent of current key has metakey "command" set
			const char * commandMetaString = keyGetMetaString (maybeCommand, "command");
			if (commandMetaString == NULL && strcmp (elektraKeyName (maybeCommand), elektraKeyName (specParent)) != 0)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					errorKey, "The parent of this key (%s) must have the 'command' metakey set. Offending key: %s",
					elektraKeyName (cur), elektraKeyName (maybeCommand));
				elektraKeyDel (keyWithOpt);
				elektraKeyDel (command);
				elektraKeysetDel (spec->options);
				elektraKeysetDel (spec->argIndices);
				elektraKeysetDel (spec->commands);
				elektraKeysetDel (spec->keys);
				elektraKeysetDel (usedEnvVars);
				return false;
			}

			if (commandMeta != NULL)
			{
				// add sub-command to parent command
				ElektraKey * parentCommand = elektraKeysetLookup (spec->keys, maybeCommand, 0);
				ElektraKey * subCommand = elektraKeyNew ("meta:/command", ELEKTRA_KEY_VALUE, elektraKeyBaseName (cur), ELEKTRA_KEY_END);
				elektraKeyAddBaseName (subCommand, elektraKeyString (commandMeta));
				if (elektraKeysetLookup (elektraKeyMeta (parentCommand), subCommand, 0) != NULL)
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Duplicate sub-command '%s'. Offending key: %s",
										elektraKeyString (commandMeta), elektraKeyName (cur));
					elektraKeyDel (subCommand);
					elektraKeyDel (keyWithOpt);
					elektraKeyDel (command);
					elektraKeysetDel (spec->options);
					elektraKeysetDel (spec->argIndices);
					elektraKeysetDel (spec->keys);
					elektraKeysetDel (spec->commands);
					elektraKeysetDel (usedEnvVars);
					return false;
				}

				elektraKeysetAppendKey (elektraKeyMeta (parentCommand), subCommand);
			}

			elektraKeyAddName (command, elektraKeyName (maybeCommand) + specParentLen);
			elektraKeySetString (command, elektraKeyString (maybeCommand));

			if (commandMeta != NULL)
			{
				elektraKeySetMeta (command, "hassubcommands", "1");
			}
		}

		elektraKeyCopyAllMeta (command, elektraKeysetLookup (spec->commands, command, ELEKTRA_KDB_O_CREATE));

		// step 1c.)
		if (!processOptions (spec, command, cur, &keyWithOpt, errorKey))
		{
			elektraKeyDel (command);
			elektraKeyDel (keyWithOpt);
			elektraKeysetDel (spec->argIndices);
			elektraKeysetDel (spec->options);
			elektraKeysetDel (spec->keys);
			elektraKeysetDel (spec->commands);
			elektraKeysetDel (usedEnvVars);
			return false;
		}

		if (!processEnvVars (usedEnvVars, cur, &keyWithOpt, errorKey))
		{
			elektraKeyDel (command);
			elektraKeyDel (keyWithOpt);
			elektraKeysetDel (spec->argIndices);
			elektraKeysetDel (spec->options);
			elektraKeysetDel (spec->keys);
			elektraKeysetDel (spec->commands);
			elektraKeysetDel (usedEnvVars);
			return false;
		}

		if (!processArgs (command, cur, spec->argIndices, &keyWithOpt, errorKey))
		{
			elektraKeyDel (command);
			elektraKeyDel (keyWithOpt);
			elektraKeysetDel (spec->argIndices);
			elektraKeysetDel (spec->options);
			elektraKeysetDel (spec->keys);
			elektraKeysetDel (spec->commands);
			elektraKeysetDel (usedEnvVars);
			return false;
		}

		elektraKeyCopyAllMeta (elektraKeysetLookup (spec->commands, command, ELEKTRA_KDB_O_CREATE), command);

		elektraKeyDel (command);

		if (keyWithOpt != NULL)
		{
			// Add the processed key to the KeySet
			elektraKeysetAppendKey (spec->keys, keyWithOpt);
		}
	}
	elektraKeysetDel (usedEnvVars);

	for (kdb_long_long_t i = 0; i < elektraKeysetGetSize (spec->argIndices); i++)
	{
		ElektraKey * cur = elektraKeysetAtCursor (spec->argIndices, i);
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
				elektraKeysetDel (spec->argIndices);
				elektraKeysetDel (spec->options);
				elektraKeysetDel (spec->keys);
				elektraKeysetDel (spec->commands);
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
bool processOptions (struct Specification * spec, ElektraKey * command, ElektraKey * specKey, ElektraKey ** keyWithOpt, ElektraKey * errorKey)
{
	const char * optHelp = keyGetMetaString (specKey, "opt/help");
	const char * description = keyGetMetaString (specKey, "description");

	const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

	ElektraKeyset * opts = ksMetaGetSingleOrArray (specKey, "opt");
	if (opts == NULL)
	{
		const char * longOpt = keyGetMetaString (specKey, "opt/long");
		if (longOpt == NULL)
		{
			return true;
		}

		// no other way to create Key with name "opt"
		ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_META, "opt", "", ELEKTRA_KEY_END);
		opts = elektraKeysetNew (2, elektraKeyNew ("meta:/#", ELEKTRA_KEY_END), elektraKeyGetMeta (k, "opt"), ELEKTRA_KS_END);
		elektraKeyDel (k);
	}

	elektraKeysetRewind (opts);
	elektraKeysetNext (opts); // skip count
	ElektraKey * metaKey;

	char * shortOptLine = elektraStrDup ("");
	char * longOptLine = elektraStrDup ("");
	while ((metaKey = elektraKeysetNext (opts)) != NULL)
	{
		struct OptionData optionData;

		if (!readOptionData (&optionData, specKey, elektraKeyName (metaKey), errorKey))
		{
			elektraKeysetDel (opts);
			elektraFree (shortOptLine);
			elektraFree (longOptLine);
			return false;
		}


		if (!processShortOptSpec (spec, &optionData, command, keyWithOpt, &shortOptLine, errorKey))
		{
			elektraKeysetDel (opts);
			elektraFree (shortOptLine);
			elektraFree (longOptLine);
			return false;
		}

		if (!processLongOptSpec (spec, &optionData, command, keyWithOpt, &longOptLine, errorKey))
		{
			elektraKeysetDel (opts);
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

			elektraKeySetMeta (*keyWithOpt, "opt/help", optsLine);
			elektraFree (optsLine);
		}
		elektraFree (optsLinePart);
	}
	else
	{
		elektraFree (shortOptLine);
		elektraFree (longOptLine);
	}

	elektraKeysetDel (opts);

	return true;
}

/**
 * Read the option data (i.e. hasarg, flagvalue, etc.) for the option
 * given by @p metaKey 's name from @p key.
 * @retval true on success
 * @retval false on error
 */
bool readOptionData (struct OptionData * optionData, ElektraKey * key, const char * metaKey, ElektraKey * errorKey)
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
			elektraKeyName (key));
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
	if (elektraStrCmp (elektraKeyBaseName (key), "#") == 0)
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
bool processShortOptSpec (struct Specification * spec, struct OptionData * optionData, ElektraKey * command, ElektraKey ** keyWithOpt,
			  char ** shortOptLine, ElektraKey * errorKey)
{
	ElektraKey * key = optionData->specKey;
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
							elektraKeyName (key));
		return false;
	}

	const char * commandName = command != NULL ? elektraKeyName (command) : "/";
	ElektraKey * shortOptSpec = elektraKeyNew (commandName, ELEKTRA_KEY_META, "key", elektraKeyName (key), ELEKTRA_KEY_META, "hasarg", hasArg, ELEKTRA_KEY_META, "kind", kind,
				     ELEKTRA_KEY_META, "flagvalue", flagValue, ELEKTRA_KEY_END);
	elektraKeyAddBaseName (shortOptSpec, "short");
	elektraKeyAddBaseName (shortOptSpec, (char[]){ shortOpt, '\0' });

	ElektraKey * existing = elektraKeysetLookupByName (spec->options, elektraKeyName (shortOptSpec), 0);
	if (existing != NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"The option '-%c' has already been specified for the key '%s'. Additional key: %s",
							shortOpt, keyGetMetaString (existing, "key"), elektraKeyName (key));
		elektraKeyDel (shortOptSpec);
		return false;
	}

	elektraKeysetAppendKey (spec->options, shortOptSpec);

	if (*keyWithOpt == NULL)
	{
		// Mark this option as belonging to command "command".
		*keyWithOpt = elektraKeyNew (elektraKeyName (key), ELEKTRA_KEY_META, META_COMMAND_KEY, elektraKeyName (command), ELEKTRA_KEY_END);
	}
	elektraMetaArrayAdd (*keyWithOpt, "opt", elektraKeyName (shortOptSpec));

	if (!hidden)
	{
		char * argString = "";
		if (elektraStrCmp (hasArg, "required") == 0)
		{
			argString = argName == NULL ? " ARG" : elektraFormat (" %s", argName);
		}

		char * newShortOptLine = elektraFormat ("%s-%c%s, ", *shortOptLine, shortOpt, argString);
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
bool processLongOptSpec (struct Specification * spec, struct OptionData * optionData, ElektraKey * command, ElektraKey ** keyWithOpt, char ** longOptLine,
			 ElektraKey * errorKey)
{
	ElektraKey * key = optionData->specKey;
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
							elektraKeyName (key));
		return false;
	}

	const char * commandName = command != NULL ? elektraKeyName (command) : "/";
	ElektraKey * longOptSpec = elektraKeyNew (commandName, ELEKTRA_KEY_META, "key", elektraKeyName (key), ELEKTRA_KEY_META, "hasarg", hasArg, ELEKTRA_KEY_META, "kind", kind,
				    ELEKTRA_KEY_META, "flagvalue", flagValue, ELEKTRA_KEY_END);
	elektraKeyAddBaseName (longOptSpec, "long");
	elektraKeyAddBaseName (longOptSpec, longOpt);

	ElektraKey * existing = elektraKeysetLookupByName (spec->options, elektraKeyName (longOptSpec), 0);
	if (existing != NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"The option '--%s' has already been specified for the key '%s'. Additional key: %s",
							longOpt, keyGetMetaString (existing, "key"), elektraKeyName (key));
		elektraKeyDel (longOptSpec);
		return false;
	}

	elektraKeysetAppendKey (spec->options, longOptSpec);

	if (*keyWithOpt == NULL)
	{
		// Mark this option as belonging to command "command".
		*keyWithOpt = elektraKeyNew (elektraKeyName (key), ELEKTRA_KEY_META, META_COMMAND_KEY, elektraKeyName (command), ELEKTRA_KEY_END);
	}
	elektraMetaArrayAdd (*keyWithOpt, "opt", elektraKeyName (longOptSpec));

	if (!hidden)
	{
		char * argString = "";
		if (elektraStrCmp (hasArg, "required") == 0)
		{
			argString = argName == NULL ? "=ARG" : elektraFormat ("=%s", argName);
		}
		else if (elektraStrCmp (hasArg, "optional") == 0)
		{
			argString = argName == NULL ? "=[ARG]" : elektraFormat ("=[%s]", argName);
		}


		char * newLongOptLine = elektraFormat ("%s--%s%s, ", *longOptLine, longOpt, argString);
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
bool processEnvVars (ElektraKeyset * usedEnvVars, ElektraKey * specKey, ElektraKey ** keyWithOpt, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	const char * optHelp = keyGetMetaString (specKey, "opt/help");
	const char * description = keyGetMetaString (specKey, "description");

	const char * help = optHelp != NULL ? optHelp : (description != NULL ? description : "");

	ElektraKeyset * envVars = ksMetaGetSingleOrArray (specKey, "env");
	if (envVars == NULL)
	{
		return true;
	}

	char * envsLinePart = elektraStrDup ("");

	elektraKeysetRewind (envVars);
	elektraKeysetNext (envVars); // skip count
	ElektraKey * k;
	while ((k = elektraKeysetNext (envVars)) != NULL)
	{
		const char * envVar = elektraKeyString (k);
		if (envVar == NULL)
		{
			continue;
		}

		ElektraKey * envVarKey = elektraKeyNew ("/", ELEKTRA_KEY_META, "key", elektraKeyName (specKey), ELEKTRA_KEY_END);
		elektraKeyAddBaseName (envVarKey, envVar);

		ElektraKey * existing = elektraKeysetLookup (usedEnvVars, envVarKey, 0);
		if (existing != NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "The environment variable '%s' has already been specified for the key '%s'. Additional key: %s",
				envVar, keyGetMetaString (existing, "key"), elektraKeyName (specKey));
			elektraFree (envsLinePart);
			elektraKeyDel (envVarKey);
			elektraKeysetDel (envVars);
			return false;
		}

		elektraKeysetAppendKey (usedEnvVars, envVarKey);

		if (*keyWithOpt == NULL)
		{
			*keyWithOpt = elektraKeyNew (elektraKeyName (specKey), ELEKTRA_KEY_END);
		}
		elektraMetaArrayAdd (*keyWithOpt, "env", elektraKeyName (envVarKey));

		char * tmp = elektraFormat ("%s%s, ", envsLinePart, envVar);
		elektraFree (envsLinePart);
		envsLinePart = tmp;
	}

	elektraKeysetDel (envVars);

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

	elektraKeySetMeta (*keyWithOpt, "env/help", envsLine);
	elektraFree (envsLine);
	elektraFree (envsLinePart);
	return true;
}

bool processArgs (ElektraKey * command, ElektraKey * specKey, ElektraKeyset * argIndices, ElektraKey ** keyWithOpt, ElektraKey * errorKey)
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
		if (elektraStrCmp (elektraKeyBaseName (specKey), "#") != 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args=remaining' can only be set on array keys (basename = '#'). Offending key: %s",
				elektraKeyName (specKey));
			return false;
		}

		if (*keyWithOpt == NULL)
		{
			// Mark this arg as belonging to command "command".
			*keyWithOpt = elektraKeyNew (elektraKeyName (specKey), ELEKTRA_KEY_META, META_COMMAND_KEY, elektraKeyName (command), ELEKTRA_KEY_END);
		}
		elektraKeySetMeta (*keyWithOpt, "args", "remaining");

		ElektraKey * dup = elektraKeyDup (specKey, ELEKTRA_KEY_CP_ALL);
		elektraKeySetBaseName (dup, NULL); // remove #

		const char * existing = keyGetMetaString (command, "remainingargskey");
		if (existing != NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'args=remaining' is already used on key '%s'. Offending key: %s",
								existing, elektraKeyName (specKey));
			elektraKeyDel (dup);
			return false;
		}

		elektraKeySetMeta (command, "remainingargs", elektraKeyBaseName (dup));
		elektraKeySetMeta (command, "remainingargskey", elektraKeyName (specKey));

		char * argName = elektraFormat ("%s...", elektraKeyBaseName (dup));
		char * argHelp = elektraFormat ("  %-28s%s", argName, help);
		elektraKeySetMeta (*keyWithOpt, "args/help", argHelp);
		elektraFree (argHelp);
		elektraFree (argName);
		elektraKeyDel (dup);
	}
	else if (elektraStrCmp (argsMeta, "indexed") == 0)
	{
		if (elektraStrCmp (elektraKeyBaseName (specKey), "#") == 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args=indexed' can only be set on non-array keys (basename != '#'). Offending key: %s",
				elektraKeyName (specKey));
			return false;
		}

		const ElektraKey * argsIndex = elektraKeyGetMeta (specKey, "args/index");
		if (argsIndex == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args=indexed' must be accompanied by 'args/index'. Offending key: %s", elektraKeyName (specKey));
			return false;
		}

		kdb_long_long_t indexValue;
		if (!elektraKeyToLongLong (argsIndex, &indexValue) || indexValue < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
								"'args/index' must be a non-negative integer not '%s'. Offending key: %s",
								elektraKeyString (argsIndex), elektraKeyName (specKey));
			return false;
		}

		if (*keyWithOpt == NULL)
		{
			// Mark this arg as belonging to command "command".
			*keyWithOpt = elektraKeyNew (elektraKeyName (specKey), ELEKTRA_KEY_META, META_COMMAND_KEY, elektraKeyName (command), ELEKTRA_KEY_END);
		}
		elektraKeySetMeta (*keyWithOpt, "args", "indexed");
		elektraKeySetMeta (*keyWithOpt, "args/index", elektraKeyString (argsIndex));

		ElektraKey * indexKey = elektraKeysetLookup (argIndices, elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END), ELEKTRA_KDB_O_DEL | ELEKTRA_KDB_O_CREATE);
		elektraKeySetMeta (indexKey, "key", elektraKeyString (command));

		char indexMetaName[ELEKTRA_MAX_ARRAY_SIZE + sizeof ("index/")];
		strcpy (indexMetaName, "index/");
		elektraWriteArrayNumber (indexMetaName + sizeof ("index"), indexValue);

		const char * maxIndex = keyGetMetaString (indexKey, "index");
		if (maxIndex == NULL || strcmp (maxIndex, indexMetaName + sizeof ("index")) < 0)
		{
			elektraKeySetMeta (indexKey, "index", indexMetaName + sizeof ("index"));
		}

		const char * existing = keyGetMetaString (indexKey, indexMetaName);
		if (existing != NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "'args/index=" ELEKTRA_LONG_LONG_F "' is already used by '%s'. Offending Key: %s", indexValue,
				existing, elektraKeyName (specKey));
			return false;
		}

		elektraKeySetMeta (indexKey, indexMetaName, elektraKeyName (specKey));

		char * argHelp = elektraFormat ("  %-28s%s", elektraKeyBaseName (specKey), help);
		elektraKeySetMeta (*keyWithOpt, "args/help", argHelp);
		elektraFree (argHelp);

		elektraMetaArrayAdd (command, "args", elektraKeyBaseName (specKey));
	}
	else
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "unknown value for 'args' metadata: '%s'. Offending key: %s", argsMeta,
							elektraKeyName (specKey));
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
int writeOptionValues (ElektraKeyset * ks, ElektraKey * keyWithOpt, ElektraKeyset * options, ElektraKey * errorKey)
{
	bool valueFound = false;

	ElektraKeyset * optMetas = elektraMetaArrayToKS (keyWithOpt, "opt");
	if (optMetas == NULL)
	{
		return 0;
	}

	elektraKeysetRewind (optMetas);
	elektraKeysetNext (optMetas); // skip count
	ElektraKey * optMeta;
	bool shortFound = false;
	while ((optMeta = elektraKeysetNext (optMetas)) != NULL)
	{
		ElektraKey * optLookup = elektraKeyNew (elektraKeyString (optMeta), ELEKTRA_KEY_END);
		ElektraKey * optKey = elektraKeysetLookup (options, optLookup, ELEKTRA_KDB_O_DEL);
		bool isShort = strncmp (elektraKeyString (optMeta), "/short", 6) == 0;
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
				isShort ? "-" : "--", isShort ? (const char[]){ elektraKeyBaseName (optKey)[0], '\0' } : elektraKeyBaseName (optKey),
				elektraKeyName (keyWithOpt));
			elektraKeysetDel (optMetas);
			return -1;
		}
	}

	elektraKeysetDel (optMetas);

	return valueFound ? 1 : 0;
}

/**
 * Add keys to the proc namespace in @p ks for everything that is specified
 * by the 'env' metadata on @p keyWithOpt. The env-vars are taken from @p envValues.
 * @retval -1 in case of error
 * @retval 0 if no key was added
 * @retval 1 if keys were added to @p ks
 */
int writeEnvVarValues (ElektraKeyset * ks, ElektraKey * keyWithOpt, ElektraKeyset * envValues, ElektraKey * errorKey)
{
	bool valueFound = false;

	ElektraKeyset * envMetas = elektraMetaArrayToKS (keyWithOpt, "env");
	if (envMetas == NULL)
	{
		return 0;
	}

	elektraKeysetRewind (envMetas);
	elektraKeysetNext (envMetas); // skip count
	ElektraKey * envMeta;
	while ((envMeta = elektraKeysetNext (envMetas)) != NULL)
	{
		ElektraKey * envKey = elektraKeysetLookupByName (envValues, elektraKeyString (envMeta), 0);

		bool isArray = strcmp (elektraKeyBaseName (keyWithOpt), "#") == 0;
		ElektraKey * envValueKey;
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
			envValueKey = elektraKeyNew (elektraKeyName (envKey), ELEKTRA_KEY_VALUE, elektraKeyString (envKey), ELEKTRA_KEY_END);
		}

		int res = addProcKey (ks, keyWithOpt, envValueKey);
		if (res < 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey,
				"The environment variable '%s' cannot be used, because another variable has "
				"already been used for the key '%s'.",
				elektraKeyBaseName (envKey), elektraKeyName (keyWithOpt));
			elektraKeyDel (envValueKey);
			elektraKeysetDel (envMetas);
			return -1;
		}
		else if (res == 0)
		{
			valueFound = true;
		}
		elektraKeyDel (envValueKey);
	}
	elektraKeysetDel (envMetas);

	return valueFound ? 1 : 0;
}

/**
 * Add keys to the proc namespace in @p ks for everything that is specified
 * by the 'args' metadata on @p keyWithOpt. The args are taken from @p args.
 * @retval -1 in case of error
 * @retval 0 if no key was added
 * @retval 1 if keys were added to @p ks
 */
int writeArgsValues (ElektraKeyset * ks, ElektraKey * keyWithOpt, ElektraKey * command, ElektraKeyset * argIndices, ElektraKeyset * args, ElektraKey * errorKey)
{
	const char * argsMeta = keyGetMetaString (keyWithOpt, "args");
	if (argsMeta == NULL)
	{
		return 0;
	}

	if (strcmp (argsMeta, "remaining") == 0)
	{
		ElektraKey * argIndex = elektraKeysetLookup (argIndices, command, 0);
		ElektraKeyset * indices = elektraMetaArrayToKS (argIndex, "index");
		elektraCursor firstRemainingArg = argIndex == NULL ? 0 : elektraKeysetGetSize (indices) - 1; // -1 because of parent
		elektraKeysetDel (indices);

		ElektraKey * procKey = elektraKeyNew ("proc:/", ELEKTRA_KEY_END);
		elektraKeyAddName (procKey, strchr (elektraKeyName (keyWithOpt), '/'));

		ElektraKey * insertKey = elektraKeyDup (procKey, ELEKTRA_KEY_CP_NAME);

		elektraKeysetRewind (args);
		for (elektraCursor i = firstRemainingArg; i < elektraKeysetGetSize (args); ++i)
		{
			ElektraKey * arg = elektraKeysetAtCursor (args, i);
			elektraArrayIncName (insertKey);

			ElektraKey * k = elektraKeyDup (insertKey, ELEKTRA_KEY_CP_NAME);
			elektraKeySetString (k, elektraKeyString (arg));
			elektraKeysetAppendKey (ks, k);
		}

		elektraKeySetBaseName (procKey, NULL); // remove #
		if (strcmp (elektraKeyBaseName (insertKey), "#") != 0)
		{
			elektraKeySetMeta (procKey, "array", elektraKeyBaseName (insertKey));
		}
		elektraKeysetAppendKey (ks, procKey);
		elektraKeyDel (insertKey);
		return 1;
	}
	else if (strcmp (argsMeta, "indexed") == 0)
	{
		kdb_long_long_t index;
		elektraKeyToLongLong (elektraKeyGetMeta (keyWithOpt, "args/index"), &index);

		char arrayIndex[ELEKTRA_MAX_ARRAY_SIZE];
		elektraWriteArrayNumber (arrayIndex, index);

		ElektraKey * argKey = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END);
		elektraKeyAddBaseName (argKey, "args");
		elektraKeyAddBaseName (argKey, arrayIndex);
		ElektraKey * arg = elektraKeysetLookup (args, argKey, ELEKTRA_KDB_O_DEL);
		if (arg == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "Expected at least " ELEKTRA_LONG_LONG_F " non-option arguments, but only got %zd", index + 1,
				elektraKeysetGetSize (args));
			return -1;
		}

		ElektraKey * procKey = elektraKeyNew ("proc:/", ELEKTRA_KEY_END);
		elektraKeyAddName (procKey, strchr (elektraKeyName (keyWithOpt), '/'));
		elektraKeySetString (procKey, elektraKeyString (arg));
		elektraKeysetAppendKey (ks, procKey);
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
ElektraKey * splitEnvValue (const ElektraKey * envKey)
{
	ElektraKey * valueKey = elektraKeyNew (elektraKeyName (envKey), ELEKTRA_KEY_END);

	char * envValue = elektraStrDup (elektraKeyString (envKey));
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
int addProcKey (ElektraKeyset * ks, const ElektraKey * key, ElektraKey * valueKey)
{
	if (ks == NULL || key == NULL || valueKey == NULL)
	{
		return 1;
	}

	ElektraKey * procKey = elektraKeyNew ("proc:/", ELEKTRA_KEY_END);
	elektraKeyAddName (procKey, strchr (elektraKeyName (key), '/'));

	bool isArrayKey = elektraStrCmp (elektraKeyBaseName (procKey), "#") == 0;
	if (isArrayKey)
	{
		elektraKeySetBaseName (procKey, NULL); // remove # (for lookup)
	}


	ElektraKey * existing = elektraKeysetLookupByName (ks, elektraKeyName (procKey), 0);
	if (existing != NULL)
	{
		const char * value = isArrayKey ? keyGetMetaString (existing, "array") : elektraKeyString (existing);
		if (value != NULL && strlen (value) > 0)
		{
			elektraKeyDel (procKey);
			return -1;
		}
	}

	if (isArrayKey)
	{
		ElektraKey * insertKey = elektraKeyDup (procKey, ELEKTRA_KEY_CP_NAME);
		elektraKeyAddBaseName (insertKey, "#");
		ElektraKeyset * values = elektraMetaArrayToKS (valueKey, "values");
		if (values == NULL)
		{
			elektraKeyDel (procKey);
			elektraKeyDel (insertKey);
			return 1;
		}

		elektraKeysetRewind (values);
		ElektraKey * cur;
		elektraKeysetNext (values); // skip count
		while ((cur = elektraKeysetNext (values)) != NULL)
		{
			elektraArrayIncName (insertKey);

			ElektraKey * k = elektraKeyDup (insertKey, ELEKTRA_KEY_CP_NAME);
			elektraKeySetString (k, elektraKeyString (cur));
			elektraKeysetAppendKey (ks, k);
		}

		elektraKeySetMeta (procKey, "array", elektraKeyBaseName (insertKey));
		elektraKeyDel (insertKey);
		elektraKeysetDel (values);
	}
	else
	{
		elektraKeySetString (procKey, elektraKeyString (valueKey));
	}

	return elektraKeysetAppendKey (ks, procKey) > 0 ? 0 : 1;
}

/**
 * Parses env-vars from envp into an internal format.
 */
ElektraKeyset * parseEnvp (const char ** envp)
{
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	const char ** cur = envp;
	while (*cur != NULL)
	{
		const char * eq = strchr (*cur, '=');
		ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_VALUE, eq + 1, ELEKTRA_KEY_END);
		size_t len = eq - *cur;
		char * name = elektraMemDup (*cur, len + 1);
		name[len] = '\0';
		elektraKeyAddBaseName (key, name);
		elektraKeysetAppendKey (ks, key);
		elektraFree (name);

		cur++;
	}

	return ks;
}

/**
 * Parses command-line arguments (options and parameters) from argc/argv into an internal format.
 */
ElektraKeyset * parseArgs (ElektraKey * command, ElektraKeyset * optionsSpec, bool useSubcommands, int argc, const char ** argv, int * endArg, ElektraKey * errorKey)
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

	ElektraKey * argKey = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END);
	elektraKeyAddName (argKey, "/args/#");

	ElektraKeyset * options = elektraKeysetNew (0, ELEKTRA_KS_END);
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
					elektraKeyDel (argKey);
					elektraKeysetDel (options);
					return NULL;
				}

				continue;
			}

			if (!parseShortOptions (command, optionsSpec, options, argc, argv, &i, errorKey))
			{
				elektraKeyDel (argKey);
				elektraKeysetDel (options);
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
			ElektraKey * newArgKey = elektraKeyDup (argKey, ELEKTRA_KEY_CP_NAME);
			elektraKeySetString (newArgKey, cur);
			elektraKeysetAppendKey (options, newArgKey);
		}
	}

	// collect rest of argv
	for (; i < argc; ++i)
	{
		elektraArrayIncName (argKey);
		ElektraKey * newArgKey = elektraKeyDup (argKey, ELEKTRA_KEY_CP_NAME);
		elektraKeySetString (newArgKey, argv[i]);
		elektraKeysetAppendKey (options, newArgKey);
	}

	ElektraKey * argsParent = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_VALUE, elektraKeyBaseName (argKey), ELEKTRA_KEY_END);
	elektraKeyAddBaseName (argsParent, "args");
	elektraKeysetAppendKey (options, argsParent);
	elektraKeyDel (argKey);

	return options;
}

bool parseShortOptions (ElektraKey * command, ElektraKeyset * optionsSpec, ElektraKeyset * options, int argc, const char ** argv, int * index, ElektraKey * errorKey)
{
	int i = *index;
	for (const char * c = &argv[i][1]; *c != '\0'; ++c)
	{

		ElektraKey * shortOpt = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END);
		elektraKeyAddBaseName (shortOpt, "short");
		elektraKeyAddBaseName (shortOpt, (char[]){ *c, '\0' });

		ElektraKey * optSpec = elektraKeysetLookupByName (optionsSpec, elektraKeyName (shortOpt), 0);

		if (optSpec == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Unknown short option: -%c", elektraKeyBaseName (shortOpt)[0]);
			elektraKeyDel (shortOpt);
			return false;
		}

		const char * hasArg = keyGetMetaString (optSpec, "hasarg");
		const char * kind = keyGetMetaString (optSpec, "kind");
		const char * flagValue = keyGetMetaString (optSpec, "flagvalue");

		bool repeated = elektraStrCmp (kind, "array") == 0;

		ElektraKey * option = elektraKeysetLookupByName (options, elektraKeyName (shortOpt), 0);
		if (option == NULL)
		{
			option = elektraKeyNew (elektraKeyName (shortOpt), ELEKTRA_KEY_META, "key", keyGetMetaString (optSpec, "key"), ELEKTRA_KEY_END);
			elektraKeysetAppendKey (options, option);
		}
		else if (!repeated)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "This option cannot be repeated: -%c", elektraKeyBaseName (shortOpt)[0]);
			elektraKeyDel (shortOpt);
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
										elektraKeyBaseName (shortOpt)[0]);
					elektraKeyDel (shortOpt);
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
		elektraKeyDel (shortOpt);

		elektraKeySetMeta (option, "short", "1");
		elektraKeysetAppendKey (options, option);

		if (last)
		{
			break;
		}
	}

	return true;
}

bool parseLongOption (ElektraKey * command, ElektraKeyset * optionsSpec, ElektraKeyset * options, int argc, const char ** argv, int * index, ElektraKey * errorKey)
{
	int i = *index;
	ElektraKey * longOpt = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END);
	elektraKeyAddBaseName (longOpt, "long");

	char * opt = elektraStrDup (&argv[i][2]);
	char * eq = strchr (opt, '=');
	size_t argStart = 0;
	if (eq != NULL)
	{
		// mark end of option
		*eq = '\0';
		argStart = eq - opt + 3;
	}

	elektraKeyAddBaseName (longOpt, opt);
	elektraFree (opt);

	// lookup spec
	ElektraKey * optSpec = elektraKeysetLookupByName (optionsSpec, elektraKeyName (longOpt), 0);

	if (optSpec == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Unknown long option: --%s", elektraKeyBaseName (longOpt));
		elektraKeyDel (longOpt);
		return false;
	}

	const char * hasArg = keyGetMetaString (optSpec, "hasarg");
	const char * kind = keyGetMetaString (optSpec, "kind");
	const char * flagValue = keyGetMetaString (optSpec, "flagvalue");

	bool repeated = elektraStrCmp (kind, "array") == 0;

	ElektraKey * option = elektraKeysetLookupByName (options, elektraKeyName (longOpt), 0);
	if (option == NULL)
	{
		option = elektraKeyNew (elektraKeyName (longOpt), ELEKTRA_KEY_META, "key", keyGetMetaString (optSpec, "key"), ELEKTRA_KEY_END);
		elektraKeysetAppendKey (options, option);
	}
	else if (!repeated)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "This option cannot be repeated: --%s", elektraKeyBaseName (longOpt));
		elektraKeyDel (longOpt);
		return false;
	}
	else if (keyGetMetaString (option, "short") != NULL)
	{
		elektraKeyDel (longOpt);
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
									elektraKeyBaseName (longOpt));
				elektraKeyDel (longOpt);
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
								elektraKeyBaseName (longOpt));
			elektraKeyDel (longOpt);
			return false;
		}

		// use flag value
		setOption (option, flagValue, repeated);
	}
	elektraKeyDel (longOpt);

	return true;
}

void setOption (ElektraKey * option, const char * value, bool repeated)
{
	if (repeated)
	{
		elektraMetaArrayAdd (option, "values", value);
	}
	else
	{
		elektraKeySetString (option, value);
	}
}

/**
 * Writes the options from parseArgs into keys in the proc namespace
 */
int writeOptions (ElektraKey * command, ElektraKey * commandKey, ElektraKey * commandArgs, bool writeArgs, bool * argsWritten, ElektraKeyset * options,
		  struct Specification * spec, ElektraKeyset * ks, const char * progname, const char ** envp, ElektraKey * parentKey)
{
	// Check if help message should be generated
	ElektraKey * helpKey = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END);
	elektraKeyAddName (helpKey, "/long/help");

	// Generate help message
	if (elektraKeysetLookup (options, helpKey, ELEKTRA_KDB_O_DEL) != NULL)
	{
		char * lastSlash = strrchr (progname, '/');
		if (lastSlash != NULL)
		{
			progname = lastSlash + 1;
		}

		char * usage = generateUsageLine (progname, elektraKeysetLookup (spec->commands, command, 0), commandArgs);
		char * optionsText = generateOptionsList (spec->keys, command);
		char * commandsText = generateCommandsList (spec->keys, commandKey);
		char * argsText = generateArgsList (spec->keys, command);
		char * envsText = generateEnvsList (spec->keys);

		elektraKeySetMeta (parentKey, "internal/libopts/help/usage", usage);
		elektraKeySetMeta (parentKey, "internal/libopts/help/options", optionsText);
		elektraKeySetMeta (parentKey, "internal/libopts/help/commands", commandsText);
		elektraKeySetMeta (parentKey, "internal/libopts/help/args", argsText);
		elektraKeySetMeta (parentKey, "internal/libopts/help/envs", envsText);

		elektraFree (usage);
		elektraFree (optionsText);
		elektraFree (commandsText);
		elektraFree (argsText);
		elektraFree (envsText);
		return 1;
	}
	else // Don't generate help message
	{
		ElektraKeyset * envValues = parseEnvp (envp);

		ElektraKey * argsParent = elektraKeyNew (elektraKeyName (command), ELEKTRA_KEY_END);
		elektraKeyAddBaseName (argsParent, "args");
		ElektraKeyset * args = elektraArrayGet (argsParent, options);
		elektraKeyDel (argsParent);

		ElektraKey * keyWithOpt;
		elektraKeysetRewind (spec->keys);
		while ((keyWithOpt = elektraKeysetNext (spec->keys)) != NULL)
		{
			if (spec->useSubcommands)
			{
				ElektraKey * checkKey = elektraKeyDup (keyWithOpt, ELEKTRA_KEY_CP_ALL);
				if (strcmp (elektraKeyBaseName (keyWithOpt), "#") == 0)
				{
					elektraKeySetBaseName (checkKey, NULL); // remove #
				}

				int result = elektraKeyIsDirectlyBelow (commandKey, checkKey);
				elektraKeyDel (checkKey);

				if (result != 1)
				{
					continue;
				}
			}

			if (elektraKeyGetMeta (keyWithOpt, "command") != NULL)
			{
				ElektraKey * procKey = elektraKeyNew ("proc:/", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
				elektraKeyAddName (procKey, strchr (elektraKeyName (keyWithOpt), '/'));
				elektraKeysetAppendKey (ks, procKey);
			}

			int result = writeOptionValues (ks, keyWithOpt, options, parentKey);
			if (result < 0)
			{
				elektraKeysetDel (envValues);
				elektraKeysetDel (args);
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
					elektraKeysetDel (envValues);
					elektraKeysetDel (args);
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
				elektraKeysetDel (envValues);
				elektraKeysetDel (args);
				return -1;
			}
		}

		elektraKeysetDel (envValues);
		elektraKeysetDel (args);

		return 0;
	}
}

ElektraKeyset * ksMetaGetSingleOrArray (ElektraKey * key, const char * metaName)
{
	const ElektraKey * k = elektraKeyGetMeta (key, metaName);
	if (k == NULL)
	{
		return NULL;
	}

	const char * value = elektraKeyString (k);
	if (value[0] != '#')
	{
		// add dummy key to mimic elektraMetaArrayToKS
		return elektraKeysetNew (2, elektraKeyNew ("meta:/#", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);
	}

	ElektraKey * testKey = elektraKeyDup (k, ELEKTRA_KEY_CP_NAME);
	elektraKeyAddBaseName (testKey, elektraKeyString (k));

	const ElektraKey * test = elektraKeyGetMeta (key, elektraKeyName (testKey));
	elektraKeyDel (testKey);

	if (test == NULL)
	{
		// add dummy key to mimic elektraMetaArrayToKS
		return elektraKeysetNew (2, elektraKeyNew ("meta:/#", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);
	}

	return elektraMetaArrayToKS (key, metaName);
}

/**
 * Generate usage line for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateUsageLine (const char * progname, ElektraKey * command, const ElektraKey * commandArgs)
{
	size_t commandStringSize = elektraKeyGetUnescapedNameSize (commandArgs) - 2;
	char * commandString = elektraMalloc (commandStringSize);
	memcpy (commandString, ((const char *) elektraKeyUnescapedName (commandArgs)) + 2, commandStringSize);

	for (char * p = commandString; p < commandString + commandStringSize; ++p)
	{
		*p = *p == '\0' ? ' ' : *p;
	}
	commandString[commandStringSize - 1] = '\0';

	ElektraKeyset * args = elektraMetaArrayToKS (command, "args");

	char * indexedArgs;

	if (elektraKeysetGetSize (args) <= 0)
	{
		indexedArgs = elektraStrDup ("");
	}
	else
	{
		size_t argsTotalSize = 0;
		for (elektraCursor i = 1; i < elektraKeysetGetSize (args); ++i) // start at one to skip size
		{
			argsTotalSize += strlen (elektraKeyString (elektraKeysetAtCursor (args, i))) + 3; // + 3 for space and []
		}

		indexedArgs = elektraMalloc (argsTotalSize + 1);
		char * pos = indexedArgs;
		size_t size = argsTotalSize + 1;
		for (elektraCursor i = 1; i < elektraKeysetGetSize (args); i++) // start at one to skip size
		{
			*pos++ = '<';
			pos = memccpy (pos, elektraKeyString (elektraKeysetAtCursor (args, i)), '\0', size);
			*(pos - 1) = '>';
			*pos++ = ' ';
		}
		*(pos - 1) = '\0';
	}

	bool hasSubCommands = elektraKeyGetMeta (command, "hassubcommands") != NULL;
	const char * remainingArgs = keyGetMetaString (command, "remainingargs");

	char * usage;
	if (hasSubCommands)
	{
		if (elektraKeysetGetSize (args) > 0)
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
		if (elektraKeysetGetSize (args) > 0)
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

	elektraKeysetDel (args);
	elektraFree (indexedArgs);
	elektraFree (commandString);
	return usage;
}


/**
 * Generate options list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateOptionsList (ElektraKeyset * keysWithOpts, ElektraKey * command)
{
	elektraCursor cursor = elektraKeysetGetCursor (keysWithOpts);

	char * optionsList = elektraStrDup ("");

	ElektraKey * cur = NULL;
	elektraKeysetRewind (keysWithOpts);
	while ((cur = elektraKeysetNext (keysWithOpts)) != NULL)
	{
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

	elektraKeysetSetCursor (keysWithOpts, cursor);
	return optionsList;
}

/**
 * Generate commands list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateCommandsList (ElektraKeyset * keysWithOpts, ElektraKey * commandKey)
{
	elektraCursor cursor = elektraKeysetGetCursor (keysWithOpts);

	char * commandsList = elektraStrDup ("");

	ElektraKey * cur = NULL;
	elektraKeysetRewind (keysWithOpts);
	while ((cur = elektraKeysetNext (keysWithOpts)) != NULL)
	{
		ElektraKey * checkKey = elektraKeyDup (cur, ELEKTRA_KEY_CP_NAME);
		if (strcmp (elektraKeyBaseName (cur), "#") == 0)
		{
			elektraKeySetBaseName (checkKey, NULL); // remove #
		}

		int result = elektraKeyIsDirectlyBelow (commandKey, checkKey);
		elektraKeyDel (checkKey);

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

	elektraKeysetSetCursor (keysWithOpts, cursor);
	return commandsList;
}

/**
 * Generate args (parameters) list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateArgsList (ElektraKeyset * keysWithOpts, ElektraKey * command)
{
	elektraCursor cursor = elektraKeysetGetCursor (keysWithOpts);

	char * argsList = elektraStrDup ("");

	ElektraKey * cur = NULL;
	elektraKeysetRewind (keysWithOpts);
	while ((cur = elektraKeysetNext (keysWithOpts)) != NULL)
	{
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

	elektraKeysetSetCursor (keysWithOpts, cursor);
	return argsList;
}

/**
 * Generate env-var list for help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateEnvsList (ElektraKeyset * keysWithOpts)
{
	elektraCursor cursor = elektraKeysetGetCursor (keysWithOpts);

	char * envsList = elektraStrDup ("");

	ElektraKey * cur = NULL;
	elektraKeysetRewind (keysWithOpts);
	while ((cur = elektraKeysetNext (keysWithOpts)) != NULL)
	{
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

	elektraKeysetSetCursor (keysWithOpts, cursor);
	return envsList;
}

/**
 * Determines whether the option or argument belongs to the given command.
 * @param command The Key of the command.
 * @param optionOrArg The key of the option/arg.
 * @return True, if it belongs, false otherwise.
 */
bool optionOrArgBelongsToCommand (const ElektraKey * command, const ElektraKey * optionOrArg)
{
	const ElektraKey * commandKey = elektraKeyGetMeta (optionOrArg, META_COMMAND_KEY);
	const char * commandKeyString = commandKey != NULL ? elektraKeyString (commandKey) : NULL;
	return commandKeyString != NULL && strcmp (elektraKeyName (command), commandKeyString) == 0;
}
