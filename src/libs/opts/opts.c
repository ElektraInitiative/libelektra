/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbopts.h>

#include <stdlib.h>
#include <string.h>

#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbmeta.h>

#include <kdberrors.h>

#ifdef _WIN32
static const char SEP_ENV_VALUE = ';';
#else
static const char SEP_ENV_VALUE = ':';
#endif

static inline const char * keyGetMetaString (const Key * key, const char * meta)
{
	const Key * mk = keyGetMeta (key, meta);
	const char * value = mk == NULL ? NULL : keyString (mk);
	return value != NULL && value[0] == '\0' ? NULL : value;
}

static bool addProcKey (KeySet * ks, const Key * key, Key * valueKey);
static KeySet * parseEnvp (const char ** envp);

static KeySet * parseArgs (KeySet * optionsSpec, int argc, const char ** argv, Key * errorKey);
static void setOption (Key * option, const char * value, bool repeated);

static Key * splitEnvValue (const Key * envKey);

static KeySet * ksMetaGetSingleOrArray (Key * key, const char * metaName);

static char * generateHelpMessage (KeySet * optionsSpec, const char * progname);

/**
 * This functions parses a specification of program options, together with a list of arguments
 * and environment variables to extract the option values.
 *
 * The options have to be defined in the metadata of keys in the spec namespace. If an option value
 * is found for any of the given keys, a new key with the same path but inside the proc namespace
 * will be inserted into @p ks. This enables a cascading lookup to find these values.
 *
 * If argv contains "-h" or "--help" @p ks will not be changed, instead the value of @p errorKey
 * will be set to a help message describing available options and 1 will be returned. The program
 * name used in this message is taken from `argv[0]`. If it contains a '/' only the part after the
 * last '/' will be used.
 *
 * Because "-h" and "--help" are reserved for the help message, neither can be used for anything else.
 *
 * To define a command line option set the `opt` meta-key to the short option. Only the first
 * character of the given value will be used ('\0' is not allowed). You can also set `opt/long`,
 * to use a long option. Each option can have a short version, a long version or both.
 * Per default an option is expected to have an argument. To change this behaviour set `opt/arg` to
 * either 'none' or 'optional' (the default is 'required'). The behaviour of short options, long
 * options, required and optional arguments is similar to getopt_long(3).
 *
 * A short option cannot be set to 'optional', as the only way to give an 'optional' argument is via
 * the "--opt=value" syntax of long options. If a long option with an 'optional' argument does not
 * have an argument, its value will be set as if `opt/arg` were 'none'. One difference to getopt_long(3)
 * is that options with 'optional' arguments can also have short versions. In this case the short version
 * is treated as if `opt/arg` were set to 'none', i.e. it cannot have an argument.
 *
 * If `opt/arg` is set to 'none' the the corresponding key will have the value "1", if the option is
 * provided. This value can be changed by setting `opt/flagvalue` to something else.
 *
 * A key can also be associated with multiple options. To achieve this, simply follow the instructions
 * above, but replace `opt` with `opt/#XXX` (XXX being the index of the option) in all keynames. Each
 * option can have a different setting for none/required/optional arguments and flagvalue too.
 *
 * In addition to command line options this function also supports environment variables. These are
 * specified with `env` (or `env/#XXX` if multiple are used).
 *
 * Lastly, if the key for which the options are defined, has the basename '#', an option can be repeated.
 * All occurrences will be collected into an array. Environment variables obviously cannot be repeated,
 * instead a behaviour similar that used for PATH is adopted. On Windows the variable will be split at
 * each ';' character. On all other systems ':' is used as a separator.
 *
 * In case multiple versions (short, long, env-var) of an option are found, the order of precedence is:
 * <ul>
 * 	<li> Short options always win. </li>
 * 	<li> Long options are used if no short option is present. </li>
 * 	<li> If neither a long nor short option is found, environment variables are considered. </li>
 * </ul>
 * NOTE: for array-type options (basename '#') the order of precedence is respected as well. Different
 * options of the same type (e.g. '-s' and '-a', or '--add' and '--append') or multiple environment
 * variables are found for the same key, the resulting arrays will be merged. Different options of
 * different types (e.g. '-s' and '--append') will not be merged.
 *
 * NOTE: While environment variable can be used on multiple keys, options (short and long) can only be
 * used for a single key. This is because options could be configured with different behaviour (arg,
 * flagvalue, repeatability) on separate keys. There is not good way to handle this, so it is prohibited.
 * Environment variables meanwhile always behave the same way. The only difference is whether or not they
 * are split into multiple strings. This does not result in any conflicts, so it is possible to use on
 * variable for multiple keys (although there really isn't any reason to do so).
 * If you need to have the values of one option in more than one key, consider fallbacks.
 *
 * @param ks	The KeySet containing the specification for the options.
 * @param argc	The number of strings in argv.
 * @param argv	The arguments to be processed.
 * @param envp	A list of environment variables. This needs to be a null-terminated list of
 * 		strings of the format 'KEY=VALUE'.
 * @param errorKey A key to store an error in, if one occurs.
 *
 * @retval 0	on success
 * @retval -1	on error, the error will be added to @p errorKey
 * @retval 1	if help option was found
 */
int elektraGetOpts (KeySet * ks, int argc, const char ** argv, const char ** envp, Key * errorKey)
{
	KeySet * keysWithOpts = ksNew (0, KS_END);
	KeySet * optionsSpec = ksNew (
		2, keyNew ("/short/h", KEY_META, "hasarg", "none", KEY_META, "kind", "single", KEY_META, "flagvalue", "1", KEY_END),
		keyNew ("/long/help", KEY_META, "hasarg", "none", KEY_META, "kind", "single", KEY_META, "flagvalue", "1", KEY_END), KS_END);

	cursor_t initial = ksGetCursor (ks);

	ksRewind (ks);
	Key * cur;
	while ((cur = ksNext (ks)) != NULL)
	{
		if (keyGetNamespace (cur) != KEY_NS_SPEC)
		{
			continue;
		}

		const char * hasArg = keyGetMetaString (cur, "opt/arg");
		if (hasArg == NULL)
		{
			hasArg = "required";
		}

		const char * kind = "single";
		if (strcmp (keyBaseName (cur), "#") == 0)
		{
			kind = "array";
		}

		const char * flagValue = keyGetMetaString (cur, "opt/flagvalue");
		if (flagValue == NULL)
		{
			flagValue = "1";
		}
		else if (strcmp (hasArg, "none") != 0 && strcmp (hasArg, "optional") != 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_SPEC, errorKey,
					    "The flagvalue metadata can only be used, if the opt/arg metadata is set to 'none' or "
					    "'optional'. (key: %s)",
					    keyName (cur));
			ksDel (optionsSpec);
			ksDel (keysWithOpts);
			return -1;
		}

		Key * key = NULL;

		KeySet * opts = ksMetaGetSingleOrArray (cur, "opt");
		if (opts != NULL)
		{
			ksRewind (opts);
			Key * k;
			while ((k = ksNext (opts)) != NULL)
			{
				const char * shortOpt = keyString (k);
				if (shortOpt != NULL && shortOpt[0] != '\0')
				{
					Key * optSpec = keyNew ("/short", KEY_META, "key", keyName (cur), KEY_META, "hasarg", hasArg,
								KEY_META, "kind", kind, KEY_META, "flagvalue", flagValue, KEY_END);
					keyAddBaseName (optSpec, (char[]){ shortOpt[0], '\0' });

					Key * existing = ksLookupByName (optionsSpec, keyName (optSpec), 0);
					if (existing != NULL)
					{
						ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_SPEC, errorKey,
								    "The option '-%c' has already been used for the key '%s'.", shortOpt[0],
								    keyGetMetaString (existing, "key"));
						keyDel (optSpec);
						keyDel (existing);
						ksDel (optionsSpec);
						ksDel (keysWithOpts);
						return -1;
					}

					ksAppendKey (optionsSpec, optSpec);

					if (key == NULL)
					{
						key = keyNew (keyName (cur), KEY_END);
					}
					elektraMetaArrayAdd (key, "opt", keyName (optSpec));
				}

				Key * k1 = keyDup (k);
				keyAddBaseName (k1, "long");

				const char * longOpt = keyGetMetaString (cur, keyName (k1));
				keyDel (k1);

				if (longOpt != NULL)
				{
					Key * optSpec = keyNew ("/long", KEY_META, "key", keyName (cur), KEY_META, "hasarg", hasArg,
								KEY_META, "kind", kind, KEY_META, "flagvalue", flagValue, KEY_END);
					keyAddBaseName (optSpec, longOpt);

					Key * existing = ksLookupByName (optionsSpec, keyName (optSpec), 0);
					if (existing != NULL)
					{
						ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_SPEC, errorKey,
								    "The option '--%s' has already been used for the key '%s'.", longOpt,
								    keyGetMetaString (existing, "key"));
						keyDel (optSpec);
						keyDel (existing);
						ksDel (optionsSpec);
						ksDel (keysWithOpts);
						return -1;
					}

					ksAppendKey (optionsSpec, optSpec);

					if (key == NULL)
					{
						key = keyNew (keyName (cur), KEY_END);
					}
					elektraMetaArrayAdd (key, "opt", keyName (optSpec));
				}
			}
			ksDel (opts);
		}

		KeySet * envVars = ksMetaGetSingleOrArray (cur, "env");
		if (envVars != NULL)
		{
			ksRewind (envVars);
			Key * k;
			while ((k = ksNext (envVars)) != NULL)
			{
				const char * envVar = keyString (k);
				if (envVar != NULL)
				{
					if (key == NULL)
					{
						key = keyNew (keyName (cur), KEY_END);
					}
					char * envVarName = elektraFormat ("/%s", envVar);
					elektraMetaArrayAdd (key, "env", envVarName);
					elektraFree (envVarName);
				}
			}
			ksDel (envVars);
		}

		if (key != NULL)
		{
			ksAppendKey (keysWithOpts, key);
		}
	}

	KeySet * options = parseArgs (optionsSpec, argc, argv, errorKey);
	ksDel (optionsSpec);

	if (options == NULL)
	{
		ksDel (options);
		ksDel (keysWithOpts);
		return -1;
	}

	Key * helpKey = ksLookupByName (options, "/short/h", 0);
	if (helpKey == NULL)
	{
		helpKey = ksLookupByName (options, "/long/help", 0);
	}

	if (helpKey != NULL)
	{
		const char * progname = argv[0];
		char * lastSlash = strrchr (progname, '/');
		if (lastSlash != NULL)
		{
			progname = lastSlash + 1;
		}

		char * help = generateHelpMessage (optionsSpec, progname);
		keySetString (errorKey, help);
		elektraFree (help);
		ksDel (options);
		ksDel (keysWithOpts);
		return 1;
	}


	KeySet * envValues = parseEnvp (envp);

	ksRewind (keysWithOpts);
	while ((cur = ksNext (keysWithOpts)) != NULL)
	{
		bool valueFound = false;

		KeySet * optMetas = elektraMetaArrayToKS (cur, "opt");
		if (optMetas != NULL)
		{
			ksRewind (optMetas);
			Key * optMeta;
			while ((optMeta = ksNext (optMetas)) != NULL)
			{
				Key * optKey = ksLookupByName (options, keyString (optMeta), 0);
				if (addProcKey (ks, cur, optKey))
				{
					valueFound = true;
					break;
				}
			}
		}
		ksDel (optMetas);

		if (valueFound)
		{
			continue;
		}

		KeySet * envMetas = elektraMetaArrayToKS (cur, "env");
		if (envMetas != NULL)
		{
			ksRewind (envMetas);
			Key * envMeta;
			while ((envMeta = ksNext (envMetas)) != NULL)
			{
				Key * envKey = ksLookupByName (envValues, keyString (envMeta), 0);
				Key * envValueKey = splitEnvValue (envKey);

				bool added = addProcKey (ks, cur, envValueKey);
				keyDel (envValueKey);

				if (added)
				{
					break;
				}
			}
		}
		ksDel (envMetas);
	}

	ksDel (envValues);
	ksDel (options);
	ksDel (keysWithOpts);

	ksSetCursor (ks, initial);

	return 0;
}

Key * splitEnvValue (const Key * envKey)
{
	if (envKey == NULL)
	{
		return NULL;
	}

	Key * valueKey = keyNew (keyName (envKey), KEY_END);

	char * envValue = elektraStrDup (keyString (envKey));

	char * c = strchr (envValue, SEP_ENV_VALUE);
	if (c == NULL)
	{
		keySetString (valueKey, envValue);
	}
	else
	{
		keySetString (valueKey, NULL);

		while (c != NULL)
		{
			*c = '\0';

			elektraMetaArrayAdd (valueKey, "values", envValue);

			envValue = c + 1;
			c = strchr (envValue, SEP_ENV_VALUE);
		}
	}

	elektraFree (envValue);

	return valueKey;
}

/**
 * @retval true if a proc key was added to ks
 * @retval false otherwise
 */
bool addProcKey (KeySet * ks, const Key * key, Key * valueKey)
{
	if (ks == NULL || key == NULL || valueKey == NULL)
	{
		return false;
	}

	Key * procKey = keyNew ("proc", KEY_END);
	keyAddName (procKey, strchr (keyName (key), '/'));

	Key * existing = ksLookup (ks, procKey, 0);
	if (existing != NULL)
	{
		keyDel (procKey);
		procKey = existing;
	}

	KeySet * values = elektraMetaArrayToKS (valueKey, "values");
	if (values == NULL)
	{
		keySetString (procKey, keyString (valueKey));
	}
	else
	{
		ksRewind (values);
		Key * cur;
		char indexBuffer[ELEKTRA_MAX_ARRAY_SIZE];
		int index = 0;
		while ((cur = ksNext (values)) != NULL)
		{
			elektraWriteArrayNumber (indexBuffer, index);

			Key * k = keyDup (procKey);
			keyAddBaseName (k, indexBuffer);
			keySetString (k, keyString (cur));
			ksAppendKey (ks, k);

			index++;
		}
		elektraWriteArrayNumber (indexBuffer, index - 1); // last index written to
		keySetString (procKey, indexBuffer);
	}


	return ksAppendKey (ks, procKey) > 0;
}

KeySet * parseEnvp (const char ** envp)
{
	KeySet * ks = ksNew (0, KS_END);

	const char ** cur = envp;
	while (*cur != NULL)
	{
		const char * eq = strchr (*cur, '=');
		Key * key = keyNew ("/", KEY_VALUE, eq + 1, KEY_END);
		char * name = strndup (*cur, eq - *cur); // elektraStrNDup does not terminate string
		keyAddBaseName (key, name);
		ksAppendKey (ks, key);
		free (name);

		cur++;
	}

	return ks;
}

KeySet * parseArgs (KeySet * optionsSpec, int argc, const char ** argv, Key * errorKey)
{
	KeySet * options = ksNew (0, KS_END);
	for (int i = 1; i < argc; ++i)
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
					break;
				}

				// long option
				Key * longOpt = keyNew ("/long", KEY_END);

				char * opt = elektraStrDup (&cur[2]);
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
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_UNKNOWN_OPTION, errorKey, "Unknown long option: --%s",
							    keyBaseName (longOpt));
					keyDel (longOpt);
					ksDel (options);
					return NULL;
				}

				const char * hasArg = keyGetMetaString (optSpec, "hasarg");
				const char * kind = keyGetMetaString (optSpec, "kind");
				const char * flagValue = keyGetMetaString (optSpec, "flagvalue");

				bool repeated = strcmp (kind, "array") == 0;

				Key * option = ksLookupByName (options, keyName (longOpt), 0);
				if (option == NULL)
				{
					option = keyNew (keyName (longOpt), KEY_META, "key", keyGetMetaString (optSpec, "key"), KEY_END);
					ksAppendKey (options, option);
				}
				else if (!repeated)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_USE, errorKey,
							    "This option cannot be repeated: --%s", keyBaseName (longOpt));
					keyDel (longOpt);
					ksDel (options);
					keyDel (optSpec);
					return NULL;
				}
				else if (keyGetMetaString (option, "short") != NULL)
				{
					keyDel (longOpt);
					keyDel (optSpec);
					// short option found already ignore long version
					continue;
				}
				keyDel (optSpec);

				if (strcmp (hasArg, "required") == 0)
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
							ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_USE, errorKey,
									    "Missing argument for long option: --%s",
									    keyBaseName (longOpt));
							keyDel (longOpt);
							ksDel (options);
							return NULL;
						}
						// use next as arg and skip
						setOption (option, argv[++i], repeated);
					}
				}
				else if (strcmp (hasArg, "optional") == 0)
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
						ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_USE, errorKey,
								    "This option cannot have an argument: --%s", keyBaseName (longOpt));
						keyDel (longOpt);
						ksDel (options);
						return NULL;
					}

					// use flag value
					setOption (option, flagValue, repeated);
				}
				keyDel (longOpt);

				continue;
			}

			for (const char * c = &cur[1]; *c != '\0'; ++c)
			{
				// short option
				Key * shortOpt = keyNew ("/short", KEY_END);
				keyAddBaseName (shortOpt, (char[]){ *c, '\0' });

				Key * optSpec = ksLookupByName (optionsSpec, keyName (shortOpt), 0);

				if (optSpec == NULL)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_UNKNOWN_OPTION, errorKey, "Unknown short option: -%c",
							    keyBaseName (shortOpt)[0]);
					keyDel (shortOpt);
					keyDel (optSpec);
					ksDel (options);
					return NULL;
				}

				const char * hasArg = keyGetMetaString (optSpec, "hasarg");
				const char * kind = keyGetMetaString (optSpec, "kind");
				const char * flagValue = keyGetMetaString (optSpec, "flagvalue");

				bool repeated = strcmp (kind, "array") == 0;

				Key * option = ksLookupByName (options, keyName (shortOpt), 0);
				if (option == NULL)
				{
					option = keyNew (keyName (shortOpt), KEY_META, "key", keyGetMetaString (optSpec, "key"), KEY_END);
					ksAppendKey (options, option);
				}
				else if (!repeated)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_USE, errorKey, "This option cannot be repeated: -%c",
							    keyBaseName (shortOpt)[0]);
					keyDel (shortOpt);
					keyDel (optSpec);
					ksDel (options);
					return NULL;
				}
				keyDel (optSpec);

				bool last = false;
				if (strcmp (hasArg, "required") == 0)
				{
					if (*(c + 1) == '\0')
					{
						if (i >= argc - 1)
						{
							ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_OPTS_ILLEGAL_USE, errorKey,
									    "Missing argument for short option: -%c",
									    keyBaseName (shortOpt)[0]);
							keyDel (shortOpt);
							keyDel (option);
							ksDel (options);
							return NULL;
						}
						// use next as arg and skip
						setOption (option, argv[++i], repeated);
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
		}
	}
	return options;
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
		return ksNew (1, k, KS_END);
	}

	Key * testKey = keyDup (k);
	keyAddBaseName (testKey, keyString (k));

	const Key * test = keyGetMeta (key, keyName (testKey));
	keyDel (testKey);

	if (test == NULL)
	{
		return ksNew (1, k, KS_END);
	}

	return elektraMetaArrayToKS (key, metaName);
}

/**
 * Generate help message from optionsSpec.
 *
 * @return a newly allocated string, must be freed with elektraFree()
 */
char * generateHelpMessage (KeySet * optionsSpec, const char * progname)
{
	cursor_t cursor = ksGetCursor (optionsSpec);

	ksSetCursor (optionsSpec, cursor);
	// TODO
	return elektraFormat ("Usage: %s TODO\n", progname);
}
