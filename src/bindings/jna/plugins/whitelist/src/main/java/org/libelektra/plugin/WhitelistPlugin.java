package org.libelektra.plugin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;
import org.libelektra.exception.SemanticValidationException;

/**
 * Plugin enforcing key values to adhere to a specified whitelist
 */
public class WhitelistPlugin implements Plugin
{

	private static final String PLUGIN_NAME = "Whitelist";
	private static final Pattern META_WHITELISTENTRY_PATTERN = Pattern.compile ("meta:/check/whitelist/.*");
	private static final Pattern META_WHITELISTENTRY_VALID_PATTERN = Pattern.compile ("meta:/check/whitelist/#_*\\d+");

	@Override public KeySet getConfig ()
	{
		return KeySet.create ();
	}

	@Override public int open (KeySet conf, Key errorKey)
	{
		// no initialization necessary
		return STATUS_SUCCESS;
	}

	@Override public int get (KeySet keySet, Key parentKey)
	{
		// if plugin meta data is requested, return additional contract values
		if (parentKey.isBelowOrSame (Key.create (JNI_MODULE_CONTRACT_ROOT)))
		{
			keySet.append (
				Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos", PLUGIN_NAME + " Java plugin, loaded by the JNI plugin"));
			keySet.append (Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos/author", "Michael Tucek <michael@tucek.eu>"));
			keySet.append (Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos/provides", "check"));
			keySet.append (Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos/placements", "presetstorage"));
			keySet.append (Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos/metadata", "check/whitelist/#"));
			keySet.append (Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos/description", "Enforces a whitelist for key values"));
			keySet.append (Key.create (JNI_MODULE_CONTRACT_ROOT + "/infos/status", "preview maintained"));
			return STATUS_SUCCESS;
		}

		// here could go some normalization code (e.g. one could introduce an option to
		// make whitelist values case-insensitive and normalize all read values to lower
		// case) - see type.c for an example on how to do that

		return STATUS_NO_UPDATE;
	}

	@Override public int set (KeySet keySet, Key parentKey)
	{
		// iterate key set and validate each key
		var iter = keySet.iterator ();
		while (iter.hasNext ())
		{
			var key = iter.next ();

			// look whether a whitelist has been defined
			Set<String> whitelist = new HashSet<> ();
			List<Key> invalidWhitelistSpecification = new ArrayList<> ();
			key.rewindMeta ();
			Optional<Key> oCurrentMetaKey;
			while ((oCurrentMetaKey = key.nextMeta ()).isPresent ())
			{
				var metaKey = oCurrentMetaKey.get ();
				if (META_WHITELISTENTRY_PATTERN.matcher (metaKey.getName ()).matches ())
				{
					if (META_WHITELISTENTRY_VALID_PATTERN.matcher (metaKey.getName ()).matches ())
					{
						whitelist.add (metaKey.getString ());
					}
					else
					{
						invalidWhitelistSpecification.add (metaKey);
					}
				}
			}

			if (!invalidWhitelistSpecification.isEmpty ())
			{
				// add semantic validation warnings
				for (int i = 0; i < invalidWhitelistSpecification.size (); i++)
				{
					final String warningIndex = Integer.toString (i);
					char[] underscores = new char[warningIndex.length () - 1];
					Arrays.fill (underscores, '_');
					final String warningKeyName = "warnings/#" + new String (underscores) + warningIndex;
					parentKey.setMeta (warningKeyName + "/number", SemanticValidationException.ERROR_NUMBER);
					parentKey.setMeta (
						warningKeyName + "/reason",
						String.format (
							"Key '%s' is no valid whitelist check specification and is therefore ignored.",
							invalidWhitelistSpecification.get (i).getName ()));
				}
			}

			if (!whitelist.isEmpty () && !whitelist.contains (key.getString ()))
			{
				// add semantic validation error
				parentKey.setMeta ("error/number", SemanticValidationException.ERROR_NUMBER);
				parentKey.setMeta (
					"error/reason",
					String.format (
						"Value of key '%s' with value '%s' does not adhere to whitelist of possible values: %s",
						key.getName (), key.getString (), whitelist.stream ().collect (Collectors.joining (", "))));
				return STATUS_ERROR;
			}
		}

		return STATUS_SUCCESS;
	}

	@Override public int error (KeySet keySet, Key parentKey)
	{
		// no clean-up in case of error necessary
		return STATUS_SUCCESS;
	}

	@Override public int close (Key parentKey)
	{
		// no finalization necessary
		return STATUS_SUCCESS;
	}

	@Override public String getName ()
	{
		return PLUGIN_NAME;
	}
}
