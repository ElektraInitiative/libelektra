package org.libelektra.exception.model;

import java.util.Arrays;
import org.libelektra.Key;

public class WarningEntry
{

	private final String warningNumber;
	private final String reason;
	private final String mountpoint;
	private final String configFile;
	private final String debugInformation;
	private final String module;

	/**
	 * Extracts warning information from the errorKey
	 * @param key the errorkey containing the warnings/* metakeys
	 * @param current The current entry you want to parse, e.g., (key, 0) will search for entries with "warnings/#0"
	 */
	public WarningEntry (Key key, int current)
	{
		final String warningIndex = Integer.toString (current);
		char[] underscores = new char[warningIndex.length () - 1];
		Arrays.fill (underscores, '_');
		final String warningKeyName = "warnings/#" + new String (underscores) + warningIndex;
		warningNumber = key.getMeta (warningKeyName + "/number").getString ();
		reason = key.getMeta (warningKeyName + "/reason").getString ();
		module = key.getMeta (warningKeyName + "/module").getString ();
		debugInformation = String.format ("At: %s:%s", key.getMeta (warningKeyName + "/file").getString (),
						  key.getMeta (warningKeyName + "/line").getString ());
		mountpoint = key.getMeta (warningKeyName + "/mountpoint").getString ();
		configFile = key.getMeta (warningKeyName + "/configfile").getString ();
	}

	/**
	 * Returns the warningNumber from Elektra
	 *
	 * @return the warningNumber from Elektra
	 */
	public String getWarningNumber ()
	{
		return warningNumber;
	}

	/**
	 * Returns the reason text from the warning
	 *
	 * @return the reason text from the warning
	 */
	public String getReason ()
	{
		return reason;
	}

	/**
	 * Returns the mountpoint from the warning
	 *
	 * @return the mountpoint from the warning
	 */
	public String getMountpoint ()
	{
		return mountpoint;
	}

	/**
	 * Returns the module from the warning
	 *
	 * @return the module from the warning
	 */
	public String getModule ()
	{
		return module;
	}

	/**
	 * Returns the configuration file from the warning
	 *
	 * @return the configuration file from the warning
	 */
	public String getConfigFile ()
	{
		return configFile;
	}

	/**
	 * Returns the debug information from the warning in the form of "At: file:line"
	 *
	 * @return the debug information from the warning in the form of "At: file:line"
	 */
	public String getDebugInformation ()
	{
		return debugInformation;
	}
}
