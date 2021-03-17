package org.libelektra.exception;

import java.util.ArrayList;
import java.util.List;
import org.libelektra.Key;
import org.libelektra.exception.model.WarningEntry;


/**
 * This exception wraps Elektra errors into the corresponding Java Exceptions
 */
public abstract class KDBException extends Exception
{
	private static final long serialVersionUID = 1L;

	private final transient Key errorKey;
	private List<WarningEntry> warnings;

	/**
	 * KDBException which holds the errorKey
	 * @param k The errorKey
	 */
	public KDBException (final Key k)
	{
		errorKey = k;
		Key warningsKey = k.getMeta ("warnings");
		warnings = new ArrayList<> ();
		if (warningsKey.isNull ())
		{
			return;
		}
		final String lastArrayIndex = warningsKey.getString ();
		final int arraySize = Integer.valueOf (lastArrayIndex.replaceAll ("^#_*", ""));
		for (int i = 0; i <= arraySize; i++)
		{
			warnings.add (new WarningEntry (k, i));
		}
	}

	/**
	 * Gets the errorKey from Elektra
	 *
	 * @return ErrorKey from Elektra
	 */
	public Key getErrorKey ()
	{
		return errorKey;
	}

	/**
	 * Gets the errorNumber from Elektra
	 *
	 * @return ErrorNumber from Elektra
	 */
	public String getErrorNumber ()
	{
		return errorKey.getMeta ("error/number").getString ();
	}

	/**
	 * Returns the affected configuration file of the error.
	 * It empty returns the parents Key name
	 *
	 * @return either the configuration file or if empty the parent key name
	 */
	public String getConfigFile ()
	{
		Key configKey = errorKey.getMeta ("error/configfile");
		if (!configKey.isNull () && !configKey.getString ().isEmpty ())
		{
			return configKey.getString ();
		}
		return errorKey.getName ();
	}

	/**
	 * Returns the mountpoint of the configuration
	 *
	 * @return the mountpoint of the configuration
	 */
	public String getMountpoint ()
	{
		return errorKey.getMeta ("error/mountpoint").getString ();
	}

	/**
	 * Prints Elektra specific debug information in the form of "At: file:line"
	 *
	 * @return Elektra specific debug information in the form of "At: file:line"
	 */
	public String getDebugInformation ()
	{
		return String.format ("At: %s:%s", errorKey.getMeta ("error/file").getString (),
				      errorKey.getMeta ("error/line").getString ());
	}

	/**
	 * Returns the module which has thrown the error
	 *
	 * @return the module which has thrown the error
	 */
	public String getModule ()
	{
		return errorKey.getMeta ("error/module").getString ();
	}

	/**
	 * @see this.getMessage()
	 */
	@Override public String getLocalizedMessage ()
	{
		return getMessage ();
	}

	/**
	 * Returns the error reason which is written to the `error/reason` metakey of the errorkey
	 * @return The reason for the error
	 */
	public String getReason ()
	{
		return errorKey.getMeta ("error/reason").getString ();
	}

	/**
	 * getMessage() returns the thrown Elektra error in the same format as it would be printed in the terminal
	 * @return The complete error information in a String with configfile, moutpoint and debuginformation
	 */
	@Override public String getMessage ()
	{
		StringBuilder builder = new StringBuilder ();
		builder.append (String.format ("Sorry, module %s issued error %s:", getModule (), getErrorNumber ())).append ("\n");
		builder.append (getReason ()).append ("\n");
		builder.append ("Configfile: ").append (getConfigFile ()).append ("\n");
		if (!errorKey.getMeta ("error/mountpoint").isNull ())
		{
			builder.append ("Mountpoint: ").append (getMountpoint ()).append ("\n");
		}
		if (!errorKey.getMeta ("error/file").isNull ())
		{
			builder.append (getDebugInformation ()).append ("\n");
		}
		return builder.toString ();
	}

	/**
	 * If an error occurred it may also has important warnings which caused the error.
	 * This method checks if they are available
	 *
	 * @return true if additional warnings were emitted
	 */
	public boolean hasWarnings ()
	{
		return !warnings.isEmpty ();
	}

	/**
	 * Returns the warnings list
	 *
	 * @return the warnings list
	 */
	public List<WarningEntry> getWarnings ()
	{
		return warnings;
	}
}
