package org.libelektra;

import static org.libelektra.ValidationUtil.argNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nonnull;
import org.libelektra.exception.ConflictingStateException;
import org.libelektra.exception.InstallationException;
import org.libelektra.exception.InterfaceException;
import org.libelektra.exception.InternalException;
import org.libelektra.exception.KeyReleasedException;
import org.libelektra.exception.OutOfMemoryException;
import org.libelektra.exception.PluginMisbehaviorException;
import org.libelektra.exception.ResourceException;
import org.libelektra.exception.SemanticValidationException;
import org.libelektra.exception.SyntacticValidationException;

/**
 * Wraps Elektra errors into the corresponding Java exceptions
 */
public abstract class KDBException extends Exception
{

	protected static final String META_KEY_NOT_FOUND_VALUE = "(unknown)";
	protected static final String MSG_UNKNOWN_ERRROR_NUMBER =
		"Sorry, could not map error number '%s'. Please report this incident at https://issues.libelektra.org/";
	protected static final String MSG_MODULE_ERRROR_NUMBER = "Sorry, module %s issued error %s:";
	protected static final String MSG_CONFIGFILE = "Configfile: ";
	protected static final String MSG_MOUNTPOINT = "Mountpoint: ";
	protected static final String MSG_DEBUGINFO = "At: %s:%s";

	private static final long serialVersionUID = 1L;

	private final transient Key errorKey;
	private final transient List<WarningEntry> warnings;

	/**
	 * Extracts warnings and error information and maps it to an appropriate
	 * exception
	 *
	 * @param errorKey Key containing {@code error/*} and {@code warnings/*} meta
	 *                 keys
	 * @return {@link KDBException} corresponding to the error information
	 * @throws KeyReleasedException     if this {@code errorKey} has already been
	 *                                  released
	 * @throws IllegalArgumentException if {@code errorKey} is {@code null}
	 */
	@Nonnull public static KDBException getMappedException (Key errorKey)
	{
		argNotNull (errorKey, "Key 'errorKey'");
		String errorNumber = errorKey.getMeta ("error/number").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);

		if (errorNumber.equals (ResourceException.ERROR_NUMBER))
		{
			return new ResourceException (errorKey);
		}
		if (errorNumber.equals (OutOfMemoryException.ERROR_NUMBER))
		{
			return new OutOfMemoryException (errorKey);
		}
		if (errorNumber.equals (InternalException.ERROR_NUMBER))
		{
			return new InternalException (errorKey);
		}
		if (errorNumber.equals (InterfaceException.ERROR_NUMBER))
		{
			return new InterfaceException (errorKey);
		}
		if (errorNumber.equals (InstallationException.ERROR_NUMBER))
		{
			return new InstallationException (errorKey);
		}
		if (errorNumber.equals (PluginMisbehaviorException.ERROR_NUMBER))
		{
			return new PluginMisbehaviorException (errorKey);
		}
		if (errorNumber.equals (ConflictingStateException.ERROR_NUMBER))
		{
			return new ConflictingStateException (errorKey);
		}
		if (errorNumber.equals (SyntacticValidationException.ERROR_NUMBER))
		{
			return new SyntacticValidationException (errorKey);
		}
		if (errorNumber.equals (SemanticValidationException.ERROR_NUMBER))
		{
			return new SemanticValidationException (errorKey);
		}

		errorKey.setMeta ("error/reason", String.format (MSG_UNKNOWN_ERRROR_NUMBER, errorNumber));
		return new InternalException (errorKey);
	}

	/**
	 * @param errorKey Key containing {@code error/*} and {@code warnings/*} meta
	 *                 keys
	 */
	protected KDBException (Key errorKey)
	{
		this.errorKey = errorKey;
		warnings = new ArrayList<> ();

		Optional<String> oWarningsKeyValue = errorKey.getMeta ("warnings").map (Key::getString);
		if (oWarningsKeyValue.isPresent ())
		{
			String lastArrayIndex = oWarningsKeyValue.get ();
			int arraySize = Integer.parseInt (lastArrayIndex.replaceAll ("^#_*", ""));
			for (int i = 0; i <= arraySize; i++)
			{
				warnings.add (new WarningEntry (errorKey, i));
			}
		}
	}

	/**
	 * @return Key containing {@code error/*} and {@code warnings/*} meta keys
	 *         backing this exception
	 */
	@Nonnull public Key getErrorKey ()
	{
		return errorKey;
	}

	/**
	 * @return Elektra error number read from the error key backing this exception
	 */
	@Nonnull public String getErrorNumber ()
	{
		return errorKey.getMeta ("error/number").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
	}

	/**
	 * @return The affected configuration file of the error or if not available
	 *         returns the error key name
	 */
	@Nonnull public String getConfigFile ()
	{
		return errorKey.getMeta ("error/configfile").map (Key::getString).filter (s -> !s.isEmpty ()).orElseGet (errorKey::getName);
	}

	/**
	 * @return Mountpoint of the configuration
	 */
	@Nonnull public String getMountpoint ()
	{
		return errorKey.getMeta ("error/mountpoint").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
	}

	/**
	 * @return Elektra specific debug information in the form of "At: file:line"
	 */
	@Nonnull public String getDebugInformation ()
	{
		return String.format (MSG_DEBUGINFO, errorKey.getMeta ("error/file").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE),
				      errorKey.getMeta ("error/line").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE));
	}

	/**
	 * @return Module which issued the error
	 */
	@Nonnull public String getModule ()
	{
		return errorKey.getMeta ("error/module").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
	}

	/**
	 * @return Error reason read from the error key backing this exception
	 */
	@Nonnull public String getReason ()
	{
		return errorKey.getMeta ("error/reason").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
	}

	/**
	 * @return The complete error information in a String with config file,
	 *         mount point and debug information as it would be printed in the
	 *         terminal
	 */
	@Override public String getMessage ()
	{
		StringBuilder builder = new StringBuilder ();
		builder.append (String.format (MSG_MODULE_ERRROR_NUMBER, getModule (), getErrorNumber ())).append ("\n");
		builder.append (getReason ()).append ("\n");
		builder.append (MSG_CONFIGFILE).append (getConfigFile ()).append ("\n");
		errorKey.getMeta ("error/mountpoint")
			.ifPresent (s -> builder.append (MSG_MOUNTPOINT).append (getMountpoint ()).append ("\n"));
		errorKey.getMeta ("error/file").ifPresent (s -> builder.append (getDebugInformation ()).append ("\n"));
		return builder.toString ();
	}

	/**
	 * If an error occurred it may also has important warnings which caused the
	 * error
	 *
	 * @return True if additional warnings were emitted, false otherwise
	 */
	public boolean hasWarnings ()
	{
		return !warnings.isEmpty ();
	}

	/**
	 * @return Additional warnings emitted with the error
	 */
	@Nonnull public List<WarningEntry> getWarnings ()
	{
		return warnings;
	}

	/**
	 * Warning extracted from an error key
	 */
	public static class WarningEntry
	{

		private final String warningNumber;
		private final String reason;
		private final String mountpoint;
		private final String configFile;
		private final String debugInformation;
		private final String module;

		/**
		 * Extracts warning information from the specified {@code key}
		 *
		 * @param key     Key to ectract {@code warnings/*} meta keys from
		 * @param current Warning entry to parse, e.g.: {@code (key, 0)} will search for
		 *                entries with {@code warnings/#0}
		 */
		public WarningEntry (Key key, int current)
		{
			final String warningIndex = Integer.toString (current);
			char[] underscores = new char[warningIndex.length () - 1];
			Arrays.fill (underscores, '_');
			final String warningKeyName = "warnings/#" + new String (underscores) + warningIndex;
			warningNumber = key.getMeta (warningKeyName + "/number").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
			reason = key.getMeta (warningKeyName + "/reason").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
			module = key.getMeta (warningKeyName + "/module").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
			debugInformation = String.format (
				MSG_DEBUGINFO,
				key.getMeta (warningKeyName + "/file").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE),
				key.getMeta (warningKeyName + "/line").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE));
			mountpoint = key.getMeta (warningKeyName + "/mountpoint").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
			configFile = key.getMeta (warningKeyName + "/configfile").map (Key::getString).orElse (META_KEY_NOT_FOUND_VALUE);
		}

		/**
		 * @return Warning number issued by Elektra
		 */
		@Nonnull public String getWarningNumber ()
		{
			return warningNumber;
		}

		/**
		 * @return Warning reason text
		 */
		@Nonnull public String getReason ()
		{
			return reason;
		}

		/**
		 * @return Warning reason mount point
		 */
		@Nonnull public String getMountpoint ()
		{
			return mountpoint;
		}

		/**
		 * @return Warning module
		 */
		@Nonnull public String getModule ()
		{
			return module;
		}

		/**
		 * @return Warning reason configuration file
		 */
		@Nonnull public String getConfigFile ()
		{
			return configFile;
		}

		/**
		 * @return Warning debug information in the form of {@code At: file:line}
		 */
		@Nonnull public String getDebugInformation ()
		{
			return debugInformation;
		}
	}
}
