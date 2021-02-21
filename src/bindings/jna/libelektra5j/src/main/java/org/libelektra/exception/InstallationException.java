package org.libelektra.exception;

import java.util.List;
import org.libelektra.Key;
import org.libelektra.exception.model.WarningEntry;

public class InstallationException extends PermanentException
{

	private static final String errorNumber = "C01200";

	public InstallationException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}

	/**
	 * getMessage() returns the thrown Elektra error in the same format as it would be printed in the terminal
	 * @return The complete error information in a String with configfile, moutpoint and debuginformation
	 */
	@Override public String getMessage ()
	{
		StringBuilder builder = new StringBuilder ();
		builder.append (super.getMessage ());
		List<WarningEntry> warnings = getWarnings ();
		if (warnings.size () > 0)
		{
			builder.append ("\n").append ("\n").append ("Warnings:").append ("\n");
		}
		for (int i = 0; i < warnings.size (); i++)
		{
			WarningEntry w = warnings.get (i);
			builder.append ("  ")
				.append (String.format ("#%d: Warning %s from module %s", w.getWarningNumber (), w.getModule ()))
				.append ("\n");
			builder.append ("    ").append (w.getReason ()).append ("\n");
			builder.append ("    ").append ("Configfile: ").append (w.getConfigFile ()).append ("\n");
			builder.append ("    ").append ("Mountpoint: ").append (w.getMountpoint ()).append ("\n");
			builder.append ("    ").append (w.getDebugInformation ()).append ("\n");
		}
		return builder.toString ();
	}
}
