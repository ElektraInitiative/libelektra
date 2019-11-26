package org.libelektra.exception;

import org.libelektra.Key;

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
}
