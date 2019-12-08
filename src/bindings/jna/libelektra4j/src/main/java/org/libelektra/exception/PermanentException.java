package org.libelektra.exception;

import org.libelektra.Key;

public abstract class PermanentException extends KDBException
{

	private static final String errorNumber = "C01000";

	public PermanentException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
