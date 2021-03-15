package org.libelektra.exception;

import org.libelektra.Key;

public abstract class PermanentException extends KDBException
{
	private static final long serialVersionUID = 1L;
	private static final String ERROR_NUMBER = "C01000";

	public PermanentException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return ERROR_NUMBER;
	}
}
