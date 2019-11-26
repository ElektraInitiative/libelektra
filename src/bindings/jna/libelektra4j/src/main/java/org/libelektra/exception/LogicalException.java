package org.libelektra.exception;

import org.libelektra.Key;

public abstract class LogicalException extends PermanentException
{

	private static final String errorNumber = "C01300";

	public LogicalException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
