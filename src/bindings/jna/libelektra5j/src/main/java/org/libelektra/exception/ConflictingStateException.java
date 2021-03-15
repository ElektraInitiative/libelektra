package org.libelektra.exception;

import org.libelektra.Key;

public class ConflictingStateException extends KDBException
{
	private static final long serialVersionUID = 1L;
	private static final String errorNumber = "C02000";

	public ConflictingStateException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
