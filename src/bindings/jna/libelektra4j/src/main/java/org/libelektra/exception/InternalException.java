package org.libelektra.exception;

import org.libelektra.Key;

public class InternalException extends LogicalException
{

	private static final String errorNumber = "C01310";

	public InternalException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
