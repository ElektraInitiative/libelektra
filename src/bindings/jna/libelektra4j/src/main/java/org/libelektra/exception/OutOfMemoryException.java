package org.libelektra.exception;

import org.libelektra.Key;

public class OutOfMemoryException extends ResourceException
{

	private static final String errorNumber = "C01110";

	public OutOfMemoryException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
