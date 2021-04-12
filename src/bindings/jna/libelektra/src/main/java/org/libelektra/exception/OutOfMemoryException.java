package org.libelektra.exception;

import org.libelektra.Key;

public class OutOfMemoryException extends ResourceException
{
	private static final long serialVersionUID = 1L;
	private static final String ERROR_NUMBER = "C01110";

	public OutOfMemoryException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return ERROR_NUMBER;
	}
}
