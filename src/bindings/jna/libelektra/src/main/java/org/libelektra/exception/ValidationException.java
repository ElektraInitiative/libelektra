package org.libelektra.exception;

import org.libelektra.Key;

public abstract class ValidationException extends KDBException
{
	private static final long serialVersionUID = 1L;
	private static final String ERROR_NUMBER = "C03000";

	public ValidationException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return ERROR_NUMBER;
	}
}
