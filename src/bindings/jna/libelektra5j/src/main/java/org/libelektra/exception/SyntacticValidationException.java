package org.libelektra.exception;

import org.libelektra.Key;

public class SyntacticValidationException extends ValidationException
{
	private static final long serialVersionUID = 1L;
	private static final String ERROR_NUMBER = "C03100";

	public SyntacticValidationException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return ERROR_NUMBER;
	}
}
