package org.libelektra.exception;

import org.libelektra.Key;

public class SyntacticValidationException extends ValidationException
{

	private static final String errorNumber = "C03100";

	public SyntacticValidationException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
