package org.libelektra.exception;

import org.libelektra.Key;

public class SemanticValidationException extends ValidationException
{

	private static final String errorNumber = "C03200";

	public SemanticValidationException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
