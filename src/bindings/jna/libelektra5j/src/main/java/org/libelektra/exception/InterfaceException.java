package org.libelektra.exception;

import org.libelektra.Key;

public class InterfaceException extends LogicalException
{

	private static final String errorNumber = "C01320";

	public InterfaceException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
