package org.libelektra.exception;

import org.libelektra.Key;

public abstract class LogicalException extends PermanentException
{
	private static final long serialVersionUID = 1L;
	public static final String ERROR_NUMBER = "C01300";

	public LogicalException (Key k)
	{
		super (k);
	}
}
