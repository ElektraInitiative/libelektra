package org.libelektra.exception;

import org.libelektra.KDBException;
import org.libelektra.Key;

public class ConflictingStateException extends KDBException
{
	private static final long serialVersionUID = 1L;
	public static final String ERROR_NUMBER = "C02000";

	public ConflictingStateException (Key k)
	{
		super (k);
	}
}
