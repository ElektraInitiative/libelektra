package org.libelektra.exception;

import org.libelektra.Key;

public class PluginMisbehaviorException extends LogicalException
{

	private static final String errorNumber = "C01330";

	public PluginMisbehaviorException (Key k)
	{
		super (k);
	}

	public static String errorNumber ()
	{
		return errorNumber;
	}
}
