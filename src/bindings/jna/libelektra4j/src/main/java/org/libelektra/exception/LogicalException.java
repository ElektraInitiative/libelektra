package org.libelektra.exception;

import org.libelektra.Key;

public abstract class LogicalException extends PermanentException {

	private static String errorCode = "C01300";

	public LogicalException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
