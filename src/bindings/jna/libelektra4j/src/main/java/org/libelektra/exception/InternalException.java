package org.libelektra.exception;

import org.libelektra.Key;

public class InternalException extends LogicalException {

	private static String errorCode = "C01310";

	public InternalException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
