package org.libelektra.exception;

import org.libelektra.Key;

public class ConflictingStateException extends KDBException {

	private static String errorCode = "C02000";

	public ConflictingStateException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
