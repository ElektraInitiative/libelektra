package org.libelektra.exception;

import org.libelektra.Key;

public class SyntacticValidationException extends ValidationException {

	private static String errorCode = "C03100";

	public SyntacticValidationException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
