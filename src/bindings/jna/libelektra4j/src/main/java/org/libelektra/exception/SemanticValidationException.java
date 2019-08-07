package org.libelektra.exception;

import org.libelektra.Key;

public class SemanticValidationException extends ValidationException {

	private static String errorCode = "C03200";

	public SemanticValidationException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
