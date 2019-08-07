package org.libelektra.exception;

import org.libelektra.Key;

public class ResourceException extends PermanentException {

	private static String errorCode = "C01100";

	public ResourceException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
