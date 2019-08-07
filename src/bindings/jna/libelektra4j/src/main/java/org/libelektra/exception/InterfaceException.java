package org.libelektra.exception;

import org.libelektra.Key;

public class InterfaceException extends LogicalException {

	private static String errorCode = "C01320";

	public InterfaceException(Key k) {
		super(k);
	}

	public static String errorCode() {
		return errorCode;
	}
}
