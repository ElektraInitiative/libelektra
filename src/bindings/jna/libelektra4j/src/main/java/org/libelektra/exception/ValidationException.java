package org.libelektra.exception;

import org.libelektra.Key;

public abstract class ValidationException extends KDBException {

    private static String errorCode = "C03000";

    public ValidationException(Key k) {
        super(k);
    }

    public static String errorCode() {
        return errorCode;
    }
}
