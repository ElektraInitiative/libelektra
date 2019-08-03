package org.libelektra.exception;

import org.libelektra.Key;

public class OutOfMemoryException extends ResourceException {

    private static String errorCode = "C01110";

    public OutOfMemoryException(Key k) {
        super(k);
    }

    public static String errorCode() {
        return errorCode;
    }
}
