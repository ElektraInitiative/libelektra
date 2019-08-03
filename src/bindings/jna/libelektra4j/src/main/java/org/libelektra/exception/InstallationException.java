package org.libelektra.exception;

import org.libelektra.Key;

public class InstallationException extends PermanentException {

    private static String errorCode = "C01200";

    public InstallationException(Key k) {
        super(k);
    }

    public static String errorCode() {
        return errorCode;
    }
}
