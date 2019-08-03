package org.libelektra.exception;

import org.libelektra.Key;

public class PluginMisbehaviorException extends LogicalException {

    private static String errorCode = "C01330";

    public PluginMisbehaviorException(Key k) {
        super(k);
    }

    public static String errorCode() {
        return errorCode;
    }
}
