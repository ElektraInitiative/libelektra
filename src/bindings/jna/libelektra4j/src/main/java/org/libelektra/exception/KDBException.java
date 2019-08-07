package org.libelektra.exception;

import org.libelektra.Key;
import org.libelektra.exception.model.WarningEntry;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Custom KDB exception class being used for I/O errors
 */
public abstract class KDBException extends java.io.IOException {

	private static final long serialVersionUID = 1L;

	final transient Key errorKey;
	private Collection<WarningEntry> warnings;

	public KDBException(final Key k) {
		errorKey = k;
		Key warningsKey = k.getMeta("warnings");
		warnings = new ArrayList<>();
		if (warningsKey.isNull()) {
			return;
		}
		final int nr = warningsKey.getInteger();
		for (int i = 0; i <= nr; i++) {
			warnings.add(new WarningEntry(k, nr));
		}
	}

	public Key getErrorKey() {
		return errorKey;
	}

	public String getErrorCode() {
		return errorKey.getMeta("error/number").getString();
	}

	public String getConfigFile() {
		return errorKey.getMeta("error/configfile").getString();
	}

	public String getMountpoint() {
		return errorKey.getMeta("error/mountpoint").getString();
	}

	public String getDebugInformation() {
		return String.format("At: %s:%s%n", errorKey.getMeta("error/file").getString(),
				errorKey.getMeta("error/line").getString());
	}

	public String getModule() {
		return errorKey.getMeta("error/module").getString();
	}

	@Override
	public String getLocalizedMessage() {
		return getMessage();
	}

	@Override
	public String getMessage() {
		return errorKey.getMeta("error/reason").getString();
	}

	public boolean hasWarnings() {
		return !warnings.isEmpty();
	}

	public Collection<WarningEntry> getWarnings() {
		return warnings;
	}
}
