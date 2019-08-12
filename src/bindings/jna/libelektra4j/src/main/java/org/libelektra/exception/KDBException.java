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

	public String getReason() {
		return errorKey.getMeta("error/reason").getString();
	}

	@Override
	public String getMessage() {
		StringBuilder builder = new StringBuilder();
		builder.append(String.format("Sorry, module %s issued error %s:", getModule(), getErrorCode())).append("\n");
		builder.append(getReason()).append("\n");
		if (!errorKey.getMeta("error/configfile").isNull()) {
			builder.append("Configfile: ").append(getConfigFile()).append("\n");
		}
		if (!errorKey.getMeta("error/mountpoint").isNull()) {
			builder.append("Mountpoint: ").append(getMountpoint()).append("\n");
		}
		if (!errorKey.getMeta("error/file").isNull()) {
			builder.append(getDebugInformation()).append("\n");
		}
		return builder.toString();
	}

	public boolean hasWarnings() {
		return !warnings.isEmpty();
	}

	public Collection<WarningEntry> getWarnings() {
		return warnings;
	}
}