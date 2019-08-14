package org.libelektra.exception;

import org.libelektra.Key;
import org.libelektra.exception.model.WarningEntry;

import java.util.ArrayList;
import java.util.Collection;


/**
 * This exception wraps Elektra errors into the corresponding Java Exceptions
 */
public abstract class KDBException extends Exception {

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

	/**
	 * Gets the errorKey from Elektra
	 *
	 * @return ErrorKey from Elektra
	 */
	public Key getErrorKey() {
		return errorKey;
	}

	/**
	 * Gets the errorNumber from Elektra
	 *
	 * @return ErrorNumber from Elektra
	 */
	public String getErrorNumber() {
		return errorKey.getMeta("error/number").getString();
	}

	/**
	 * Returns the affected configuration file of the error.
	 * It empty returns the parents Key name
	 *
	 * @return either the configuration file or if empty the parent key name
	 */
	public String getConfigFile() {
		Key configKey = errorKey.getMeta("error/configfile");
		if (configKey.isNull()) {
			return errorKey.getString();
		}
		return configKey.getString();
	}

	/**
	 * Returns the mountpoint of the configuration
	 *
	 * @return the mountpoint of the configuration
	 */
	public String getMountpoint() {
		return errorKey.getMeta("error/mountpoint").getString();
	}

	/**
	 * Prints Elektra specific debug information in the form of "At: file:line"
	 *
	 * @return Elektra specific debug information in the form of "At: file:line"
	 */
	public String getDebugInformation() {
		return String.format("At: %s:%s%n", errorKey.getMeta("error/file").getString(),
				errorKey.getMeta("error/line").getString());
	}

	/**
	 * Returns the module which has thrown the error
	 *
	 * @return the module which has thrown the error
	 */
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
		builder.append(String.format("Sorry, module %s issued error %s:", getModule(), getErrorNumber())).append("\n");
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

	/**
	 * If an error occurred it may also has important warnings which caused the error.
	 * This method checks if they are available
	 *
	 * @return true if additional warnings were emitted
	 */
	public boolean hasWarnings() {
		return !warnings.isEmpty();
	}

	/**
	 * Returns the warnings collection
	 *
	 * @return the warnings collection
	 */
	public Collection<WarningEntry> getWarnings() {
		return warnings;
	}
}
