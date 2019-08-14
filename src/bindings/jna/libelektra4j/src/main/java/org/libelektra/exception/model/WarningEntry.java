package org.libelektra.exception.model;

import org.libelektra.Key;

public class WarningEntry {

	private final String warningCode;
	private final String reason;
	private final String mountpoint;
	private final String configFile;
	private final String debugInformation;
	private final String module;

	public WarningEntry(Key key, int current) {
		final String warningKeyName = String.format("warnings/#%02d", current);
		warningCode = key.getMeta(warningKeyName + "/number").getString();
		reason = key.getMeta(warningKeyName + "/reason").getString();
		module = key.getMeta(warningKeyName + "/module").getString();
		debugInformation = String.format("\tAt: %s:%s%n", key.getMeta(warningKeyName + "/file").getString(),
				key.getMeta(warningKeyName + "/line").getString());
		mountpoint = key.getMeta(warningKeyName + "/mountpoint").getString();
		configFile = key.getMeta(warningKeyName + "/configfile").getString();
	}

	/**
	 * Returns the warningNumber from Elektra
	 *
	 * @return the warningNumber from Elektra
	 */
	public String getWarningNumber() {
		return warningCode;
	}

	/**
	 * Returns the reason text from the warning
	 *
	 * @return the reason text from the warning
	 */
	public String getReason() {
		return reason;
	}

	/**
	 * Returns the mountpoint from the warning
	 *
	 * @return the mountpoint from the warning
	 */
	public String getMountpoint() {
		return mountpoint;
	}

	/**
	 * Returns the module from the warning
	 *
	 * @return the module from the warning
	 */
	public String getModule() {
		return module;
	}

	/**
	 * Returns the configuration file from the warning
	 *
	 * @return the configuration file from the warning
	 */
	public String getConfigFile() {
		return configFile;
	}

	/**
	 * Returns the debug information from the warning in the form of "At: file:line"
	 *
	 * @return the debug information from the warning in the form of "At: file:line"
	 */
	public String getDebugInformation() {
		return debugInformation;
	}
}
