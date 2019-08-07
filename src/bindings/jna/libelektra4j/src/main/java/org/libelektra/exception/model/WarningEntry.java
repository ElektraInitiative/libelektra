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

	public String getWarningCode() {
		return warningCode;
	}

	public String getReason() {
		return reason;
	}

	public String getMountpoint() {
		return mountpoint;
	}

	public String getModule() {
		return module;
	}

	public String getConfigFile() {
		return configFile;
	}

	public String getDebugInformation() {
		return debugInformation;
	}
}
