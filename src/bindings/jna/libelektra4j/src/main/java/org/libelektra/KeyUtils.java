package org.libelektra;

import org.libelektra.Key.KeyTypeConversion;

public final class KeyUtils {

	private KeyUtils() {
		// utility class
	}

	public static String getErrors(final Key key) {
		final StringBuilder sb = new StringBuilder();

		try {
			if (key.getMeta("error").isNull()) {
				// no errors were issued
				return sb.toString();
			}

			sb.append(String.format("Sorry, the error #%s occurred!%n", key.getMeta("error/number").getString()));
			sb.append(String.format("Description: %s%n", key.getMeta("error/description").getString()));
			sb.append(String.format("Ingroup: %s%n", key.getMeta("error/ingroup").getString()));
			sb.append(String.format("Module: %s%n", key.getMeta("error/module").getString()));
			sb.append(String.format("At: %s:%s%n", key.getMeta("error/file").getString(), key.getMeta("error/line").getString()));
			sb.append(String.format("Reason: %s%n", key.getMeta("error/reason").getString()));
			sb.append(String.format("Mountpoint: %s%n", key.getMeta("error/mountpoint").getString()));
			sb.append(String.format("Configfile: %s%n", key.getMeta("error/configfile").getString()));
		} catch (final KeyTypeConversion ex) {
			sb.append(String.format("Error metadata is not set correctly by a plugin: %s%n", ex.getMessage()));
		}

		return sb.toString();
	}

	public static String getWarnings(final Key key) {
		final StringBuilder sb = new StringBuilder();

		try {
			final Key warningsKey = key.getMeta("warnings");
			if (warningsKey.isNull()) {
				// no warnings were issued
				return sb.toString();
			}

			final int nr = warningsKey.getInteger();
			if (nr == 0) {
				sb.append("1 Warning was issued:\n");
			} else {
				sb.append(nr + 1).append(" Warnings were issued:\n");
			}

			for (int i = 0; i <= nr; i++) {
				final String warningKeyName = String.format("warnings/#%02d", i);
				sb.append(String.format(" Warning number: %s%n", key.getMeta(warningKeyName + "/number").getString()));
				sb.append(String.format("\tDescription: %s%n", key.getMeta(warningKeyName + "/description").getString()));
				sb.append(String.format("\tIngroup: %s%n", key.getMeta(warningKeyName + "/ingroup").getString()));
				sb.append(String.format("\tModule: %s%n", key.getMeta(warningKeyName + "/module").getString()));
				sb.append(String.format("\tAt: %s:%s%n", key.getMeta(warningKeyName + "/file").getString(),
						key.getMeta(warningKeyName + "/line").getString()));
				sb.append(String.format("\tReason: %s%n", key.getMeta(warningKeyName + "/reason").getString()));
				sb.append(String.format("\tMountpoint: %s%n", key.getMeta(warningKeyName + "/mountpoint").getString()));
				sb.append(String.format("\tConfigfile: %s%n", key.getMeta(warningKeyName + "/configfile").getString()));
			}
		} catch (final KeyTypeConversion ex) {
			sb.append(String.format("Warnings metadata not set correctly by a plugin: %s%n", ex.getMessage()));
		}
		return sb.toString();
	}

}
