package org.libelektra;

import org.libelektra.Key.KeyTypeConversion;

public final class KeyUtils {

	private KeyUtils() {
		// utility class
	}

	public static String getWarnings(final Key errorKey) {
		final StringBuilder warningsBuilder = new StringBuilder();

		try {
			final Key warningsKey = errorKey.getMeta("warnings");
			if (warningsKey.isNull()) {
				// no errors were issued
				return warningsBuilder.toString();
			}
			final int nr = warningsKey.getInteger();
			if (nr == 1) {
				warningsBuilder.append("1 Warning was issued:\n");
			} else {
				warningsBuilder.append(nr + 1);
				warningsBuilder.append(" Warnings were issued:\n");
			}

			for (int i = 0; i <= nr; i++) {
				final String warningKeyName = String.format("warnings/#%02d", i);
				warningsBuilder.append(String.format(" Warning number: %s%n", errorKey.getMeta(warningKeyName + "/number").getString()));
				warningsBuilder.append(String.format("\tDescription: %s%n", errorKey.getMeta(warningKeyName + "/description").getString()));
				warningsBuilder.append(String.format("\tIngroup: %s%n", errorKey.getMeta(warningKeyName + "/ingroup").getString()));
				warningsBuilder.append(String.format("\tModule: %s%n", errorKey.getMeta(warningKeyName + "/module").getString()));
				warningsBuilder.append(String.format("\tAt: %s:%s%n", errorKey.getMeta(warningKeyName + "/file").getString(),
						errorKey.getMeta(warningKeyName + "/line").getString()));
				warningsBuilder.append(String.format("\tReason: %s%n", errorKey.getMeta(warningKeyName + "/reason").getString()));
				warningsBuilder.append(String.format("\tMountpoint: %s%n", errorKey.getMeta(warningKeyName + "/mountpoint").getString()));
				warningsBuilder.append(String.format("\tConfigfile: %s%n", errorKey.getMeta(warningKeyName + "/configfile").getString()));
			}
		} catch (final KeyTypeConversion ex) {
			warningsBuilder.append("Warnings metadata not set correctly by a plugin: " + ex.getMessage());
		}
		return warningsBuilder.toString();
	}

}
