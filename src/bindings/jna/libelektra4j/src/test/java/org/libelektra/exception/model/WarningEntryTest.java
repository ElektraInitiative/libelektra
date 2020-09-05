package org.libelektra.exception.model;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.libelektra.Key;
import org.libelektra.exception.InstallationException;

public class WarningEntryTest
{

	@Test public void warningEntry_shouldBePardsedCorrectly ()
	{
		String errorNumber = InstallationException.errorNumber ();
		String configFile = "file.conf";
		String mountpoint = "system:/test";
		String file = "kdb.c";
		String line = "50";
		String module = "kdb";
		String reason = "This is a testerror";

		final String warningKeyName = String.format ("warnings/#0", 0);
		Key warningKey = Key.create ("user:/temporary/errorkey");
		warningKey.setMeta ("warnings/#0/number", errorNumber);
		warningKey.setMeta ("warnings/#0/configfile", configFile);
		warningKey.setMeta ("warnings/#0/mountpoint", mountpoint);
		warningKey.setMeta ("warnings/#0/file", file);
		warningKey.setMeta ("warnings/#0/line", line);
		warningKey.setMeta ("warnings/#0/module", module);
		warningKey.setMeta ("warnings/#0/reason", reason);

		WarningEntry warningEntry = new WarningEntry (warningKey, 0);

		assertEquals (errorNumber, warningEntry.getWarningNumber ());
		assertEquals (configFile, warningEntry.getConfigFile ());
		assertEquals (mountpoint, warningEntry.getMountpoint ());
		assertEquals (module, warningEntry.getModule ());
		assertEquals (reason, warningEntry.getReason ());

		String expectedDebugInformation = String.format ("At: %s:%s", file, line);
		assertEquals (expectedDebugInformation, warningEntry.getDebugInformation ());
	}
}