package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;
import org.libelektra.exception.InstallationException;
import org.libelektra.exception.InternalException;

public class KDBExceptionTest
{

	@Test public void KdbException_shouldCorrectlyExtractMetadata ()
	{
		var errorNumber = InstallationException.ERROR_NUMBER;
		var configFile = "file.conf";
		var mountpoint = "system:/test";
		var file = "kdb.c";
		var line = "50";
		var module = "kdb";
		var reason = "This is a testerror";

		var errorKey = Key.create ("user:/temporary/errorkey");
		errorKey.setMeta ("error/number", errorNumber);
		errorKey.setMeta ("error/configfile", configFile);
		errorKey.setMeta ("error/mountpoint", mountpoint);
		errorKey.setMeta ("error/file", file);
		errorKey.setMeta ("error/line", line);
		errorKey.setMeta ("error/module", module);
		errorKey.setMeta ("error/reason", reason);

		KDBException exception = new InstallationException (errorKey);

		assertEquals (errorNumber, exception.getErrorNumber ());
		assertEquals (configFile, exception.getConfigFile ());
		assertEquals (mountpoint, exception.getMountpoint ());
		assertEquals (module, exception.getModule ());
		assertEquals (reason, exception.getReason ());

		var expectedDebugInformation = String.format (KDBException.MSG_DEBUGINFO, file, line);
		assertEquals (expectedDebugInformation, exception.getDebugInformation ());

		StringBuilder builder = new StringBuilder ();
		builder.append (String.format (KDBException.MSG_MODULE_ERRROR_NUMBER, module, errorNumber)).append ("\n");
		builder.append (reason).append ("\n");
		builder.append (KDBException.MSG_CONFIGFILE).append (configFile).append ("\n");
		builder.append (KDBException.MSG_MOUNTPOINT).append (mountpoint).append ("\n");
		builder.append (expectedDebugInformation).append ("\n");
		String expectedErrorMessage = builder.toString ();

		assertEquals (expectedErrorMessage, exception.getMessage ());
		assertFalse (exception.hasWarnings ());
		assertEquals (errorKey, exception.getErrorKey ());
	}

	@Test public void KdbException_shouldCorrectlyProvideConfigFileIfEmpty ()
	{
		var errorNumber = InstallationException.ERROR_NUMBER;
		var reason = "This is a testerror";
		var errorKeyName = "user:/temporary/errorkey";

		var errorKey = Key.create (errorKeyName);
		errorKey.setMeta ("error/number", errorNumber);
		errorKey.setMeta ("error/reason", reason);

		KDBException exception = new InstallationException (errorKey);
		assertEquals (errorKeyName, exception.getConfigFile ());
	}

	@Test public void KdbException_shouldCorrectlyDecodeWarnings ()
	{
		var errorKey = Key.create ("user:/temporary/warningkey");
		errorKey.setMeta ("warnings", "#1");
		errorKey.setMeta ("warnings/#0/number", "C01100");
		errorKey.setMeta ("warnings/#0/reason", "Test reason");
		errorKey.setMeta ("warnings/#0/module", "test module");
		errorKey.setMeta ("warnings/#0/file", "testfile");
		errorKey.setMeta ("warnings/#0/line", "0");
		errorKey.setMeta ("warnings/#0/mountpoint", "test");
		errorKey.setMeta ("warnings/#0/configfile", "testconfig");
		errorKey.setMeta ("warnings/#1/number", "C03100");
		errorKey.setMeta ("warnings/#1/reason", "Test reason 2");
		errorKey.setMeta ("warnings/#1/module", "test module 2");
		errorKey.setMeta ("warnings/#1/file", "testfile 2");
		errorKey.setMeta ("warnings/#1/line", "1");
		errorKey.setMeta ("warnings/#1/mountpoint", "test2");
		errorKey.setMeta ("warnings/#1/configfile", "testconfig 2");

		KDBException exception = new InternalException (errorKey);

		assertEquals (2, exception.getWarnings ().size ());

		assertEquals ("C01100", exception.getWarnings ().get (0).getWarningNumber ());
		assertEquals ("Test reason", exception.getWarnings ().get (0).getReason ());
		assertEquals ("test module", exception.getWarnings ().get (0).getModule ());
		assertEquals ("At: testfile:0", exception.getWarnings ().get (0).getDebugInformation ());
		assertEquals ("test", exception.getWarnings ().get (0).getMountpoint ());
		assertEquals ("testconfig", exception.getWarnings ().get (0).getConfigFile ());

		assertEquals ("C03100", exception.getWarnings ().get (1).getWarningNumber ());
		assertEquals ("Test reason 2", exception.getWarnings ().get (1).getReason ());
		assertEquals ("test module 2", exception.getWarnings ().get (1).getModule ());
		assertEquals ("At: testfile 2:1", exception.getWarnings ().get (1).getDebugInformation ());
		assertEquals ("test2", exception.getWarnings ().get (1).getMountpoint ());
		assertEquals ("testconfig 2", exception.getWarnings ().get (1).getConfigFile ());
	}

	@Test public void warningEntry_shouldBePardsedCorrectly ()
	{
		var errorNumber = InstallationException.ERROR_NUMBER;
		var configFile = "file.conf";
		var mountpoint = "system:/test";
		var file = "kdb.c";
		var line = "50";
		var module = "kdb";
		var reason = "This is a testerror";

		var warningKey = Key.create ("user:/temporary/errorkey");
		warningKey.setMeta ("warnings/#0/number", errorNumber);
		warningKey.setMeta ("warnings/#0/configfile", configFile);
		warningKey.setMeta ("warnings/#0/mountpoint", mountpoint);
		warningKey.setMeta ("warnings/#0/file", file);
		warningKey.setMeta ("warnings/#0/line", line);
		warningKey.setMeta ("warnings/#0/module", module);
		warningKey.setMeta ("warnings/#0/reason", reason);

		var warningEntry = new KDBException.WarningEntry (warningKey, 0);

		assertEquals (errorNumber, warningEntry.getWarningNumber ());
		assertEquals (configFile, warningEntry.getConfigFile ());
		assertEquals (mountpoint, warningEntry.getMountpoint ());
		assertEquals (module, warningEntry.getModule ());
		assertEquals (reason, warningEntry.getReason ());

		var expectedDebugInformation = String.format (KDBException.MSG_DEBUGINFO, file, line);
		assertEquals (expectedDebugInformation, warningEntry.getDebugInformation ());
	}
}
