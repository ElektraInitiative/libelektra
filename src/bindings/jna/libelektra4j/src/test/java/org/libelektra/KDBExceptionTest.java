package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;
import org.libelektra.exception.InstallationException;
import org.libelektra.exception.KDBException;

public class KDBExceptionTest
{

	@Test public void KdbException_shouldCorrectlyExtractMetadata ()
	{
		String errorNumber = InstallationException.errorNumber ();
		String configFile = "file.conf";
		String mountpoint = "system:/test";
		String file = "kdb.c";
		String line = "50";
		String module = "kdb";
		String reason = "This is a testerror";

		Key errorKey = Key.create ("user:/temporary/errorkey");
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

		String expectedDebugInformation = String.format ("At: %s:%s", file, line);
		assertEquals (expectedDebugInformation, exception.getDebugInformation ());

		StringBuilder builder = new StringBuilder ();
		builder.append (String.format ("Sorry, module %s issued error %s:", module, errorNumber)).append ("\n");
		builder.append (reason).append ("\n");
		builder.append ("Configfile: ").append (configFile).append ("\n");
		builder.append ("Mountpoint: ").append (mountpoint).append ("\n");
		builder.append (expectedDebugInformation).append ("\n");
		String expectedErrorMessage = builder.toString ();

		assertEquals (expectedErrorMessage, exception.getMessage ());
		assertFalse (exception.hasWarnings ());
		assertEquals (errorKey, exception.getErrorKey ());
	}

	@Test public void KdbException_shouldCorrectlyProvideConfigFileIfEmpty ()
	{
		String errorNumber = InstallationException.errorNumber ();
		String reason = "This is a testerror";
		String errorKeyName = "user:/temporary/errorkey";

		Key errorKey = Key.create (errorKeyName);
		errorKey.setMeta ("error/number", errorNumber);
		errorKey.setMeta ("error/reason", reason);

		KDBException exception = new InstallationException (errorKey);
		assertEquals (errorKeyName, exception.getConfigFile ());
	}
}
