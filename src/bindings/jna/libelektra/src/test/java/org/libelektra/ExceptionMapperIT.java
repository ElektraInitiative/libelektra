package org.libelektra;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.libelektra.exception.ConflictingStateException;
import org.libelektra.exception.InstallationException;
import org.libelektra.exception.InterfaceException;
import org.libelektra.exception.InternalException;
import org.libelektra.exception.OutOfMemoryException;
import org.libelektra.exception.PluginMisbehaviorException;
import org.libelektra.exception.ResourceException;
import org.libelektra.exception.SemanticValidationException;
import org.libelektra.exception.SyntacticValidationException;

public class ExceptionMapperIT
{

	private Key parentKey = Key.create ("user:/tests/javabinding");
	private final String errorMeta = "trigger/error";
	private final String warningMeta = "trigger/warnings";
	private final String errorPluginName = "error";

	@Test (expected = OutOfMemoryException.class) public void kdbSetWithError_shouldMapOutOfMemoryError () throws Exception
	{
		String errorNumber = OutOfMemoryException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new OutOfMemoryException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = InternalException.class) public void kdbSetWithError_shouldMapInternalError () throws Exception
	{
		String errorNumber = InternalException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new InternalException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = InterfaceException.class) public void kdbSetWithError_shouldMapInterfaceError () throws Exception
	{
		String errorNumber = InterfaceException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new InterfaceException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = InstallationException.class) public void kdbSetWithError_shouldMapInstallationError () throws Exception
	{
		String errorNumber = InstallationException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new InstallationException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = PluginMisbehaviorException.class) public void kdbSetWithError_shouldMapPluginMisbehaviorError () throws Exception
	{
		String errorNumber = PluginMisbehaviorException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new PluginMisbehaviorException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = ConflictingStateException.class) public void kdbSetWithError_shouldMapConflictError () throws Exception
	{
		String errorNumber = ConflictingStateException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new ConflictingStateException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = SyntacticValidationException.class)
	public void kdbSetWithError_shouldMapSyntacticValidationError () throws Exception
	{
		String errorNumber = SyntacticValidationException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new SyntacticValidationException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	@Test (expected = SemanticValidationException.class)
	public void kdbSetWithError_shouldMapSemanticValidationError () throws Exception
	{
		String errorNumber = SemanticValidationException.ERROR_NUMBER;
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException exception = new SemanticValidationException (temporaryError);
		passErrorToPluginAndExecute (exception);
	}

	private void passErrorToPluginAndExecute (KDBException exception) throws KDBException
	{
		NativePlugin errorPlugin = null;
		try
		{
			errorPlugin = new NativePlugin (errorPluginName, parentKey, KeySet.create ());
		}
		catch (InstallationException e)
		{
			// On some builds are not able to load the native error plugin
			throw exception;
		}
		Key errorKey = Key.create ("user:/tests/myError");
		errorKey.setMeta (errorMeta, exception.getErrorNumber ());
		final KeySet ks = KeySet.create (10, KeySet.KS_END);
		ks.append (errorKey);
		errorPlugin.set (ks, parentKey);
	}

	@Test public void kdbSetWithWarning_shouldNotTriggerException () throws Exception
	{
		NativePlugin errorPlugin = null;
		try
		{
			errorPlugin = new NativePlugin (errorPluginName, parentKey, KeySet.create ());
		}
		catch (InstallationException e)
		{
			// On some builds are not able to load the native error plugin
			return;
		}
		Key warningKey = Key.create ("user:/tests/myError");
		warningKey.setMeta (warningMeta, ResourceException.ERROR_NUMBER);
		final KeySet ks = KeySet.create (10, KeySet.KS_END);
		ks.append (warningKey);
		errorPlugin.set (ks, parentKey);
	}

	@Test public void kdbSetWithWarningAndError_shouldHaveWarnings () throws Exception
	{
		NativePlugin errorPlugin = null;
		try
		{
			errorPlugin = new NativePlugin (errorPluginName, parentKey, KeySet.create ());
		}
		catch (InstallationException e)
		{
			// On some builds are not able to load the native error plugin
			return;
		}
		Key warningKey = Key.create ("user:/tests/myError");
		warningKey.setMeta (warningMeta, SemanticValidationException.ERROR_NUMBER);
		Key errorKey = Key.create ("user:/tests/myError2");
		errorKey.setMeta (errorMeta, ResourceException.ERROR_NUMBER);
		final KeySet ks = KeySet.create (10, KeySet.KS_END);
		ks.append (warningKey);
		ks.append (errorKey);
		try
		{
			errorPlugin.set (ks, parentKey);
		}
		catch (KDBException e)
		{
			assertEquals (e.getWarnings ().size (), 1);
			assertTrue (e instanceof ResourceException);
			assertEquals (e.getWarnings ().iterator ().next ().getWarningNumber (), SemanticValidationException.ERROR_NUMBER);
			return;
		}
		throw new RuntimeException ("Exception did not trigger");
	}
}
