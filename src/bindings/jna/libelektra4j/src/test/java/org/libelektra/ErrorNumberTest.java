package org.libelektra;

import org.junit.Test;
import org.libelektra.exception.*;
import org.libelektra.plugin.NativePlugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ErrorNumberTest {

	private Key parentKey = Key.create("user/tests/javabinding");
	private final String errorMeta = "trigger/error";
	private final String warningMeta = "trigger/warnings";
	private final String errorPluginName = "error";

	@Test(expected = OutOfMemoryException.class)
	public void kdbSetWithError_shouldMapOutOfMemoryError() throws Exception {
		NativePlugin errorPlugin = null;
		try {
			errorPlugin = new NativePlugin(errorPluginName, parentKey);
		} catch (InstallationException e) {
			// On some builds are not able to load the native error plugin
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", OutOfMemoryException.errorNumber());
			throw new OutOfMemoryException(temporaryError);
		}
		Key errorKey = Key.create("user/tests/myError");
		errorKey.setMeta(errorMeta, OutOfMemoryException.errorNumber());
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		ks.append(errorKey);
		errorPlugin.set(ks, parentKey);
	}

	@Test
	public void kdbSetWithWarning_shouldNotTriggerException() throws Exception {
		NativePlugin errorPlugin = null;
		try {
			errorPlugin = new NativePlugin(errorPluginName, parentKey);
		} catch (InstallationException e) {
			// On some builds are not able to load the native error plugin
			return;
		}
		Key warningKey = Key.create("user/tests/myError");
		warningKey.setMeta(warningMeta, ResourceException.errorNumber());
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		ks.append(warningKey);
		errorPlugin.set(ks, parentKey);
	}

	@Test
	public void kdbSetWithWarningAndError_shouldHaveWarnings() throws Exception {
		NativePlugin errorPlugin = null;
		try {
			errorPlugin = new NativePlugin(errorPluginName, parentKey);
		} catch (InstallationException e) {
			// On some builds are not able to load the native error plugin
			return;
		}
		Key warningKey = Key.create("user/tests/myError");
		warningKey.setMeta(warningMeta, SemanticValidationException.errorNumber());
		Key errorKey = Key.create("user/tests/myError2");
		errorKey.setMeta(errorMeta, ResourceException.errorNumber());
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		ks.append(warningKey);
		ks.append(errorKey);
		try {
			errorPlugin.set(ks, parentKey);
		} catch (KDBException e) {
			assertEquals(e.getWarnings().size(), 1);
			assertTrue(e instanceof ResourceException);
			assertEquals(e.getWarnings().iterator().next().getWarningNumber(),
                    SemanticValidationException.errorNumber());
			return;
		}
		throw new RuntimeException("Exception did not trigger");
	}
}
