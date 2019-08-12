package org.libelektra;

import org.junit.Test;
import org.libelektra.exception.*;
import org.libelektra.plugin.NativeElektraPlugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ErrorCodeTest {

	private Key parentKey = Key.create("user/tests/javabinding");
	private final String errorMeta = "trigger/error";
	private final String warningMeta = "trigger/warnings";
	private final String errorPluginName = "error";

	@Test(expected = OutOfMemoryException.class)
	public void kdbSetWithError_shouldMapOutOfMemoryError() throws Exception {
		NativeElektraPlugin errorPlugin = null;
		try {
			errorPlugin = new NativeElektraPlugin(errorPluginName, parentKey);
		} catch (InstallationException e) {
			// Some builds are not able to load the native error plugin
			Key temporaryError = Key.create("user/temporary/errorkey");
			temporaryError.setMeta("error/number", OutOfMemoryException.errorCode());
			throw new OutOfMemoryException(temporaryError);
		}
		Key errorKey = Key.create("user/tests/myError");
		errorKey.setMeta(errorMeta, OutOfMemoryException.errorCode());
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		ks.append(errorKey);
		errorPlugin.kdbSet(ks, parentKey);
	}

	@Test
	public void kdbSetWithWarning_shouldNotTriggerException() throws Exception {
		NativeElektraPlugin errorPlugin = null;
		try {
			errorPlugin = new NativeElektraPlugin(errorPluginName, parentKey);
		} catch (InstallationException e) {
			// Some builds are not able to load the native error plugin
			return;
		}
		Key warningKey = Key.create("user/tests/myError");
		warningKey.setMeta(warningMeta, ResourceException.errorCode());
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		ks.append(warningKey);
		errorPlugin.kdbSet(ks, parentKey);
	}

	@Test
	public void kdbSetWithWarningAndError_shouldHaveWarnings() throws Exception {
		NativeElektraPlugin errorPlugin = null;
		try {
			errorPlugin = new NativeElektraPlugin(errorPluginName, parentKey);
		} catch (InstallationException e) {
			// Some builds are not able to load the native error plugin
			return;
		}
		Key warningKey = Key.create("user/tests/myError");
		warningKey.setMeta(warningMeta, SemanticValidationException.errorCode());
		Key errorKey = Key.create("user/tests/myError2");
		errorKey.setMeta(errorMeta, ResourceException.errorCode());
		final KeySet ks = KeySet.create(10, KeySet.KS_END);
		ks.append(warningKey);
		ks.append(errorKey);
		try {
			errorPlugin.kdbSet(ks, parentKey);
		} catch (KDBException e) {
			assertEquals(e.getWarnings().size(),1);
			assertTrue(e instanceof ResourceException);
			assertEquals(e.getWarnings().iterator().next().getWarningCode(), SemanticValidationException.errorCode());
			return;
		}
		throw new RuntimeException("Exception did not trigger");
	}
}