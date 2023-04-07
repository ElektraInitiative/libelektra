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

public class ExceptionMapperIT {

  private final Key parentKey = Key.create("user:/tests/javabinding");
  private final String errorMeta = "trigger/error";
  private final String warningMeta = "trigger/warnings";
  private final String errorPluginName = "error";

  private static Key buildTempKey(String errorNumber) {
    return Key.create("user:/temporary/errorkey").setMeta("error/number", errorNumber);
  }

  @Test(expected = OutOfMemoryException.class)
  public void kdbSetWithError_shouldMapOutOfMemoryError() throws Exception {
    passErrorToPluginAndExecute(
        new OutOfMemoryException(buildTempKey(OutOfMemoryException.ERROR_NUMBER)));
  }

  @Test(expected = InternalException.class)
  public void kdbSetWithError_shouldMapInternalError() throws Exception {
    passErrorToPluginAndExecute(
        new InternalException(buildTempKey(InternalException.ERROR_NUMBER)));
  }

  @Test(expected = InterfaceException.class)
  public void kdbSetWithError_shouldMapInterfaceError() throws Exception {
    passErrorToPluginAndExecute(
        new InterfaceException(buildTempKey(InterfaceException.ERROR_NUMBER)));
  }

  @Test(expected = InstallationException.class)
  public void kdbSetWithError_shouldMapInstallationError() throws Exception {
    passErrorToPluginAndExecute(
        new InstallationException(buildTempKey(InstallationException.ERROR_NUMBER)));
  }

  @Test(expected = PluginMisbehaviorException.class)
  public void kdbSetWithError_shouldMapPluginMisbehaviorError() throws Exception {
    passErrorToPluginAndExecute(
        new PluginMisbehaviorException(buildTempKey(PluginMisbehaviorException.ERROR_NUMBER)));
  }

  @Test(expected = ConflictingStateException.class)
  public void kdbSetWithError_shouldMapConflictError() throws Exception {
    passErrorToPluginAndExecute(
        new ConflictingStateException(buildTempKey(ConflictingStateException.ERROR_NUMBER)));
  }

  @Test(expected = SyntacticValidationException.class)
  public void kdbSetWithError_shouldMapSyntacticValidationError() throws Exception {
    passErrorToPluginAndExecute(
        new SyntacticValidationException(buildTempKey(SyntacticValidationException.ERROR_NUMBER)));
  }

  @Test(expected = SemanticValidationException.class)
  public void kdbSetWithError_shouldMapSemanticValidationError() throws Exception {
    passErrorToPluginAndExecute(
        new SemanticValidationException(buildTempKey(SemanticValidationException.ERROR_NUMBER)));
  }

  private void passErrorToPluginAndExecute(KDBException exception) throws KDBException {
    NativePlugin errorPlugin;
    try {
      errorPlugin = new NativePlugin(errorPluginName, parentKey, KeySet.create());
    } catch (InstallationException e) {
      // some builds are not able to load the native error plugin
      throw exception;
    }
    var errorKey = Key.create("user:/tests/myError").setMeta(errorMeta, exception.getErrorNumber());
    var keySet = KeySet.create(errorKey);
    errorPlugin.set(keySet, parentKey);
  }

  @Test
  public void kdbSetWithWarning_shouldNotTriggerException() throws Exception {
    NativePlugin errorPlugin;
    try {
      errorPlugin = new NativePlugin(errorPluginName, parentKey, KeySet.create());
    } catch (InstallationException e) {
      // some builds are not able to load the native error plugin
      return;
    }
    var warningKey =
        Key.create("user:/tests/myError").setMeta(warningMeta, ResourceException.ERROR_NUMBER);
    var keySet = KeySet.create(warningKey);
    errorPlugin.set(keySet, parentKey);
  }

  @Test
  public void kdbSetWithWarningAndError_shouldHaveWarnings() throws Exception {
    NativePlugin errorPlugin;
    try {
      errorPlugin = new NativePlugin(errorPluginName, parentKey, KeySet.create());
    } catch (InstallationException e) {
      // some builds are not able to load the native error plugin
      return;
    }
    var warningKey =
        Key.create("user:/tests/myError")
            .setMeta(warningMeta, SemanticValidationException.ERROR_NUMBER);
    var errorKey =
        Key.create("user:/tests/myError2").setMeta(errorMeta, ResourceException.ERROR_NUMBER);
    var keySet = KeySet.create(warningKey, errorKey);
    try {
      errorPlugin.set(keySet, parentKey);
    } catch (KDBException e) {
      assertEquals(1, e.getWarnings().size());
      assertTrue(e instanceof ResourceException);
      assertEquals(
          SemanticValidationException.ERROR_NUMBER,
          e.getWarnings().iterator().next().getWarningNumber());
      return;
    }
    throw new RuntimeException("Exception did not trigger");
  }
}
