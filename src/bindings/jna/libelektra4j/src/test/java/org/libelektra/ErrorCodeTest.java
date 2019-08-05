package org.libelektra;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import org.libelektra.exception.KDBException;
import org.libelektra.exception.OutOfMemoryException;
import org.libelektra.exception.ResourceException;
import org.libelektra.exception.SemanticValidationException;

public class ErrorCodeTest {

    private Key parentKey = Key.create("user/tests/javabinding");
    private final String errorMeta = "trigger/error";
    private final String warningMeta = "trigger/warnings";
    private Plugin errorPlugin;

    @Before
    public void setup(){
        errorPlugin = new Plugin("error", parentKey);
    }

    @Test(expected = OutOfMemoryException.class)
    public void kdbSetWithError_shouldMapOutOfMemoryError() throws Exception {
        Key errorKey = Key.create("user/tests/myError");
        errorKey.setMeta(errorMeta, OutOfMemoryException.errorCode());
        final KeySet ks = KeySet.create(10, KeySet.KS_END);
        ks.append(errorKey);
        errorPlugin.kdbSet(ks, parentKey);
    }

    @Test
    public void kdbSetWithWarning_shouldNotTriggerException() throws Exception {
        Key warningKey = Key.create("user/tests/myError");
        warningKey.setMeta(warningMeta, ResourceException.errorCode());
        final KeySet ks = KeySet.create(10, KeySet.KS_END);
        ks.append(warningKey);
        errorPlugin.kdbSet(ks, parentKey);
    }

    @Test
    public void kdbSetWithWarningAndError_shouldHaveWarnings() {
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
