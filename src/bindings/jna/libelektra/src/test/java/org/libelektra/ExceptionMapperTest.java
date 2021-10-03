package org.libelektra;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ExceptionMapperTest {

  @Test
  public void kdbSetWithError_shouldThrowInternalExceptionOnUnmappedError() throws Exception {
    var errorNumber = "abc123";
    var errorKey = Key.create("user:/temporary/errorkey").setMeta("error/number", errorNumber);
    var mappedException = KDBException.getMappedException(errorKey);

    assertEquals(
        String.format(KDBException.MSG_UNKNOWN_ERROR_NUMBER, errorNumber),
        mappedException.getReason());
  }
}
