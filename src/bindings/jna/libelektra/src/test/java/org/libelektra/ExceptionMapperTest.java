package org.libelektra;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ExceptionMapperTest
{

	@Test public void kdbSetWithError_shouldThrowInternalExceptionOnUnmappedError () throws Exception
	{
		String errorNumber = "abc123";
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException mappedException = KDBException.getMappedException (temporaryError);
		assertEquals (String.format (KDBException.MSG_UNKNOWN_ERRROR_NUMBER, errorNumber), mappedException.getReason ());
	}
}
