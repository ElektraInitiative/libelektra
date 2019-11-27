package org.libelektra;

import org.junit.Test;
import org.libelektra.exception.KDBException;
import org.libelektra.exception.mapper.ExceptionMapperService;

import static org.junit.Assert.assertEquals;

public class ExceptionMapperTest
{

	@Test public void kdbSetWithError_shouldThrowInternalExceptionOnUnmappedError () throws Exception
	{
		String errorNumber = "abc123";
		Key temporaryError = Key.create ("user:/temporary/errorkey");
		temporaryError.setMeta ("error/number", errorNumber);
		KDBException mappedException = ExceptionMapperService.getMappedException (temporaryError);
		assertEquals ("Sorry, could not map error number '" + errorNumber +
				      "'. Please report this incident at https://issues.libelektra.org/",
			      mappedException.getReason ());
	}
}
