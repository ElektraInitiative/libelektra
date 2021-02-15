package org.libelektra;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.libelektra.exception.model.WarningEntryTest;

@RunWith (Suite.class)
@SuiteClasses ({ KeySetTest.class, KeyTest.class, KDBTest.class, ExceptionMapperTest.class, KDBExceptionTest.class, PluginLoaderIT.class,
		 WarningEntryTest.class, ExceptionMapperIT.class, GOptsTest.class })
public class AllTests
{
}