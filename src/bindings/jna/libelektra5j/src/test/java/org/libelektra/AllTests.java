package org.libelektra;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.libelektra.exception.model.WarningEntryTest;
import org.libelektra.plugin.ReturnTest;

@RunWith (Suite.class)
@SuiteClasses ({ KeySetTest.class, KeyTest.class, KeyNameIteratorTest.class, KDBTest.class, ExceptionMapperTest.class,
		 KDBExceptionTest.class, PluginLoaderIT.class, WarningEntryTest.class, ReturnTest.class, ExceptionMapperIT.class,
		 GOptsTest.class })
public class AllTests
{
}