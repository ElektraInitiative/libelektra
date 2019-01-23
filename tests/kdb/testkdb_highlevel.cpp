/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <elektra.h>
#include <elektra/conversion.h>
#include <elektra/errorcodes.h>

#include <gtest/gtest-elektra.h>
#include <kdbhelper.h>

constexpr const char * severityString (ElektraErrorSeverity severity)
{
	return severity == ELEKTRA_ERROR_SEVERITY_ERROR ? "ERROR" : (severity == ELEKTRA_ERROR_SEVERITY_WARNING ? "WARNING" : "FATAL");
}

static inline const char * severityString (ElektraError * error)
{
	return severityString (elektraErrorSeverity (error));
}

static std::ostream & operator<< (std::ostream & os, ElektraError ** error)
{
	if (*error != nullptr)
	{
		os << "[" << severityString (*error) << "] (" << elektraErrorCode (*error) << ") " << elektraErrorDescription (*error)
		   << std::endl;

		int kdbCode = elektraKDBErrorCode (*error);
		if (kdbCode > 0)
		{
			os << "\tKDB";
			switch (elektraKDBErrorSeverity (*error))
			{
			case ELEKTRA_ERROR_SEVERITY_ERROR:
				os << "ERROR";
				break;
			case ELEKTRA_ERROR_SEVERITY_WARNING:
				os << "WARNING";
				break;
			default:
			case ELEKTRA_ERROR_SEVERITY_FATAL:
				os << "FATAL";
				break;
			}

			os << " " << kdbCode << " [" << elektraKDBErrorGroup (*error) << "/" << elektraKDBErrorModule (*error) << "]"
			   << ": " << elektraKDBErrorDescription (*error) << std::endl;
			os << "\t\tReason: " << elektraKDBErrorReason (*error) << std::endl;

			int warningCount = elektraKDBErrorWarningCount (*error);
			os << "\t\t" << warningCount << " Warnings:" << std::endl;
			for (int i = 0; i < warningCount; ++i)
			{
				ElektraError * warning = elektraKDBErrorGetWarning (*error, i);
				os << "\t\t - Warning " << elektraKDBErrorCode (warning) << " [" << elektraKDBErrorGroup (warning) << "/"
				   << elektraKDBErrorModule (warning) << "]: " << elektraKDBErrorDescription (warning) << std::endl;
				ckdb::elektraFree (warning);
			}
			os << "\t\tFrom Key: " << ckdb::keyName (elektraKDBErrorKey (*error)) << std::endl;
		}

		elektraErrorReset (error);
	}
	return os;
}

class Highlevel : public ::testing::Test
{
protected:
	static const std::string testRoot;
	static const std::string configFile;

	testing::Namespaces namespaces;
	testing::MountpointPtr mp;

	Elektra * elektra = nullptr;

	Highlevel () : namespaces ()
	{
	}

	void SetUp () override
	{
		mp.reset (new testing::Mountpoint (testRoot, configFile));
	}

	void TearDown () override
	{
		if (elektra != nullptr)
		{
			elektraClose (elektra);
		}

		mp.reset ();
	}

	static void fatalErrorHandler (ElektraError * error)
	{
		std::stringstream msg;
		msg << "fatal error " << elektraErrorCode (error) << " in test "
		    << ::testing::UnitTest::GetInstance ()->current_test_info ()->name () << ": " << &error << std::endl;

		throw std::runtime_error (msg.str ());
	}

	void createElektra (ckdb::KeySet * defaults = nullptr)
	{
		if (elektra != nullptr)
		{
			elektraClose (elektra);
		}

		ElektraError * error = nullptr;
		elektra = elektraOpen (testRoot.c_str (), defaults, &error);

		ASSERT_NE (elektra, nullptr) << "elektraOpen failed" << &error << std::endl;

		elektraFatalErrorHandler (elektra, &fatalErrorHandler);
	}

	void setKeyValue (KDBType type, const char * name, const char * value)
	{
		using namespace kdb;
		KDB kdb;
		KeySet config;

		kdb.get (config, testRoot);
		config.append (Key (testRoot + name, KEY_VALUE, value, KEY_META, "type", type, KEY_END));
		kdb.set (config, testRoot);
	}
};

const std::string Highlevel::configFile = "kdbFile.dump";
const std::string Highlevel::testRoot = "user/tests/highlevel/";

TEST_F (Highlevel, PrimitveGetters)
{
	setKeyValue (KDB_TYPE_STRING, "stringkey", "A string");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleankey", "1");
	setKeyValue (KDB_TYPE_CHAR, "charkey", "c");
	setKeyValue (KDB_TYPE_OCTET, "octetkey", "1");
	setKeyValue (KDB_TYPE_SHORT, "shortkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "1");
	setKeyValue (KDB_TYPE_LONG, "longkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "1");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "1");
	setKeyValue (KDB_TYPE_FLOAT, "floatkey", "1.1");
	setKeyValue (KDB_TYPE_DOUBLE, "doublekey", "1.1");

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1");

#endif

	createElektra ();

	EXPECT_STREQ (elektraGetString (elektra, "stringkey"), "A string") << "Wrong key value.";
	EXPECT_TRUE (elektraGetBoolean (elektra, "booleankey")) << "Wrong key value.";
	EXPECT_EQ (elektraGetChar (elektra, "charkey"), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGetOctet (elektra, "octetkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetShort (elektra, "shortkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShort (elektra, "unsignedshortkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLong (elektra, "longkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLong (elektra, "unsignedlongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLong (elektra, "longlongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLong (elektra, "unsignedlonglongkey"), 1) << "Wrong key value.";

	EXPECT_EQ (elektraGetFloat (elektra, "floatkey"), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetDouble (elektra, "doublekey"), 1.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraGetLongDouble (elektra, "longdoublekey"), 1.1L) << "Wrong key value.";

#endif
}

TEST_F (Highlevel, ArrayGetters)
{
	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#0", "String 1");
	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#1", "String 2");

	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#0", "0");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#1", "1");

	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#0", "c");
	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#1", "d");

	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#0", "1");
	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#1", "2");

	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#0", "1");
	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#1", "2");

	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#0", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#1", "2");

	setKeyValue (KDB_TYPE_LONG, "longarraykey/#0", "1");
	setKeyValue (KDB_TYPE_LONG, "longarraykey/#1", "2");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#0", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#1", "2");

	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#0", "1");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#1", "2");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#0", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#1", "2");

	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#0", "1.1");
	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#1", "2.1");

	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#0", "1.1");
	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#1", "2.1");

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#0", "1.1");
	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#1", "2.1");

#endif

	createElektra ();


	EXPECT_EQ (elektraArraySize (elektra, "stringarraykey"), 2) << "Wrong array size";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "stringarraykey", 0), "String 1") << "Wrong key value.";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "stringarraykey", 1), "String 2") << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "booleanarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 0), 0) << "Wrong key value.";
	EXPECT_TRUE (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 1)) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "chararraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetCharArrayElement (elektra, "chararraykey", 0), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGetCharArrayElement (elektra, "chararraykey", 1), 'd') << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "octetarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetOctetArrayElement (elektra, "octetarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetOctetArrayElement (elektra, "octetarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "shortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "shortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "shortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedshortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "longlongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedlonglongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "floatarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "floatarraykey", 0), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "floatarraykey", 1), 2.1f) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "doublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0), 1.1) << "Wrong key value.";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1), 2.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraArraySize (elektra, "longdoublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0), 1.1L) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1), 2.1L) << "Wrong key value.";

#endif
}

TEST_F (Highlevel, PrimitiveSetters)
{
	setKeyValue (KDB_TYPE_STRING, "stringkey", "");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleankey", "");
	setKeyValue (KDB_TYPE_CHAR, "charkey", "");
	setKeyValue (KDB_TYPE_OCTET, "octetkey", "");
	setKeyValue (KDB_TYPE_SHORT, "shortkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "1");
	setKeyValue (KDB_TYPE_LONG, "longkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "1");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongkey", "1");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "1");
	setKeyValue (KDB_TYPE_FLOAT, "floatkey", "1.1");
	setKeyValue (KDB_TYPE_DOUBLE, "doublekey", "1.1");

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1");

#endif
	createElektra ();

	ElektraError * error = nullptr;

	// Overwrite existing values.
	elektraSetString (elektra, "stringkey", "A string", &error);
	elektraSetBoolean (elektra, "booleankey", 1, &error);
	elektraSetChar (elektra, "charkey", 'c', &error);
	elektraSetOctet (elektra, "octetkey", 1, &error);
	elektraSetShort (elektra, "shortkey", 1, &error);
	elektraSetUnsignedShort (elektra, "unsignedshortkey", 1, &error);
	elektraSetLong (elektra, "longkey", 1, &error);
	elektraSetUnsignedLong (elektra, "unsignedlongkey", 1, &error);
	elektraSetLongLong (elektra, "longlongkey", 1, &error);
	elektraSetUnsignedLongLong (elektra, "unsignedlonglongkey", 1, &error);
	elektraSetFloat (elektra, "floatkey", 1.1, &error);
	elektraSetDouble (elektra, "doublekey", 1.1, &error);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	elektraSetLongDouble (elektra, "longdoublekey", 1.1L, &error);

#endif

	// Add new keys.
	elektraSetString (elektra, "newstringkey", "A string", &error);
	elektraSetBoolean (elektra, "newbooleankey", 1, &error);
	elektraSetChar (elektra, "newcharkey", 'c', &error);
	elektraSetOctet (elektra, "newoctetkey", 1, &error);
	elektraSetShort (elektra, "newshortkey", 1, &error);
	elektraSetUnsignedShort (elektra, "newunsignedshortkey", 1, &error);
	elektraSetLong (elektra, "newlongkey", 1, &error);
	elektraSetUnsignedLong (elektra, "newunsignedlongkey", 1, &error);
	elektraSetLongLong (elektra, "newlonglongkey", 1, &error);
	elektraSetUnsignedLongLong (elektra, "newunsignedlonglongkey", 1, &error);
	elektraSetFloat (elektra, "newfloatkey", 1.1, &error);
	elektraSetDouble (elektra, "newdoublekey", 1.1, &error);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	elektraSetLongDouble (elektra, "newlongdoublekey", 1.1L, &error);

#endif

	ASSERT_EQ (error, nullptr) << "A setter failed" << &error << std::endl;

	// Check overwritten values.
	EXPECT_STREQ (elektraGetString (elektra, "stringkey"), "A string") << "Wrong key value.";
	EXPECT_TRUE (elektraGetBoolean (elektra, "booleankey")) << "Wrong key value.";
	EXPECT_EQ (elektraGetChar (elektra, "charkey"), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGetOctet (elektra, "octetkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetShort (elektra, "shortkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShort (elektra, "unsignedshortkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLong (elektra, "longkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLong (elektra, "unsignedlongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLong (elektra, "longlongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLong (elektra, "unsignedlonglongkey"), 1) << "Wrong key value.";


	EXPECT_EQ (elektraGetFloat (elektra, "floatkey"), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetDouble (elektra, "doublekey"), 1.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraGetLongDouble (elektra, "longdoublekey"), 1.1L) << "Wrong key value.";

#endif

	// Check new keys.
	EXPECT_STREQ (elektraGetString (elektra, "newstringkey"), "A string") << "Wrong key value.";
	EXPECT_TRUE (elektraGetBoolean (elektra, "newbooleankey")) << "Wrong key value.";
	EXPECT_EQ (elektraGetChar (elektra, "newcharkey"), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGetOctet (elektra, "newoctetkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetShort (elektra, "newshortkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShort (elektra, "newunsignedshortkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLong (elektra, "newlongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLong (elektra, "newunsignedlongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLong (elektra, "newlonglongkey"), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLong (elektra, "newunsignedlonglongkey"), 1) << "Wrong key value.";

	EXPECT_EQ (elektraGetFloat (elektra, "newfloatkey"), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetDouble (elektra, "newdoublekey"), 1.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraGetLongDouble (elektra, "newlongdoublekey"), 1.1L) << "Wrong key value.";

#endif
}

TEST_F (Highlevel, ArraySetters)
{
	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#0", "");
	setKeyValue (KDB_TYPE_STRING, "stringarraykey/#1", "");

	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#0", "");
	setKeyValue (KDB_TYPE_BOOLEAN, "booleanarraykey/#1", "");

	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#0", "");
	setKeyValue (KDB_TYPE_CHAR, "chararraykey/#1", "");

	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#0", "");
	setKeyValue (KDB_TYPE_OCTET, "octetarraykey/#1", "");

	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#0", "");
	setKeyValue (KDB_TYPE_SHORT, "shortarraykey/#1", "");

	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#0", "");
	setKeyValue (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey/#1", "");

	setKeyValue (KDB_TYPE_LONG, "longarraykey/#0", "");
	setKeyValue (KDB_TYPE_LONG, "longarraykey/#1", "");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#0", "");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG, "unsignedLongarraykey/#1", "");

	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#0", "");
	setKeyValue (KDB_TYPE_LONG_LONG, "longlongarraykey/#1", "");

	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#0", "");
	setKeyValue (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey/#1", "");

	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#0", "");
	setKeyValue (KDB_TYPE_FLOAT, "floatarraykey/#1", "");

	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#0", "");
	setKeyValue (KDB_TYPE_DOUBLE, "doublearraykey/#1", "");

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#0", "");
	setKeyValue (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey/#1", "");

#endif
	createElektra ();

	ElektraError * error = nullptr;
	// Overwrite existing values.

	elektraSetStringArrayElement (elektra, "stringarraykey", 0, "String 1", &error);
	elektraSetStringArrayElement (elektra, "stringarraykey", 1, "String 2", &error);

	elektraSetBooleanArrayElement (elektra, "booleanarraykey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "booleanarraykey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "chararraykey", 0, 'c', &error);
	elektraSetCharArrayElement (elektra, "chararraykey", 1, 'd', &error);

	elektraSetOctetArrayElement (elektra, "octetarraykey", 0, 1, &error);
	elektraSetOctetArrayElement (elektra, "octetarraykey", 1, 2, &error);

	elektraSetShortArrayElement (elektra, "shortarraykey", 0, 1, &error);
	elektraSetShortArrayElement (elektra, "shortarraykey", 1, 2, &error);

	elektraSetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0, 1, &error);
	elektraSetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "longarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "longarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedLongarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "longarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "longarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1, 2, &error);

	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 0, 1, &error);
	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0, 1, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1, 2, &error);

	elektraSetFloatArrayElement (elektra, "floatarraykey", 0, 1.1, &error);
	elektraSetFloatArrayElement (elektra, "floatarraykey", 1, 2.1, &error);

	elektraSetDoubleArrayElement (elektra, "doublearraykey", 0, 1.1, &error);
	elektraSetDoubleArrayElement (elektra, "doublearraykey", 1, 2.1, &error);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 1, 2.1L, &error);

#endif

	// Add new keys.

	elektraSetStringArrayElement (elektra, "newstringarraykey", 0, "String 1", &error);
	elektraSetStringArrayElement (elektra, "newstringarraykey", 1, "String 2", &error);

	elektraSetBooleanArrayElement (elektra, "newbooleanarraykey", 0, 0, &error);
	elektraSetBooleanArrayElement (elektra, "newbooleanarraykey", 1, 1, &error);

	elektraSetCharArrayElement (elektra, "newchararraykey", 0, 'c', &error);
	elektraSetCharArrayElement (elektra, "newchararraykey", 1, 'd', &error);

	elektraSetOctetArrayElement (elektra, "newoctetarraykey", 0, 1, &error);
	elektraSetOctetArrayElement (elektra, "newoctetarraykey", 1, 2, &error);

	elektraSetShortArrayElement (elektra, "newshortarraykey", 0, 1, &error);
	elektraSetShortArrayElement (elektra, "newshortarraykey", 1, 2, &error);

	elektraSetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 0, 1, &error);
	elektraSetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "newlongarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "newlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "newunsignedLongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newunsignedLongarraykey", 1, 2, &error);

	elektraSetLongArrayElement (elektra, "newlongarraykey", 0, 1, &error);
	elektraSetLongArrayElement (elektra, "newlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 1, 2, &error);

	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 0, 1, &error);
	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 1, 2, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 0, 1, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 1, 2, &error);

	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 0, 1.1, &error);
	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 1, 2.1, &error);

	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 0, 1.1, &error);
	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 1, 2.1, &error);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1, 2.1L, &error);

#endif

	ASSERT_EQ (error, nullptr) << "A setter failed" << &error << std::endl;

	// Check overwritten values.

	EXPECT_EQ (elektraArraySize (elektra, "stringarraykey"), 2) << "Wrong array size";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "stringarraykey", 0), "String 1") << "Wrong key value.";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "stringarraykey", 1), "String 2") << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "booleanarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 0), 0) << "Wrong key value.";
	EXPECT_TRUE (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 1)) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "chararraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetCharArrayElement (elektra, "chararraykey", 0), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGetCharArrayElement (elektra, "chararraykey", 1), 'd') << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "octetarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetOctetArrayElement (elektra, "octetarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetOctetArrayElement (elektra, "octetarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "shortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "shortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "shortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedshortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "longarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongArrayElement (elektra, "longarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongArrayElement (elektra, "longarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedlongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "longlongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedlonglongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "floatarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "floatarraykey", 0), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "floatarraykey", 1), 2.1f) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "doublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0), 1.1) << "Wrong key value.";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1), 2.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraArraySize (elektra, "longdoublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0), 1.1L) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1), 2.1L) << "Wrong key value.";

#endif
	// Check new keys.

	EXPECT_EQ (elektraArraySize (elektra, "newstringarraykey"), 2) << "Wrong array size";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "newstringarraykey", 0), "String 1") << "Wrong key value.";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "newstringarraykey", 1), "String 2") << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newbooleanarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetBooleanArrayElement (elektra, "newbooleanarraykey", 0), 0) << "Wrong key value.";
	EXPECT_TRUE (elektraGetBooleanArrayElement (elektra, "newbooleanarraykey", 1)) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newchararraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetCharArrayElement (elektra, "newchararraykey", 0), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGetCharArrayElement (elektra, "newchararraykey", 1), 'd') << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newoctetarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetOctetArrayElement (elektra, "newoctetarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetOctetArrayElement (elektra, "newoctetarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newshortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "newshortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "newshortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newunsignedshortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "newunsignedshortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newlonglongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "newlonglongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "newlonglongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newunsignedlonglongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newfloatarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "newfloatarraykey", 0), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "newfloatarraykey", 1), 2.1f) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "newdoublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "newdoublearraykey", 0), 1.1) << "Wrong key value.";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "newdoublearraykey", 1), 2.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraArraySize (elektra, "newlongdoublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0), 1.1L) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1), 2.1L) << "Wrong key value.";

#endif
}

TEST_F (Highlevel, DefaultValues)
{
	ckdb::KeySet * defaults =
		ksNew (5, ckdb::keyNew ("/stringkey", KEY_VALUE, "A string", KEY_META, "type", KDB_TYPE_STRING, KEY_END), KS_END);

	createElektra (defaults);

	ElektraError * error = nullptr;

	EXPECT_STREQ (elektraGetString (elektra, "stringkey"), "A string") << "Wrong key value.";

	elektraSetString (elektra, "stringkey", "My string", &error);

	EXPECT_EQ (error, nullptr) << "elektraSet* failed" << &error << std::endl;

	EXPECT_STREQ (elektraGetString (elektra, "stringkey"), "My string") << "Wrong key value.";

	createElektra (defaults);

	ASSERT_NE (elektra, nullptr) << "elektraOpen failed" << &error << std::endl;

	EXPECT_STREQ (elektraGetString (elektra, "stringkey"), "My string") << "Wrong key value.";
	ckdb::ksDel (defaults);
}

TEST_F (Highlevel, Generic)
{
	ELEKTRA_TAG_VALUE (TEST_STRING, "stringkey", String)
	ELEKTRA_TAG_VALUE (TEST_BOOLEAN, "booleankey", Boolean)
	ELEKTRA_TAG_VALUE (TEST_CHAR, "charkey", Char)
	ELEKTRA_TAG_VALUE (TEST_OCTET, "octetkey", Octet)
	ELEKTRA_TAG_VALUE (TEST_SHORT, "shortkey", Short)
	ELEKTRA_TAG_VALUE (TEST_UNSIGNED_SHORT, "unsignedshortkey", UnsignedShort)
	ELEKTRA_TAG_VALUE (TEST_LONG, "longkey", Long)
	ELEKTRA_TAG_VALUE (TEST_UNSIGNED_LONG, "unsignedlongkey", UnsignedLong)
	ELEKTRA_TAG_VALUE (TEST_LONG_LONG, "longlongkey", LongLong)
	ELEKTRA_TAG_VALUE (TEST_UNSIGNED_LONG_LONG, "unsignedlonglongkey", UnsignedLongLong)
	ELEKTRA_TAG_VALUE (TEST_FLOAT, "floatkey", Float)
	ELEKTRA_TAG_VALUE (TEST_DOUBLE, "doublekey", Double)

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	ELEKTRA_TAG_VALUE (TEST_LONG_DOUBLE, "longdoublekey", LongDouble)

#endif

	createElektra ();

	ElektraError * error = nullptr;

	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_STRING), "A string", &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_BOOLEAN), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_CHAR), 'c', &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_OCTET), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_SHORT), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_SHORT), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG_LONG), 1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_FLOAT), 1.1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_DOUBLE), 1.1, &error);
	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_DOUBLE), 1.1, &error);

	ASSERT_EQ (error, nullptr) << "elektraSet* failed" << &error << std::endl;

	// Check values.
	EXPECT_STREQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_STRING)), "A string") << "Wrong key value.";
	EXPECT_TRUE (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_BOOLEAN))) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_CHAR)), 'c') << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_OCTET)), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_SHORT)), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_SHORT)), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_LONG)), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG)), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_LONG)), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_UNSIGNED_LONG_LONG)), 1) << "Wrong key value.";

	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_FLOAT)), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_DOUBLE)), 1.1) << "Wrong key value.";
	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_LONG_DOUBLE)), 1.1L) << "Wrong key value.";
}


typedef enum
{
	ELEKTRA_ENUM_TEST_ON = 1,
	ELEKTRA_ENUM_TEST_OFF = 0,
	ELEKTRA_ENUM_TEST_BLANK = 2
} ElektraEnumTest;

static int _elektraKeyToElektraEnumTest (const ckdb::Key * key, ElektraEnumTest * variable)
{
	kdb_long_t longVariable;
	int result = elektraKeyToLong (key, &longVariable);
	if (result)
	{
		*variable = static_cast<ElektraEnumTest> (longVariable);
	}
	return result;
}

extern "C" {
using namespace ckdb;
ELEKTRA_TAG_DECLARATIONS (ElektraEnumTest, EnumTest)
ELEKTRA_TAG_DEFINITIONS (ElektraEnumTest, EnumTest, KDB_TYPE_ENUM, elektraLongToString, _elektraKeyToElektraEnumTest)
}

TEST_F (Highlevel, Enum)
{
	createElektra ();

	ElektraError * error = nullptr;
	elektraSetEnumInt (elektra, "enumkey", ELEKTRA_ENUM_TEST_ON, &error);

	ASSERT_EQ (error, nullptr) << "elektraSet* failed" << &error << std::endl;

	EXPECT_EQ (elektraGetEnumInt (elektra, "enumkey"), static_cast<int> (ELEKTRA_ENUM_TEST_ON)) << "Wrong key value.";

	EXPECT_EQ (elektraGetEnum (elektra, "enumkey", ElektraEnumTest), ELEKTRA_ENUM_TEST_ON) << "Wrong key value.";
}

TEST_F (Highlevel, EnumArray)
{
	createElektra ();

	ElektraError * error = nullptr;

	elektraSetEnumIntArrayElement (elektra, "enumkey", 0, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 1, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 2, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 3, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetEnumIntArrayElement (elektra, "enumkey", 4, ELEKTRA_ENUM_TEST_BLANK, &error);

	ASSERT_EQ (error, nullptr) << "elektraSet* failed" << &error << std::endl;

	EXPECT_EQ (elektraGetEnumIntArrayElement (elektra, "enumkey", 0), static_cast<int> (ELEKTRA_ENUM_TEST_ON)) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumIntArrayElement (elektra, "enumkey", 1), static_cast<int> (ELEKTRA_ENUM_TEST_ON)) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumIntArrayElement (elektra, "enumkey", 2), static_cast<int> (ELEKTRA_ENUM_TEST_OFF)) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumIntArrayElement (elektra, "enumkey", 3), static_cast<int> (ELEKTRA_ENUM_TEST_OFF)) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumIntArrayElement (elektra, "enumkey", 4), static_cast<int> (ELEKTRA_ENUM_TEST_BLANK)) << "Wrong key value.";

	EXPECT_EQ (elektraGetEnumArrayElement (elektra, "enumkey", 0, ElektraEnumTest), ELEKTRA_ENUM_TEST_ON) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumArrayElement (elektra, "enumkey", 1, ElektraEnumTest), ELEKTRA_ENUM_TEST_ON) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumArrayElement (elektra, "enumkey", 2, ElektraEnumTest), ELEKTRA_ENUM_TEST_OFF) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumArrayElement (elektra, "enumkey", 3, ElektraEnumTest), ELEKTRA_ENUM_TEST_OFF) << "Wrong key value.";
	EXPECT_EQ (elektraGetEnumArrayElement (elektra, "enumkey", 4, ElektraEnumTest), ELEKTRA_ENUM_TEST_BLANK) << "Wrong key value.";
}

TEST_F (Highlevel, EnumGeneric)
{
	createElektra ();

	ElektraError * error = nullptr;

	ELEKTRA_TAG_VALUE (TEST_ENUM, "enumkey", EnumTest)

	elektraSet (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), ELEKTRA_ENUM_TEST_BLANK, &error);

	ASSERT_EQ (error, nullptr) << "elektraSet* failed" << &error << std::endl;

	EXPECT_EQ (elektraGet (elektra, ELEKTRA_TAG_NAME (TEST_ENUM)), ELEKTRA_ENUM_TEST_BLANK) << "Wrong key value.";
}

TEST_F (Highlevel, EnumArrayGeneric)
{
	createElektra ();

	ElektraError * error = nullptr;

	ELEKTRA_TAG_VALUE (TEST_ENUM, "enumkey", EnumTest)

	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 0, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 1, ELEKTRA_ENUM_TEST_ON, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 2, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 3, ELEKTRA_ENUM_TEST_OFF, &error);
	elektraSetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 4, ELEKTRA_ENUM_TEST_BLANK, &error);

	ASSERT_EQ (error, nullptr) << "elektraSet* failed" << &error << std::endl;

	EXPECT_EQ (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 0), ELEKTRA_ENUM_TEST_ON) << "Wrong key value.";
	EXPECT_EQ (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 1), ELEKTRA_ENUM_TEST_ON) << "Wrong key value.";
	EXPECT_EQ (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 2), ELEKTRA_ENUM_TEST_OFF) << "Wrong key value.";
	EXPECT_EQ (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 3), ELEKTRA_ENUM_TEST_OFF) << "Wrong key value.";
	EXPECT_EQ (elektraGetArrayElement (elektra, ELEKTRA_TAG_NAME (TEST_ENUM), 4), ELEKTRA_ENUM_TEST_BLANK) << "Wrong key value.";
}

TEST_F (Highlevel, Raw)
{
	createElektra ();

	ElektraError * error = nullptr;

	elektraSetLong (elektra, "longkey", 2, &error);
	elektraSetFloat (elektra, "floatkey", 2.25f, &error);
	elektraSetString (elektra, "stringkey", "abc", &error);
	elektraSetLongArrayElement (elektra, "longarray", 0, 15, &error);
	elektraSetLongArrayElement (elektra, "longarray", 1, 16, &error);
	elektraSetLongArrayElement (elektra, "longarray", 2, 17, &error);
	elektraSetLongArrayElement (elektra, "longarray", 3, 18, &error);

	elektraSetRawString (elektra, "rawshortkey", "2", "short", &error);
	elektraSetRawString (elektra, "rawdoublekey", "2.25", "double", &error);
	elektraSetRawString (elektra, "rawkey", "aaa", "araw", &error);
	elektraSetRawStringArrayElement (elektra, "rawlongarray", 0, "15", "long", &error);
	elektraSetRawStringArrayElement (elektra, "rawlongarray", 1, "16", "long", &error);
	elektraSetRawStringArrayElement (elektra, "rawarray", 4, "aadd", "ttt", &error);

	ASSERT_EQ (error, nullptr) << "elektraSet* failed: " << &error << std::endl;

	EXPECT_STREQ (elektraGetRawString (elektra, "longkey"), "2") << "Wrong key value.";
	EXPECT_STREQ (std::string (elektraGetRawString (elektra, "floatkey")).substr (0, 4).c_str (), "2.25") << "Wrong key value.";
	EXPECT_STREQ (elektraGetRawString (elektra, "stringkey"), "abc") << "Wrong key value.";

	EXPECT_STREQ (elektraGetRawStringArrayElement (elektra, "longarray", 0), "15") << "Wrong key value.";
	EXPECT_STREQ (elektraGetRawStringArrayElement (elektra, "longarray", 1), "16") << "Wrong key value.";
	EXPECT_STREQ (elektraGetRawStringArrayElement (elektra, "longarray", 2), "17") << "Wrong key value.";
	EXPECT_STREQ (elektraGetRawStringArrayElement (elektra, "longarray", 3), "18") << "Wrong key value.";

	EXPECT_EQ (elektraGetShort (elektra, "rawshortkey"), 2) << "Wrong key value.";

	EXPECT_EQ (elektraGetDouble (elektra, "rawdoublekey"), 2.25) << "Wrong key value.";

	EXPECT_EQ (elektraGetLongArrayElement (elektra, "rawlongarray", 0), 15) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongArrayElement (elektra, "rawlongarray", 1), 16) << "Wrong key value.";

	EXPECT_STREQ (elektraGetRawString (elektra, "rawkey"), "aaa") << "Wrong key value.";
	EXPECT_STREQ (elektraGetType (elektra, "rawkey"), "araw") << "Wrong key value.";

	EXPECT_STREQ (elektraGetArrayElementType (elektra, "rawarray", 4), "ttt") << "Wrong key value.";
	EXPECT_STREQ (elektraGetRawStringArrayElement (elektra, "rawarray", 4), "aadd") << "Wrong key value.";
}

TEST_F (Highlevel, EnforceMetadata)
{
	createElektra ();

	ElektraError * error = nullptr;

	elektraSetLong (elektra, "testkey", 2, &error);

	EXPECT_NE (elektraFindKey (elektra, "testkey", "long"), nullptr);

	try
	{
		elektraFindKey (elektra, "testkey", "string");
		ADD_FAILURE () << "expected std::runtime_error to be thrown";
	}
	catch (const std::runtime_error & err)
	{
		std::stringstream msg;
		msg << "fatal error " << ELEKTRA_ERROR_CODE_WRONG_TYPE;

		std::string errMsg = err.what ();

		auto expected = msg.str ();
		auto actual = errMsg.substr (0, expected.length ());

		EXPECT_EQ (expected, actual);
	}
	catch (...)
	{
		ADD_FAILURE () << "expected std::runtime_error to be thrown";
	}

	EXPECT_NE (elektraFindKey (elektra, "testkey", nullptr), nullptr);
}