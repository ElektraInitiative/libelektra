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

#define EXPECT_KEYVALUE(Key, Value) EXPECT_PRED2 (keyHasValue, Key, Value)
#define EXPECT_KEYVALUE_START(Key, Value) EXPECT_PRED2 (keyHasValueStart, Key, Value)
#define EXPECT_KEYMETA(Key, Meta, Value) EXPECT_PRED3 (keyHasMetaValue, Key, Meta, Value)

static inline bool keyHasValueStart (const kdb::Key & key, const std::string & value)
{
	return key && key.getString ().substr (0, value.size ()) == value;
}

static inline bool keyHasValue (const kdb::Key & key, const std::string & value)
{
	return key && key.getString () == value;
}

static inline bool keyHasMetaValue (const kdb::Key & key, const std::string & metaName, const std::string & value)
{
	return key && key.getMeta<std::string> (metaName) == value;
}

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
		closeElektra ();

		mp.reset ();
	}

	void closeElektra ()
	{
		if (elektra != nullptr)
		{
			elektraClose (elektra);
			elektra = nullptr;
		}
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
		closeElektra ();

		ElektraError * error = nullptr;
		elektra = elektraOpen (("user" + testRoot).c_str (), defaults, &error);

		ASSERT_NE (elektra, nullptr) << "elektraOpen failed" << &error << std::endl;

		elektraFatalErrorHandler (elektra, &fatalErrorHandler);
	}

	void setValues (std::initializer_list<kdb::Key> values)
	{
		using namespace kdb;
		KDB kdb;
		KeySet config;

		kdb.get (config, testRoot);
		for (auto & value : values)
		{
			config.append (value);
		}
		kdb.set (config, testRoot);
	}

	void setArrays (std::initializer_list<std::vector<kdb::Key>> arrays)
	{
		using namespace kdb;
		KDB kdb;
		KeySet config;

		kdb.get (config, testRoot);
		for (auto & array : arrays)
		{
			for (auto & value : array)
			{
				config.append (value);
			}
		}
		kdb.set (config, testRoot);
	}

	static const inline kdb::Key makeKey (KDBType type, const char * name, const char * value)
	{
		return kdb::Key ("user" + testRoot + name, KEY_VALUE, value, KEY_META, "type", type, KEY_END);
	}

	static const std::vector<kdb::Key> makeArray (KDBType type, const char * name, const std::vector<std::string> & values)
	{
		std::vector<kdb::Key> array (values.size () + 1);
		char arrayNumber[ELEKTRA_MAX_ARRAY_SIZE];
		for (size_t i = 0; i < values.size (); ++i)
		{
			ckdb::elektraWriteArrayNumber (arrayNumber, i);
			array[i + 1] = kdb::Key ("user" + testRoot + name + "/" + arrayNumber, KEY_VALUE, values[i].c_str (), KEY_META,
						 "type", type, KEY_END);
		}
		array[0] = kdb::Key ("user" + testRoot + name, KEY_META, "array", arrayNumber, KEY_END);
		return array;
	}
};

const std::string Highlevel::configFile = "kdbFileHighlevel.dump";
const std::string Highlevel::testRoot = "/tests/highlevel/"; // DO NOT use namespace here, namespace would break testing::Mountpoint

TEST_F (Highlevel, PrimitveGetters)
{
	setValues ({
		makeKey (KDB_TYPE_STRING, "stringkey", "A string"),
		makeKey (KDB_TYPE_BOOLEAN, "booleankey", "1"),
		makeKey (KDB_TYPE_CHAR, "charkey", "c"),
		makeKey (KDB_TYPE_OCTET, "octetkey", "1"),
		makeKey (KDB_TYPE_SHORT, "shortkey", "1"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "1"),
		makeKey (KDB_TYPE_LONG, "longkey", "1"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "1"),
		makeKey (KDB_TYPE_LONG_LONG, "longlongkey", "1"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "1"),
		makeKey (KDB_TYPE_FLOAT, "floatkey", "1.1"),
		makeKey (KDB_TYPE_DOUBLE, "doublekey", "1.1"),

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

		makeKey (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1"),

#endif
	});

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
	setArrays ({
		makeArray (KDB_TYPE_STRING, "stringarraykey", { "String 1", "String 2" }),
		makeArray (KDB_TYPE_BOOLEAN, "booleanarraykey", { "0", "1" }),
		makeArray (KDB_TYPE_CHAR, "chararraykey", { "c", "d" }),
		makeArray (KDB_TYPE_OCTET, "octetarraykey", { "1", "2" }),
		makeArray (KDB_TYPE_SHORT, "shortarraykey", { "1", "-1" }),
		makeArray (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey", { "1", "2" }),
		makeArray (KDB_TYPE_LONG, "longarraykey", { "1", "-1" }),
		makeArray (KDB_TYPE_UNSIGNED_LONG, "unsignedlongarraykey", { "1", "2" }),
		makeArray (KDB_TYPE_LONG_LONG, "longlongarraykey", { "1", "-1" }),
		makeArray (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey", { "1", "2" }),
		makeArray (KDB_TYPE_FLOAT, "floatarraykey", { "1.1", "-2.1" }),
		makeArray (KDB_TYPE_DOUBLE, "doublearraykey", { "1.1", "-2.1" }),

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

		makeArray (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey", { "1.1", "-2.1" }),

#endif
	});

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
	EXPECT_EQ (elektraGetShortArrayElement (elektra, "shortarraykey", 1), -1) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedshortarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedShortArrayElement (elektra, "unsignedshortarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "longarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongArrayElement (elektra, "longarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongArrayElement (elektra, "longarraykey", 1), -1) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedlongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "longlongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongLongArrayElement (elektra, "longlongarraykey", 1), -1) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "unsignedlonglongarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0), 1) << "Wrong key value.";
	EXPECT_EQ (elektraGetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1), 2) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "floatarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "floatarraykey", 0), 1.1f) << "Wrong key value.";
	EXPECT_EQ (elektraGetFloatArrayElement (elektra, "floatarraykey", 1), -2.1f) << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "doublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "doublearraykey", 0), 1.1) << "Wrong key value.";
	EXPECT_EQ (elektraGetDoubleArrayElement (elektra, "doublearraykey", 1), -2.1) << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_EQ (elektraArraySize (elektra, "longdoublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0), 1.1L) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1), -2.1L) << "Wrong key value.";

#endif
}

TEST_F (Highlevel, PrimitiveSetters)
{
	setValues ({
		makeKey (KDB_TYPE_STRING, "stringkey", "A string"),
		makeKey (KDB_TYPE_BOOLEAN, "booleankey", "1"),
		makeKey (KDB_TYPE_CHAR, "charkey", "c"),
		makeKey (KDB_TYPE_OCTET, "octetkey", "1"),
		makeKey (KDB_TYPE_SHORT, "shortkey", "1"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "1"),
		makeKey (KDB_TYPE_LONG, "longkey", "1"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "1"),
		makeKey (KDB_TYPE_LONG_LONG, "longlongkey", "1"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "1"),
		makeKey (KDB_TYPE_FLOAT, "floatkey", "1.1"),
		makeKey (KDB_TYPE_DOUBLE, "doublekey", "1.1"),

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

		makeKey (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1"),

#endif
	});

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

	closeElektra ();

	// Check overwritten values.

	using namespace kdb;
	KDB kdb;
	KeySet config;

	kdb.get (config, testRoot);
	EXPECT_KEYVALUE (config.lookup (testRoot + "stringkey"), "A string") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "booleankey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "charkey"), "c") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "octetkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "shortkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedshortkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "longkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedlongkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "longlongkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedlonglongkey"), "1") << "Wrong key value.";


	EXPECT_KEYVALUE_START (config.lookup (testRoot + "floatkey"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "doublekey"), "1.1") << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_KEYVALUE_START (config.lookup (testRoot + "longdoublekey"), "1.1") << "Wrong key value.";

#endif

	// Check new keys.
	EXPECT_KEYVALUE (config.lookup (testRoot + "newstringkey"), "A string") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newbooleankey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newcharkey"), "c") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newoctetkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newshortkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedshortkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlongkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedlongkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlonglongkey"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedlonglongkey"), "1") << "Wrong key value.";

	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newfloatkey"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newdoublekey"), "1.1") << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newlongdoublekey"), "1.1") << "Wrong key value.";

#endif
}

TEST_F (Highlevel, ArraySetters)
{
	setArrays ({
		makeArray (KDB_TYPE_STRING, "stringarraykey", { "", "" }),
		makeArray (KDB_TYPE_BOOLEAN, "booleanarraykey", { "", "" }),
		makeArray (KDB_TYPE_CHAR, "chararraykey", { "", "" }),
		makeArray (KDB_TYPE_OCTET, "octetarraykey", { "", "" }),
		makeArray (KDB_TYPE_SHORT, "shortarraykey", { "", "" }),
		makeArray (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortarraykey", { "", "" }),
		makeArray (KDB_TYPE_LONG, "longarraykey", { "", "" }),
		makeArray (KDB_TYPE_UNSIGNED_LONG, "unsignedlongarraykey", { "", "" }),
		makeArray (KDB_TYPE_LONG_LONG, "longlongarraykey", { "", "" }),
		makeArray (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongarraykey", { "", "" }),
		makeArray (KDB_TYPE_FLOAT, "floatarraykey", { "", "" }),
		makeArray (KDB_TYPE_DOUBLE, "doublearraykey", { "", "" }),

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

		makeArray (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey", { "", "" }),

#endif
	});

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

	closeElektra ();

	// Check overwritten values.

	using namespace kdb;
	KDB kdb;
	KeySet config;

	kdb.get (config, testRoot);
	EXPECT_KEYMETA (config.lookup (testRoot + "stringarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "stringarraykey/#0"), "String 1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "stringarraykey/#1"), "String 2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "booleanarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "booleanarraykey/#0"), "0") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "booleanarraykey/#1"), "1") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "chararraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "chararraykey/#0"), "c") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "chararraykey/#1"), "d") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "octetarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "octetarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "octetarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "shortarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "shortarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "shortarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "unsignedshortarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedshortarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedshortarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "longarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "longarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "longarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "unsignedlongarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedlongarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedlongarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "longlongarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "longlongarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "longlongarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "unsignedlonglongarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedlonglongarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "unsignedlonglongarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "floatarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "floatarraykey/#0"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "floatarraykey/#1"), "2.1") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "doublearraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "doublearraykey/#0"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "doublearraykey/#1"), "2.1") << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_KEYMETA (config.lookup (testRoot + "longdoublearraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "longdoublearraykey/#0"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "longdoublearraykey/#1"), "2.1") << "Wrong key value.";

#endif
	// Check new keys.

	EXPECT_KEYMETA (config.lookup (testRoot + "newstringarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newstringarraykey/#0"), "String 1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newstringarraykey/#1"), "String 2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newbooleanarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newbooleanarraykey/#0"), "0") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newbooleanarraykey/#1"), "1") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newchararraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newchararraykey/#0"), "c") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newchararraykey/#1"), "d") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newoctetarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newoctetarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newoctetarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newshortarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newshortarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newshortarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newunsignedshortarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedshortarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedshortarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newlonglongarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlonglongarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlonglongarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newunsignedlonglongarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedlonglongarraykey/#0"), "1") << "Wrong key value.";
	EXPECT_KEYVALUE (config.lookup (testRoot + "newunsignedlonglongarraykey/#1"), "2") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newfloatarraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newfloatarraykey/#0"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newfloatarraykey/#1"), "2.1") << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newdoublearraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newdoublearraykey/#0"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newdoublearraykey/#1"), "2.1") << "Wrong key value.";

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

	EXPECT_KEYMETA (config.lookup (testRoot + "newlongdoublearraykey"), "array", "#1") << "Wrong array size";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newlongdoublearraykey/#0"), "1.1") << "Wrong key value.";
	EXPECT_KEYVALUE_START (config.lookup (testRoot + "newlongdoublearraykey/#1"), "2.1") << "Wrong key value.";

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
