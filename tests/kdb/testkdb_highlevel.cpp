/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/highlevel.h>
#include <elektra/type/conversion.h>

#include <gtest/gtest-elektra.h>
#include <internal/utility/old_helper.h>

#define EXPECT_KEYVALUE(Key, Value) EXPECT_PRED2 (keyHasValue, Key, Value)
#define EXPECT_KEYMETA(Key, Meta, Value) EXPECT_PRED3 (keyHasMetaValue, Key, Meta, Value)

static inline bool keyHasValue (const kdb::Key & key, const std::string & value)
{
	return key && key.getString () == value;
}

static inline bool keyHasMetaValue (const kdb::Key & key, const std::string & metaName, const std::string & value)
{
	return key && key.getMeta<std::string> (metaName) == value;
}

/* TODO: re-add once we have a stable public error API
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
 */

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
		msg << "fatal error in test " << ::testing::UnitTest::GetInstance ()->current_test_info ()->name () << ": "
		    << elektraErrorDescription (error) << std::endl;

		elektraErrorReset (&error);

		throw std::runtime_error (msg.str ());
	}

	void createElektra (ckdb::KeySet * defaults = nullptr, ckdb::KeySet * contract = nullptr)
	{
		closeElektra ();

		ElektraError * error = nullptr;
		elektra = elektraOpen (("user:" + testRoot).c_str (), defaults, contract, &error);

		ASSERT_NE (elektra, nullptr) << "elektraOpen failed" << &error << std::endl;

		elektraFatalErrorHandler (elektra, &fatalErrorHandler);
	}

	template <class T>
	void setValues (T values)
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

	void setValues (std::initializer_list<kdb::Key> values)
	{
		setValues<std::initializer_list<kdb::Key>> (values);
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
		return kdb::Key ("user:" + testRoot + name, KEY_VALUE, value, KEY_META, "type", type, KEY_END);
	}

	static const std::vector<kdb::Key> makeArray (KDBType type, const char * name, const std::vector<std::string> & values)
	{
		std::vector<kdb::Key> array (values.size () + 1);
		char arrayNumber[ELEKTRA_MAX_ARRAY_SIZE];
		for (size_t i = 0; i < values.size (); ++i)
		{
			ckdb::elektraWriteArrayNumber (arrayNumber, i);
			array[i + 1] = kdb::Key ("user:" + testRoot + name + "/" + arrayNumber, KEY_VALUE, values[i].c_str (), KEY_META,
						 "type", type, KEY_END);
		}
		array[0] = kdb::Key ("user:" + testRoot + name, KEY_META, "array", arrayNumber, KEY_END);
		return array;
	}
};

const std::string Highlevel::configFile = "kdbFileHighlevel.dump";
const std::string Highlevel::testRoot = "/tests/highlevel/"; // DO NOT use namespace here, namespace would break testing::Mountpoint

TEST_F (Highlevel, CharTestGet)
{
	std::vector<kdb::Key> keys;
	for (int i = 0x01; i <= 0xFF; ++i)
	{
		auto c = static_cast<kdb_char_t> (i);
		char s[] = { static_cast<char> (c), '\0' };
		auto name = "char/_" + std::to_string (c);
		keys.push_back (makeKey (KDB_TYPE_CHAR, name.c_str (), s));
	}
	setValues (keys);

	createElektra ();

	for (int i = 0x01; i <= 0xFF; ++i)
	{
		auto c = static_cast<kdb_char_t> (i);
		auto name = "char/_" + std::to_string (c);
		try
		{
			EXPECT_EQ (elektraGetChar (elektra, name.c_str ()), c) << "char " + name + " wrong";
		}
		catch (std::runtime_error &)
		{
			ADD_FAILURE () << "unexpected std::runtime_error thrown for " + name;
		}
	}
}

TEST_F (Highlevel, CharTestSet)
{
	std::vector<kdb::Key> keys;
	for (int i = 0x01; i <= 0xFF; ++i)
	{
		auto c = static_cast<kdb_char_t> (i);
		auto name = "char/_" + std::to_string (c);
		keys.push_back (makeKey (KDB_TYPE_CHAR, name.c_str (), c == '_' ? "0" : "_"));
	}
	setValues (keys);

	createElektra ();

	ElektraError * error = nullptr;

	for (int i = 0x01; i <= 0xFF; ++i)
	{
		auto c = static_cast<kdb_char_t> (i);
		auto name = "char/_" + std::to_string (c);
		elektraSetChar (elektra, name.c_str (), c, &error);
		if (error != nullptr)
		{
			ADD_FAILURE () << "error for char " + name;
		}
	}

	using namespace kdb;
	KDB kdb;
	KeySet config;

	kdb.get (config, testRoot);
	for (int i = 0x01; i <= 0xFF; ++i)
	{
		auto c = static_cast<kdb_char_t> (i);
		auto name = "char/_" + std::to_string (c);
		char s[] = { static_cast<char> (c), '\0' };
		EXPECT_KEYVALUE (config.lookup (testRoot + name), s) << "Wrong key value. for char " + name;
	}
}

TEST_F (Highlevel, IntegerBordersGet)
{
	setValues ({
		makeKey (KDB_TYPE_OCTET, "octet/below", "-1"),
		makeKey (KDB_TYPE_OCTET, "octet/min", "0"),
		makeKey (KDB_TYPE_OCTET, "octet/max", "255"),
		makeKey (KDB_TYPE_OCTET, "octet/above", "256"),
		makeKey (KDB_TYPE_SHORT, "short/below", "-32769"),
		makeKey (KDB_TYPE_SHORT, "short/min", "-32768"),
		makeKey (KDB_TYPE_SHORT, "short/max", "32767"),
		makeKey (KDB_TYPE_SHORT, "short/above", "32768"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshort/below", "-1"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshort/min", "0"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshort/max", "65535"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshort/above", "65536"),
		makeKey (KDB_TYPE_LONG, "long/below", "-2147483649"),
		makeKey (KDB_TYPE_LONG, "long/min", "-2147483648"),
		makeKey (KDB_TYPE_LONG, "long/max", "2147483647"),
		makeKey (KDB_TYPE_LONG, "long/above", "2147483648"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlong/below", "-1"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlong/min", "0"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlong/max", "4294967295"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlong/above", "4294967296"),
		makeKey (KDB_TYPE_LONG_LONG, "longlong/below", "-9223372036854775809"),
		makeKey (KDB_TYPE_LONG_LONG, "longlong/min", "-9223372036854775808"),
		makeKey (KDB_TYPE_LONG_LONG, "longlong/max", "9223372036854775807"),
		makeKey (KDB_TYPE_LONG_LONG, "longlong/above", "9223372036854775808"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglong/below", "-1"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglong/min", "0"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglong/max", "18446744073709551615"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglong/above", "18446744073709551616"),
	});

	createElektra ();

	EXPECT_EQ (elektraGetOctet (elektra, "octet/min"), 0) << "octet/min wrong";
	EXPECT_EQ (elektraGetOctet (elektra, "octet/max"), 0xff) << "octet/max wrong";
	EXPECT_EQ (elektraGetShort (elektra, "short/min"), -0x8000) << "short/min wrong";
	EXPECT_EQ (elektraGetShort (elektra, "short/max"), 0x7fFF) << "short/max wrong";
	EXPECT_EQ (elektraGetUnsignedShort (elektra, "unsignedshort/min"), 0) << "unsignedshort/min wrong";
	EXPECT_EQ (elektraGetUnsignedShort (elektra, "unsignedshort/max"), 0xffFF) << "unsignedshort/max wrong";
	EXPECT_EQ (elektraGetLong (elektra, "long/min"), -0x80000000) << "long/min wrong";
	EXPECT_EQ (elektraGetLong (elektra, "long/max"), 0x7fFFffFF) << "long/max wrong";
	EXPECT_EQ (elektraGetUnsignedLong (elektra, "unsignedlong/min"), 0) << "unsignedlong/min wrong";
	EXPECT_EQ (elektraGetUnsignedLong (elektra, "unsignedlong/max"), 0xffFFffFF) << "unsignedlong/max wrong";
	EXPECT_EQ (elektraGetLongLong (elektra, "longlong/min"), -0x8000000000000000) << "longlong/min wrong";
	EXPECT_EQ (elektraGetLongLong (elektra, "longlong/max"), 0x7fFFffFFffFFffFF) << "longlong/max wrong";
	EXPECT_EQ (elektraGetUnsignedLongLong (elektra, "unsignedlonglong/min"), 0) << "unsignedlonglong/min wrong";
	EXPECT_EQ (elektraGetUnsignedLongLong (elektra, "unsignedlonglong/max"), 0xffFFffFFffFFffFF) << "unsignedlonglong/max wrong";

	EXPECT_THROW (elektraGetOctet (elektra, "octet/below"), std::runtime_error) << "octet/below accepted";
	EXPECT_THROW (elektraGetOctet (elektra, "octet/above"), std::runtime_error) << "octet/above accepted";
	EXPECT_THROW (elektraGetShort (elektra, "short/below"), std::runtime_error) << "short/below wrong";
	EXPECT_THROW (elektraGetShort (elektra, "short/above"), std::runtime_error) << "short/above wrong";
	EXPECT_THROW (elektraGetUnsignedShort (elektra, "unsignedshort/below"), std::runtime_error) << "unsignedshort/below wrong";
	EXPECT_THROW (elektraGetUnsignedShort (elektra, "unsignedshort/above"), std::runtime_error) << "unsignedshort/above wrong";
	EXPECT_THROW (elektraGetLong (elektra, "long/below"), std::runtime_error) << "long/below wrong";
	EXPECT_THROW (elektraGetLong (elektra, "long/above"), std::runtime_error) << "long/above wrong";
	EXPECT_THROW (elektraGetUnsignedLong (elektra, "unsignedlong/below"), std::runtime_error) << "unsignedlong/below wrong";
	EXPECT_THROW (elektraGetUnsignedLong (elektra, "unsignedlong/above"), std::runtime_error) << "unsignedlong/above wrong";
	EXPECT_THROW (elektraGetLongLong (elektra, "longlong/below"), std::runtime_error) << "longlong/below wrong";
	EXPECT_THROW (elektraGetLongLong (elektra, "longlong/above"), std::runtime_error) << "longlong/above wrong";
	EXPECT_THROW (elektraGetUnsignedLongLong (elektra, "unsignedlonglong/below"), std::runtime_error) << "unsignedlonglong/below wrong";
	EXPECT_THROW (elektraGetUnsignedLongLong (elektra, "unsignedlonglong/above"), std::runtime_error) << "unsignedlonglong/above wrong";
}

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
		makeKey (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "1.1"),
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
	EXPECT_EQ (elektraGetLongDouble (elektra, "longdoublekey"), 1.1L) << "Wrong key value.";
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
		makeArray (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey", { "1.1", "-2.1" }),
	});

	createElektra ();


	EXPECT_EQ (elektraArraySize (elektra, "stringarraykey"), 2) << "Wrong array size";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "stringarraykey", 0), "String 1") << "Wrong key value.";
	EXPECT_STREQ (elektraGetStringArrayElement (elektra, "stringarraykey", 1), "String 2") << "Wrong key value.";

	EXPECT_EQ (elektraArraySize (elektra, "booleanarraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 0), false) << "Wrong key value.";
	EXPECT_EQ (elektraGetBooleanArrayElement (elektra, "booleanarraykey", 1), true) << "Wrong key value.";

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

	EXPECT_EQ (elektraArraySize (elektra, "longdoublearraykey"), 2) << "Wrong array size";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 0), 1.1L) << "Wrong key value.";
	EXPECT_EQ (elektraGetLongDoubleArrayElement (elektra, "longdoublearraykey", 1), -2.1L) << "Wrong key value.";
}

TEST_F (Highlevel, PrimitiveSetters)
{
	setValues ({
		makeKey (KDB_TYPE_STRING, "stringkey", "An original string"),
		makeKey (KDB_TYPE_BOOLEAN, "booleankey", "0"),
		makeKey (KDB_TYPE_CHAR, "charkey", "x"),
		makeKey (KDB_TYPE_OCTET, "octetkey", "0"),
		makeKey (KDB_TYPE_SHORT, "shortkey", "0"),
		makeKey (KDB_TYPE_UNSIGNED_SHORT, "unsignedshortkey", "0"),
		makeKey (KDB_TYPE_LONG, "longkey", "0"),
		makeKey (KDB_TYPE_UNSIGNED_LONG, "unsignedlongkey", "0"),
		makeKey (KDB_TYPE_LONG_LONG, "longlongkey", "0"),
		makeKey (KDB_TYPE_UNSIGNED_LONG_LONG, "unsignedlonglongkey", "0"),
		makeKey (KDB_TYPE_FLOAT, "floatkey", "0.1"),
		makeKey (KDB_TYPE_DOUBLE, "doublekey", "0.1"),
		makeKey (KDB_TYPE_LONG_DOUBLE, "longdoublekey", "0.1"),
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
	elektraSetFloat (elektra, "floatkey", 1.1f, &error);
	elektraSetDouble (elektra, "doublekey", 1.1, &error);

	elektraSetLongDouble (elektra, "longdoublekey", 1.1L, &error);

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
	elektraSetFloat (elektra, "newfloatkey", 1.1f, &error);
	elektraSetDouble (elektra, "newdoublekey", 1.1, &error);

	elektraSetLongDouble (elektra, "newlongdoublekey", 1.1L, &error);

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


	std::stringstream ss;
	ss << std::setprecision (9) << 1.1f;
	EXPECT_KEYVALUE (config.lookup (testRoot + "floatkey"), ss.str ()) << "Wrong key value.";

	ss.str ("");
	ss << std::setprecision (17) << 1.1;
	EXPECT_KEYVALUE (config.lookup (testRoot + "doublekey"), ss.str ()) << "Wrong key value.";

	ss.str ("");
	ss << std::setprecision (21) << 1.1L;
	EXPECT_KEYVALUE (config.lookup (testRoot + "longdoublekey"), ss.str ()) << "Wrong key value.";

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

	ss.str ("");
	ss << std::setprecision (9) << 1.1f;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newfloatkey"), ss.str ()) << "Wrong key value.";

	ss.str ("");
	ss << std::setprecision (17) << 1.1;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newdoublekey"), ss.str ()) << "Wrong key value.";

	ss.str ("");
	ss << std::setprecision (21) << 1.1L;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlongdoublekey"), ss.str ()) << "Wrong key value.";
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
		makeArray (KDB_TYPE_LONG_DOUBLE, "longdoublearraykey", { "", "" }),
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

	elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "unsignedlongarraykey", 1, 2, &error);

	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 0, 1, &error);
	elektraSetLongLongArrayElement (elektra, "longlongarraykey", 1, 2, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 0, 1, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "unsignedlonglongarraykey", 1, 2, &error);

	elektraSetFloatArrayElement (elektra, "floatarraykey", 0, 1.1f, &error);
	elektraSetFloatArrayElement (elektra, "floatarraykey", 1, 2.1f, &error);

	elektraSetDoubleArrayElement (elektra, "doublearraykey", 0, 1.1, &error);
	elektraSetDoubleArrayElement (elektra, "doublearraykey", 1, 2.1, &error);

	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "longdoublearraykey", 1, 2.1L, &error);

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

	elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 0, 1, &error);
	elektraSetUnsignedLongArrayElement (elektra, "newunsignedlongarraykey", 1, 2, &error);

	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 0, 1, &error);
	elektraSetLongLongArrayElement (elektra, "newlonglongarraykey", 1, 2, &error);

	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 0, 1, &error);
	elektraSetUnsignedLongLongArrayElement (elektra, "newunsignedlonglongarraykey", 1, 2, &error);

	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 0, 1.1f, &error);
	elektraSetFloatArrayElement (elektra, "newfloatarraykey", 1, 2.1f, &error);

	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 0, 1.1, &error);
	elektraSetDoubleArrayElement (elektra, "newdoublearraykey", 1, 2.1, &error);

	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 0, 1.1L, &error);
	elektraSetLongDoubleArrayElement (elektra, "newlongdoublearraykey", 1, 2.1L, &error);

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
	std::stringstream ss;
	ss << std::setprecision (9) << 1.1f;
	EXPECT_KEYVALUE (config.lookup (testRoot + "floatarraykey/#0"), ss.str ()) << "Wrong key value.";
	ss.str ("");
	ss << std::setprecision (9) << 2.1f;
	EXPECT_KEYVALUE (config.lookup (testRoot + "floatarraykey/#1"), ss.str ()) << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "doublearraykey"), "array", "#1") << "Wrong array size";
	ss.str ("");
	ss << std::setprecision (17) << 1.1;
	EXPECT_KEYVALUE (config.lookup (testRoot + "doublearraykey/#0"), ss.str ()) << "Wrong key value.";
	ss.str ("");
	ss << std::setprecision (17) << 2.1;
	EXPECT_KEYVALUE (config.lookup (testRoot + "doublearraykey/#1"), ss.str ()) << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "longdoublearraykey"), "array", "#1") << "Wrong array size";
	ss.str ("");
	ss << std::setprecision (21) << 1.1L;
	EXPECT_KEYVALUE (config.lookup (testRoot + "longdoublearraykey/#0"), ss.str ()) << "Wrong key value.";
	ss.str ("");
	ss << std::setprecision (21) << 2.1L;
	EXPECT_KEYVALUE (config.lookup (testRoot + "longdoublearraykey/#1"), ss.str ()) << "Wrong key value.";

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
	ss.str ("");
	ss << std::setprecision (9) << 1.1f;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newfloatarraykey/#0"), ss.str ()) << "Wrong key value.";
	ss.str ("");
	ss << std::setprecision (9) << 2.1f;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newfloatarraykey/#1"), ss.str ()) << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newdoublearraykey"), "array", "#1") << "Wrong array size";
	ss.str ("");
	ss << std::setprecision (17) << 1.1;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newdoublearraykey/#0"), ss.str ()) << "Wrong key value.";
	ss.str ("");
	ss << std::setprecision (17) << 2.1;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newdoublearraykey/#1"), ss.str ()) << "Wrong key value.";

	EXPECT_KEYMETA (config.lookup (testRoot + "newlongdoublearraykey"), "array", "#1") << "Wrong array size";
	ss.str ("");
	ss << std::setprecision (21) << 1.1L;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlongdoublearraykey/#0"), ss.str ()) << "Wrong key value.";
	ss.str ("");
	ss << std::setprecision (21) << 2.1L;
	EXPECT_KEYVALUE (config.lookup (testRoot + "newlongdoublearraykey/#1"), ss.str ()) << "Wrong key value.";
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

	EXPECT_THROW (elektraFindKey (elektra, "testkey", "string"), std::runtime_error);
	// TODO: check error code once error API is public and stable

	EXPECT_NE (elektraFindKey (elektra, "testkey", nullptr), nullptr);
}
