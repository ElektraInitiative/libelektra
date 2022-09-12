/**
 * @file
 *
 * @brief Tests for the Backend class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <keysetio.hpp>

#include <gtest/gtest.h>

TEST (SameMountpoint, setMountpoints)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	b1.setMountpoint (Key ("user:/hello", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");

	b1.setMountpoint (Key ("user://hello", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");

	b1.setMountpoint (Key ("user:////hello", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");

	b1.setMountpoint (Key ("user:////hello//", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");

	b1.setMountpoint (Key ("user:////hello////.", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");

	b1.setMountpoint (Key ("user:////hello////.//", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");

	b1.setMountpoint (Key ("user:////hello////.//x/..", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");
}

TEST (SameMountpoint, setMountpointsNamespaces)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;

	b1.setMountpoint (Key ("dir:/", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "dir:/");

	b1.setMountpoint (Key ("system:/", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "system:/");

	b1.setMountpoint (Key ("user:/", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/");

	b1.setMountpoint (Key ("spec:/", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "spec:/");
}

TEST (SameMountpoint, strangeMountpoints)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	b1.setMountpoint (Key ("user:/elektras", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/elektras");

	b1.setMountpoint (Key ("user:/elektra/..", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/");

	b1.setMountpoint (Key ("user:/elektra\\/", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/elektra\\/");

	b1.setMountpoint (Key ("/is//../a//../complex/..///.", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "/");
}

TEST (SameMountpoint, wrongMountpoints)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	ASSERT_THROW (b1.setMountpoint (Key (static_cast<ckdb::Key *> (nullptr)), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("proc:/", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("proc:/something", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
}

TEST (SameMountpoint, wrongElektraMountpoints)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	ASSERT_THROW (b1.setMountpoint (Key ("proc:/", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("proc:/elektra", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("spec:/elektra", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("dir:/elektra", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("system:/elektra", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("system:/elektra/mountpoints", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("system:/elektra/globalplugins", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("system:/elektra/something", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("system:/elektra/something/deep/below", ELEKTRA_KEY_END), ks),
		      kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("user:/elektra", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("user:/elektra/mountpoints", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("user:/elektra/globalplugins", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("user:/elektra/something", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("user:/elektra/something/deep/below", ELEKTRA_KEY_END), ks),
		      kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("/elektra", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("/elektra/mountpoints", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("/elektra/globalplugins", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("/elektra/something", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
	ASSERT_THROW (b1.setMountpoint (Key ("/elektra/something/deep/below", ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);
	EXPECT_EQ (b1.getMountpoint (), "");
}


TEST (SameMountpoint, notSame)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	b1.setMountpoint (Key ("user:/hello", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b1.getMountpoint (), "user:/hello");
	b1.serialize (ks);

	Backend b2;
	b2.setMountpoint (Key ("user:/else", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b2.getMountpoint (), "user:/else");
	b2.serialize (ks);

	Backend b3;
	b2.setMountpoint (Key ("/somewhere", ELEKTRA_KEY_END), ks);
	EXPECT_EQ (b2.getMountpoint (), "/somewhere");
	b2.serialize (ks);
}

#define checkSame(name1, name2)                                                                                                            \
	{                                                                                                                                  \
		using namespace kdb;                                                                                                       \
		using namespace kdb::tools;                                                                                                \
		KeySet ks;                                                                                                                 \
                                                                                                                                           \
		Backend b1;                                                                                                                \
		EXPECT_EQ (b1.getMountpoint (), "");                                                                                       \
		b1.setMountpoint (Key (name1, ELEKTRA_KEY_END), ks);                                                                               \
		EXPECT_EQ (b1.getMountpoint (), name1);                                                                                    \
		b1.serialize (ks);                                                                                                         \
                                                                                                                                           \
		Backend b2;                                                                                                                \
		EXPECT_EQ (b2.getMountpoint (), "");                                                                                       \
		ASSERT_THROW (b2.setMountpoint (Key (name2, ELEKTRA_KEY_END), ks), kdb::tools::MountpointAlreadyInUseException);                   \
		EXPECT_EQ (b2.getMountpoint (), "");                                                                                       \
	}

#define checkAllow(name1, name2)                                                                                                           \
	{                                                                                                                                  \
		using namespace kdb;                                                                                                       \
		using namespace kdb::tools;                                                                                                \
		KeySet ks;                                                                                                                 \
                                                                                                                                           \
		Backend b1;                                                                                                                \
		EXPECT_EQ (b1.getMountpoint (), "");                                                                                       \
		b1.setMountpoint (Key (name1, ELEKTRA_KEY_END), ks);                                                                               \
		EXPECT_EQ (b1.getMountpoint (), name1);                                                                                    \
		b1.serialize (ks);                                                                                                         \
                                                                                                                                           \
		Backend b2;                                                                                                                \
		EXPECT_EQ (b2.getMountpoint (), "");                                                                                       \
		ASSERT_NO_THROW (b2.setMountpoint (Key (name2, ELEKTRA_KEY_END), ks));                                                             \
		EXPECT_EQ (b2.getMountpoint (), name2);                                                                                    \
	}


TEST (SameMountpoint, exactlySameHelloSpec)
{
	checkSame ("spec:/hello", "spec:/hello");
}
TEST (SameMountpoint, exactlySameHelloDir)
{
	checkSame ("dir:/hello", "dir:/hello");
}
TEST (SameMountpoint, exactlySameHelloUser)
{
	checkSame ("user:/hello", "user:/hello");
}
TEST (SameMountpoint, exactlySameHelloSystem)
{
	checkSame ("system:/hello", "system:/hello");
}

TEST (SameMountpoint, cascadingFirstSameHelloSpec)
{
	checkAllow ("/hello", "spec:/hello");
}
TEST (SameMountpoint, cascadingFirstSameHelloUser)
{
	checkSame ("/hello", "user:/hello");
}
TEST (SameMountpoint, cascadingFirstSameHelloSystem)
{
	checkSame ("/hello", "system:/hello");
}

TEST (SameMountpoint, cascadingSameHelloSpec)
{
	checkAllow ("spec:/hello", "/hello");
}
TEST (SameMountpoint, cascadingSameHelloDir)
{
	checkSame ("dir:/hello", "/hello");
}
TEST (SameMountpoint, cascadingSameHelloUser)
{
	checkSame ("user:/hello", "/hello");
}
TEST (SameMountpoint, cascadingSameHelloSystem)
{
	checkSame ("system:/hello", "/hello");
}

TEST (SameMountpoint, exactlySameSpec)
{
	checkSame ("spec:/an/more/involved/deeper/mountpoint", "spec:/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, exactlySameDir)
{
	checkSame ("dir:/an/more/involved/deeper/mountpoint", "dir:/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, exactlySameUser)
{
	checkSame ("user:/an/more/involved/deeper/mountpoint", "user:/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, exactlySameSystem)
{
	checkSame ("system:/an/more/involved/deeper/mountpoint", "system:/an/more/involved/deeper/mountpoint");
}

TEST (SameMountpoint, cascadingFirstSameSpec)
{
	checkAllow ("/an/more/involved/deeper/mountpoint", "spec:/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, cascadingFirstSameDir)
{
	checkSame ("/an/more/involved/deeper/mountpoint", "dir:/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, cascadingFirstSameUser)
{
	checkSame ("/an/more/involved/deeper/mountpoint", "user:/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, cascadingFirstSameSystem)
{
	checkSame ("/an/more/involved/deeper/mountpoint", "system:/an/more/involved/deeper/mountpoint");
}

TEST (SameMountpoint, cascadingSameSpec)
{
	checkAllow ("spec:/an/more/involved/deeper/mountpoint", "/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, cascadingSameDir)
{
	checkSame ("dir:/an/more/involved/deeper/mountpoint", "/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, cascadingSameUser)
{
	checkSame ("user:/an/more/involved/deeper/mountpoint", "/an/more/involved/deeper/mountpoint");
}
TEST (SameMountpoint, cascadingSameSystem)
{
	checkSame ("system:/an/more/involved/deeper/mountpoint", "/an/more/involved/deeper/mountpoint");
}

TEST (SameMountpoint, sameRoot)
{
	checkSame ("/", "/");
}
TEST (SameMountpoint, sameRootSpec)
{
	checkSame ("/", "spec:/");
}
TEST (SameMountpoint, sameRootDir)
{
	checkSame ("/", "dir:/");
}
TEST (SameMountpoint, sameRootUser)
{
	checkSame ("/", "user:/");
}
TEST (SameMountpoint, sameRootSystem)
{
	checkSame ("/", "system:/");
}
