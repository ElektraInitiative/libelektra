/**
 * @file
 *
 * @brief Tests for the umount
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <backends.hpp>
#include <keysetio.hpp>

#include <gtest/gtest.h>

void testUmount (std::string mp)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	b1.setMountpoint (Key (mp, KEY_END), ks);
	b1.serialize (ks);

	Backends::umount (mp, ks);
	EXPECT_EQ (ks.size (), 0) << "size not null, but keyset is:\n" << ks;
}

TEST (Umount, SimpleRoot)
{
	testUmount ("/");
}
TEST (Umount, SimpleSpec)
{
	testUmount ("spec:/hello");
}
TEST (Umount, SimpleDir)
{
	testUmount ("dir:/hello");
}
TEST (Umount, SimpleUser)
{
	testUmount ("user:/hello");
}
TEST (Umount, SimpleSystem)
{
	testUmount ("system:/hello");
}
TEST (Umount, SimpleCascading)
{
	testUmount ("/hello");
}

TEST (Umount, InvolvedRoot)
{
	testUmount ("/is//../a//../complex/..///.");
}

TEST (Umount, InvolvedSpec)
{
	testUmount ("spec:/is///a//./more/complex/../complicated///issue//.");
}
TEST (Umount, InvolvedDir)
{
	testUmount ("dir:/is///a//./more/complex/../complicated///issue//.");
}
TEST (Umount, InvolvedUser)
{
	testUmount ("user:/is///a//./more/complex/../complicated///issue//.");
}
TEST (Umount, InvolvedSystem)
{
	testUmount ("system:/is///a//./more/complex/../complicated///issue//.");
}
TEST (Umount, InvolvedCascading)
{
	testUmount ("/is///a//./more/complex/../complicated///issue//.");
}
