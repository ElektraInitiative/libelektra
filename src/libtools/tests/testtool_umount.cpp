/**
 * @file
 *
 * @brief Tests for the umount
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <backends.hpp>
#include <backend.hpp>
#include <keysetio.hpp>

#include <gtest/gtest.h>

void testUmount(std::string mp)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	Backend b1;
	b1.setMountpoint(Key(mp, KEY_END), ks);
	b1.serialize(ks);

	Backends::umount(mp, ks);
	EXPECT_EQ(ks.size(), 0) << "size not null, but keyset is:\n" << ks;
}

TEST(Umount, SimpleRoot) { testUmount("/"); }
TEST(Umount, SimpleSpec) { testUmount("spec/hello"); }
TEST(Umount, SimpleDir) { testUmount("dir/hello"); }
TEST(Umount, SimpleUser) { testUmount("user/hello"); }
TEST(Umount, SimpleSystem) { testUmount("system/hello"); }
TEST(Umount, SimpleCascading) { testUmount("/hello"); }

TEST(Umount, InvolvedRoot) { testUmount("/is//../a//../complex/..///."); }

TEST(Umount, InvolvedSpec) { testUmount("spec/is///a//./more/complex/../complicated///issue//."); }
TEST(Umount, InvolvedDir) { testUmount("dir/is///a//./more/complex/../complicated///issue//."); }
TEST(Umount, InvolvedUser) { testUmount("user/is///a//./more/complex/../complicated///issue//."); }
TEST(Umount, InvolvedSystem) { testUmount("system/is///a//./more/complex/../complicated///issue//."); }
TEST(Umount, InvolvedCascading) { testUmount("/is///a//./more/complex/../complicated///issue//."); }

/**
 * @brief Test umount with compatibility umount-names
 *
 * @param mp the mountpoint to mount
 * @param ump the mountpoint to umount
 */
void testOldMount(std::string mp, std::string ump)
{
	using namespace kdb;
	using namespace kdb::tools;
	KeySet ks;

	// fake mount
	Key x("system/elektra/mountpoints", KEY_END);
	x.addBaseName(mp);
	ks.append(x.dup());
	x.addBaseName("mountpoint");
	x.setString(mp);
	ks.append(x);

	Backends::umount(ump, ks);
	EXPECT_EQ(ks.size(), 0) << "size not null, but keyset is:\n" << ks;
}

TEST(Umount, SameSpec) { testOldMount("spec/hello", "spec/hello"); }
TEST(Umount, OldSpec) { testOldMount("spec/hello", "spec_hello"); } // actually this is impossible, in 0.8.10 spec mountpoints did not exist ;)
TEST(Umount, SameUser) { testOldMount("user/hello", "user_hello"); }
TEST(Umount, SameSystem) { testOldMount("system/hello", "system_hello"); }
TEST(Umount, SimilarOldNew) { testOldMount("/hello/hello", "/hello_hello"); }
TEST(Umount, SimilarOldNew2) { testOldMount("/hello/hello", "_hello_hello"); }
TEST(Umount, SimilarOldNew3) { testOldMount("/", "_"); }
TEST(Umount, SimilarOldNew4) { testOldMount("/", "/"); }
