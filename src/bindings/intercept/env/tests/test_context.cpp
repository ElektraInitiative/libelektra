/**
 * @file
 *
 * @brief Tests for the getenv library (context part)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <kdbgetenv.h>

namespace ckdb
{
Key * elektraLookupWithContext (std::string name);
}

using namespace ckdb;

TEST (Context, Exist)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, k = keyNew ("user:/elektra/intercept/getenv/override/does-exist", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("user:/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, ExistWithContext)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, k = keyNew ("user:/elektra/intercept/getenv/override/does-exist", KEY_VALUE, "hello", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "user:/elektra/intercept/getenv/override/does-exist", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, ExistWithContextCascading)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, k = keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

namespace ckdb
{
void addLayers ();
}

TEST (Context, ExistWithContextOverrideCascading)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "layer", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingSystem)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "layer", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "systemlayer", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, ExistWithContextOverrideCascadingDir)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("dir:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "layer", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "userlayer", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingProc)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("proc:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "layer", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", KEY_VALUE, "userlayer", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingWithSlash)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("proc:/elektra/intercept/getenv/layer/layer/name", KEY_VALUE, "layer/value", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/value/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer/name%/does-exist-too", KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, NameExplicit)
{
	using namespace ckdb;
	int argc = 2;
	const char * cargv[] = { "any-name", "--elektra%name%=other-name", "--elektra%layer%=layer" };
	char ** argv = const_cast<char **> (cargv);

	elektraOpen (&argc, argv);
	ksAppendKey (elektraConfig, keyNew ("system:/somewhere/other-name/layer/does-exist", KEY_VALUE, "hello", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/elektra/intercept/getenv/override/akey", KEY_META, "context",
					    "/somewhere/%name%/%layer%/does-exist", KEY_END));
	elektraClose ();
}

TEST (Context, ExistWithContextOverrideCascadingOverrideUser)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/hostname", KEY_VALUE, "wrongname", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/layer/hostname", KEY_META, "override/#0",
					    "system:/syscall/uname/hostname", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/syscall/uname/hostname", KEY_VALUE, "localhost", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/localhost/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%hostname%/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingOverrideSystem)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("system:/elektra/intercept/getenv/layer/hostname", KEY_VALUE, "wrongname", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/layer/hostname", KEY_META, "override/#0",
					    "system:/syscall/uname/hostname", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/syscall/uname/hostname", KEY_VALUE, "localhost", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/localhost/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%hostname%/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingOverride)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/layer/hostname", KEY_META, "override/#0",
					    "system:/syscall/uname/hostname", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/syscall/uname/hostname", KEY_VALUE, "localhost", KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/localhost/does-exist-too", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", KEY_META, "context",
					    "/elektra/intercept/getenv/override/%hostname%/does-exist-too", KEY_VALUE, "hello", KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


#include "./main.cpp"
