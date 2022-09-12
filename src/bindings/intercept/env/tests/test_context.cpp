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
	ksAppendKey (elektraConfig, k = keyNew ("user:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("user:/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, ExistWithContext)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, k = keyNew ("user:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "user:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, ExistWithContextCascading)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, k = keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
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
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "layer", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", ELEKTRA_KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingSystem)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "layer", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "systemlayer", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", ELEKTRA_KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


TEST (Context, ExistWithContextOverrideCascadingDir)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("dir:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "layer", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "userlayer", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", ELEKTRA_KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingProc)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("proc:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "layer", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/layer", ELEKTRA_KEY_VALUE, "userlayer", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer%/does-exist-too", ELEKTRA_KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingWithSlash)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("proc:/elektra/intercept/getenv/layer/layer/name", ELEKTRA_KEY_VALUE, "layer/value", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/layer/value/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%layer/name%/does-exist-too", ELEKTRA_KEY_META, "override/#0",
					    "/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
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
	ksAppendKey (elektraConfig, keyNew ("system:/somewhere/other-name/layer/does-exist", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/elektra/intercept/getenv/override/akey", ELEKTRA_KEY_META, "context",
					    "/somewhere/%name%/%layer%/does-exist", ELEKTRA_KEY_END));
	elektraClose ();
}

TEST (Context, ExistWithContextOverrideCascadingOverrideUser)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/layer/hostname", ELEKTRA_KEY_VALUE, "wrongname", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/layer/hostname", ELEKTRA_KEY_META, "override/#0",
					    "system:/syscall/uname/hostname", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/syscall/uname/hostname", ELEKTRA_KEY_VALUE, "localhost", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/localhost/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%hostname%/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingOverrideSystem)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("system:/elektra/intercept/getenv/layer/hostname", ELEKTRA_KEY_VALUE, "wrongname", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/layer/hostname", ELEKTRA_KEY_META, "override/#0",
					    "system:/syscall/uname/hostname", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/syscall/uname/hostname", ELEKTRA_KEY_VALUE, "localhost", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/localhost/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%hostname%/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}

TEST (Context, ExistWithContextOverrideCascadingOverride)
{
	Key * k;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/layer/hostname", ELEKTRA_KEY_META, "override/#0",
					    "system:/syscall/uname/hostname", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("system:/syscall/uname/hostname", ELEKTRA_KEY_VALUE, "localhost", ELEKTRA_KEY_END));
	addLayers ();

	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist-too", ELEKTRA_KEY_VALUE, "wrong", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig,
		     k = keyNew ("user:/elektra/intercept/getenv/override/localhost/does-exist-too", ELEKTRA_KEY_VALUE, "correct", ELEKTRA_KEY_END));
	ksAppendKey (elektraConfig, keyNew ("spec:/elektra/intercept/getenv/override/does-exist", ELEKTRA_KEY_META, "context",
					    "/elektra/intercept/getenv/override/%hostname%/does-exist-too", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END));
	Key * f = elektraLookupWithContext ("/elektra/intercept/getenv/override/does-exist");
	EXPECT_EQ (k, f);
}


#include "main.cpp"
