/**
 * @file
 *
 * @brief Tests for email plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <tests_plugin.h>

#include <internal/config.h>

static void testEmail (char const * const email, const int ret)
{
	Key * parentKey = keyNew ("user:/tests/email", KEY_VALUE, "", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (10, keyNew ("user:/tests/email/totest", KEY_VALUE, email, KEY_META, "check/email", "", KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	const int pluginStatus = plugin->kdbSet (plugin, ks, parentKey);
	char message[300];
	(void) snprintf (message, 300, "validation of email address “%s” returned %d instead of %d", email, pluginStatus, ret);
	succeed_if (pluginStatus == ret, message);
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testEmailAll (void)
{
	// valid email addresses
	testEmail ("simple@example.com", 1);
	testEmail ("very.common@example.com", 1);
	testEmail ("disposable.style.email.with+symbol@example.com", 1);
	testEmail ("other.email-with-hyphen@example.com", 1);
	testEmail ("fully-qualified-domain@example.com", 1);
	testEmail ("user.name+tag+sorting@example.com", 1);
	testEmail ("x@example.com", 1);
	testEmail ("example-indeed@strange-example.com", 1);
	testEmail ("test/test@test.com", 1);
	testEmail ("admin@mailserver1", 1);
	testEmail ("example@s.example", 1);
	testEmail ("mailhost!username@example.org", 1);
	testEmail ("user%example.com@example.org", 1);
	testEmail ("user-@example.org", 1);
	testEmail ("hi@elektra.io", 1);
	testEmail ("hi+there@elektra.io", 1);
	testEmail ("elektra.k@elektra.io", 1);
	testEmail ("ast*risk@elektra.io", 1);
	testEmail ("#$%!^/&@elektra.io", 1);
	testEmail ("elektra@io", 1);


	// invalid email addresses
	testEmail ("notAn@address-", -1);
	testEmail ("A@b@c@example.com", -1);
	testEmail ("a\"b(c)d,e:f;g<h>i[j\\k]l@example.com", -1);
	testEmail ("just\"not\"right@example.com", -1);
	testEmail ("this is\"not\\allowed@example.com", -1);
	testEmail ("this\\ still\\\"not\\allowed@example.com", -1);
	testEmail ("i_like_underscore@but_its_not_allowed_in_this_part.example.com", -1);
	testEmail ("QA[icon]CHOCOLATE[icon]@test.com", -1);
	testEmail ("hi@", -1);
	testEmail ("elektra.@elektra.io", -1);
	testEmail ("elektra..k@elektra.io", -1);
	testEmail ("!#$%&'*(-/=?@elektra.io", -1);
	testEmail ("h(a)i@elektra.io", -1);
	testEmail ("em@il@elektra.io", -1);
	testEmail ("\"<\\\"@\\\".!#%$@elektra.io", -1);
	testEmail ("<\\\"@\\\\\".!#%$@elektra.io", -1);
	testEmail ("hi\"@\"you@elektra.io", -1);
	testEmail ("hi\\ there@elektra.io", -1);


	// false positives because of missing top level domain and length check
	testEmail ("1@23456789", 1);
	testEmail ("valid+part@invalidtopleveldomain", 1);
	testEmail ("1234567890123456789012345678901234567890123456789012345678901234+x@example.com", 1);

	// valid but not supported email address formats:
	testEmail ("\" \"@example.org", -1);
	testEmail ("\"john..doe\"@example.org", -1);
	testEmail ("(ele)ktra@elektra.io", -1);
	testEmail ("elektra@elektra.io(io)", -1);
	testEmail ("\"hi@you\"@elektra.io", -1);
	testEmail ("\"hi you\"@elektra.io", -1);
	testEmail ("\" \"@elektra.io", -1);
	testEmail ("\"<\\\"@\\\\\".!.#%$@@elektra.io", -1);
	testEmail ("cow@[dead::beef]", -1);
	testEmail ("是是@是是.是是", -1);
	testEmail ("1@[23456789]", -1);
}
