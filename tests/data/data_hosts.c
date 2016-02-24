/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

// clang-format off

ksNew( 28 ,
	keyNew ("user/tests/hosts/gateway"
		, KEY_VALUE, "192.168.0.1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/home"
		, KEY_VALUE, "86.59.55.224"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-allhosts"
		, KEY_VALUE, "ff02::3"
		, KEY_COMMENT, "All hosts for ipv6"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-allnodes"
		, KEY_VALUE, "ff02::1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-allrouters"
		, KEY_VALUE, "ff02::2"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-localhost"
		, KEY_DIR
		, KEY_VALUE, "::1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-localhost/alias00"
		, KEY_VALUE, "ost"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-localhost/alias01"
		, KEY_VALUE, "ip6-loopback"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-localnet"
		, KEY_VALUE, "fe00::0"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/ip6-mcastprefix"
		, KEY_VALUE, "ff00::0"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/localhost"
		, KEY_VALUE, "127.0.0.1"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/markusbyte"
		, KEY_VALUE, "192.168.0.3"
		, KEY_COMMENT, "This is my home"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/mobilebyte.sil.at"
		, KEY_DIR
		, KEY_VALUE, "192.168.0.4"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/mobilebyte.sil.at/alias00"
		, KEY_VALUE, "mobilebyte"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/superbyte"
		, KEY_DIR
		, KEY_VALUE, "192.168.0.2"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/superbyte/alias00"
		, KEY_VALUE, "printer"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/superbyte/alias01"
		, KEY_VALUE, "laser"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),
	keyNew ("user/tests/hosts/superbyte/alias02"
		, KEY_VALUE, "news"
		, KEY_TYPE, KEY_TYPE_STRING
	, KEY_END),KS_END);
