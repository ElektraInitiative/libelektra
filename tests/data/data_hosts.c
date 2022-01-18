/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew( 28 ,
	keyNew ("user:/tests/hosts/gateway"
		, KEY_VALUE, "192.168.0.1"
	, KEY_END),
	keyNew ("user:/tests/hosts/home"
		, KEY_VALUE, "86.59.55.224"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-allhosts"
		, KEY_VALUE, "ff02::3"
		, KEY_META, "comment/#0", "All hosts for ipv6"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-allnodes"
		, KEY_VALUE, "ff02::1"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-allrouters"
		, KEY_VALUE, "ff02::2"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-localhost"
		, KEY_VALUE, "::1"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-localhost/alias00"
		, KEY_VALUE, "ost"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-localhost/alias01"
		, KEY_VALUE, "ip6-loopback"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-localnet"
		, KEY_VALUE, "fe00::0"
	, KEY_END),
	keyNew ("user:/tests/hosts/ip6-mcastprefix"
		, KEY_VALUE, "ff00::0"
	, KEY_END),
	keyNew ("user:/tests/hosts/localhost"
		, KEY_VALUE, "127.0.0.1"
	, KEY_END),
	keyNew ("user:/tests/hosts/markusbyte"
		, KEY_VALUE, "192.168.0.3"
		, KEY_META, "comment/#0", "This is my home"
	, KEY_END),
	keyNew ("user:/tests/hosts/mobilebyte.sil.at"
		, KEY_VALUE, "192.168.0.4"
	, KEY_END),
	keyNew ("user:/tests/hosts/mobilebyte.sil.at/alias00"
		, KEY_VALUE, "mobilebyte"
	, KEY_END),
	keyNew ("user:/tests/hosts/superbyte"
		, KEY_VALUE, "192.168.0.2"
	, KEY_END),
	keyNew ("user:/tests/hosts/superbyte/alias00"
		, KEY_VALUE, "printer"
	, KEY_END),
	keyNew ("user:/tests/hosts/superbyte/alias01"
		, KEY_VALUE, "laser"
	, KEY_END),
	keyNew ("user:/tests/hosts/superbyte/alias02"
		, KEY_VALUE, "news"
	, KEY_END),KS_END);
