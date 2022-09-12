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
		, ELEKTRA_KEY_VALUE, "192.168.0.1"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/home"
		, ELEKTRA_KEY_VALUE, "86.59.55.224"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-allhosts"
		, ELEKTRA_KEY_VALUE, "ff02::3"
		, ELEKTRA_KEY_COMMENT, "All hosts for ipv6"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-allnodes"
		, ELEKTRA_KEY_VALUE, "ff02::1"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-allrouters"
		, ELEKTRA_KEY_VALUE, "ff02::2"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-localhost"
		, ELEKTRA_KEY_VALUE, "::1"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-localhost/alias00"
		, ELEKTRA_KEY_VALUE, "ost"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-localhost/alias01"
		, ELEKTRA_KEY_VALUE, "ip6-loopback"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-localnet"
		, ELEKTRA_KEY_VALUE, "fe00::0"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/ip6-mcastprefix"
		, ELEKTRA_KEY_VALUE, "ff00::0"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/localhost"
		, ELEKTRA_KEY_VALUE, "127.0.0.1"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/markusbyte"
		, ELEKTRA_KEY_VALUE, "192.168.0.3"
		, ELEKTRA_KEY_COMMENT, "This is my home"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/mobilebyte.sil.at"
		, ELEKTRA_KEY_VALUE, "192.168.0.4"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/mobilebyte.sil.at/alias00"
		, ELEKTRA_KEY_VALUE, "mobilebyte"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/superbyte"
		, ELEKTRA_KEY_VALUE, "192.168.0.2"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/superbyte/alias00"
		, ELEKTRA_KEY_VALUE, "printer"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/superbyte/alias01"
		, ELEKTRA_KEY_VALUE, "laser"
	, ELEKTRA_KEY_END),
	keyNew ("user:/tests/hosts/superbyte/alias02"
		, ELEKTRA_KEY_VALUE, "news"
	, ELEKTRA_KEY_END),ELEKTRA_KS_END);
