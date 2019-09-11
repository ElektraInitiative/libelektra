// clang-format off

CppKeySet { 10,
	    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_META, "array", "#1", KEY_END),
	    keyNew (PREFIX "#0/bla", KEY_VALUE, "blubb", KEY_END),
	    keyNew (PREFIX "#1", KEY_META, "array", "#0", KEY_END),
	    keyNew (PREFIX "#1/#0", KEY_VALUE, "hello", KEY_END),
	    KS_END }
