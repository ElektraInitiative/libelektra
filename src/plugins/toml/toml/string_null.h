// clang-format off
ksNew (16,
	keyNew (PREFIX "/null_value_literal",
		KEY_BINARY,
		KEY_SIZE, 34, 
		KEY_VALUE, NULL,
		KEY_META, "order", "0",
		KEY_META, "tomltype", "string_literal",
		KEY_META, "binary", "",
	KEY_END),
	keyNew (PREFIX "/null_value_basic",
		KEY_VALUE, NULL,
		KEY_META, "order", "1",
		KEY_META, "tomltype", "string_basic",
		KEY_META, "binary", "",
	KEY_END),
KS_END)
