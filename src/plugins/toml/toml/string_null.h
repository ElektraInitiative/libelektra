// clang-format off
ksNew (16,
	keyNew (PREFIX "/null_value_literal",
		ELEKTRA_KEY_BINARY,
		ELEKTRA_KEY_SIZE, 34, 
		ELEKTRA_KEY_VALUE, NULL,
		ELEKTRA_KEY_META, "order", "0",
		ELEKTRA_KEY_META, "tomltype", "string_literal",
		ELEKTRA_KEY_META, "binary", "",
	ELEKTRA_KEY_END),
	keyNew (PREFIX "/null_value_basic",
		ELEKTRA_KEY_VALUE, NULL,
		ELEKTRA_KEY_META, "order", "1",
		ELEKTRA_KEY_META, "tomltype", "string_basic",
		ELEKTRA_KEY_META, "binary", "",
	ELEKTRA_KEY_END),
ELEKTRA_KS_END)
