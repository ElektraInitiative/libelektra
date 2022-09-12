// clang-format off
elektraKeysetNew (16,
	elektraKeyNew (PREFIX "/x",
		ELEKTRA_KEY_VALUE, "1",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "0",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_1",
		ELEKTRA_KEY_META, "tomltype", "simpletable",
		ELEKTRA_KEY_META, "order", "1",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_1/y",
		ELEKTRA_KEY_VALUE, "2",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "2",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_1/z",
		ELEKTRA_KEY_VALUE, "3",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "3",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_2/sub_1/sub_2",
		ELEKTRA_KEY_META, "tomltype", "simpletable",
		ELEKTRA_KEY_META, "order", "4",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_2/sub_1/sub_2/pi",
		ELEKTRA_KEY_VALUE, "3.14",
		ELEKTRA_KEY_META, "type", "double",
		ELEKTRA_KEY_META, "order", "5",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_2/sub_1/sub_2/a/b/c/d/fg",
		ELEKTRA_KEY_VALUE, "test",
		ELEKTRA_KEY_META, "tomltype", "string_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "6",
	ELEKTRA_KEY_END),
ELEKTRA_KS_END)
