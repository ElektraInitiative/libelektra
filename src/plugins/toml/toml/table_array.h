// clang-format off
elektraKeysetNew (16,
	elektraKeyNew (PREFIX "/table_array_basic",
		ELEKTRA_KEY_META, "array", "#2",
		ELEKTRA_KEY_META, "tomltype", "tablearray",
		ELEKTRA_KEY_META, "order", "0",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_array_basic/#0/x",
		ELEKTRA_KEY_VALUE, "1",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "1",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_basic/#0/y",
		ELEKTRA_KEY_VALUE, "2",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "2",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_basic/#0/z",
		ELEKTRA_KEY_VALUE, "3",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "3",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_array_basic/#1/x",
		ELEKTRA_KEY_VALUE, "1337",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "4",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_basic/#1/y",
		ELEKTRA_KEY_VALUE, "666",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "5",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_basic/#1/z",
		ELEKTRA_KEY_VALUE, "42",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "6",
	ELEKTRA_KEY_END),

	elektraKeyNew (PREFIX "/table_array_basic/#2/x",
		ELEKTRA_KEY_VALUE, "100",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "7",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_basic/#2/y",
		ELEKTRA_KEY_VALUE, "200",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "8",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_basic/#2/z",
		ELEKTRA_KEY_VALUE, "300",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "9",
	ELEKTRA_KEY_END),


	elektraKeyNew (PREFIX "/table_array_maybe_empty",
		ELEKTRA_KEY_META, "array", "#5",
		ELEKTRA_KEY_META, "tomltype", "tablearray",
		ELEKTRA_KEY_META, "order", "10",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_maybe_empty/#2/some_content",
		ELEKTRA_KEY_VALUE, "1",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "11",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/table_array_maybe_empty/#4/some_content",
		ELEKTRA_KEY_VALUE, "2",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "12",
	ELEKTRA_KEY_END),

ELEKTRA_KS_END)
