// clang-format off
ksNew (16,
	keyNew (PREFIX "/table_array",
		ELEKTRA_KEY_META, "array", "#1",
		ELEKTRA_KEY_META, "tomltype", "tablearray",
		ELEKTRA_KEY_META, "order", "0",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/table_array/#0/a",
		ELEKTRA_KEY_VALUE, "0",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "1",
	ELEKTRA_KEY_END),
	keyNew (PREFIX "/table_array/#0/table",
		ELEKTRA_KEY_META, "tomltype", "simpletable",
		ELEKTRA_KEY_META, "order", "2",
	ELEKTRA_KEY_END),
	keyNew (PREFIX "/table_array/#0/table/b",
		ELEKTRA_KEY_VALUE, "1",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "3",
	ELEKTRA_KEY_END),

	keyNew (PREFIX "/table_array/#1/a",
		ELEKTRA_KEY_VALUE, "10",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "4",
	ELEKTRA_KEY_END),
	keyNew (PREFIX "/table_array/#1/table",
		ELEKTRA_KEY_META, "tomltype", "simpletable",
		ELEKTRA_KEY_META, "order", "5",
	ELEKTRA_KEY_END),
	keyNew (PREFIX "/table_array/#1/table/b",
		ELEKTRA_KEY_VALUE, "20",
		ELEKTRA_KEY_META, "type", "long_long",
		ELEKTRA_KEY_META, "order", "6",
	ELEKTRA_KEY_END),

ELEKTRA_KS_END)
