// clang-format off
ksNew (16,
	keyNew (PREFIX,
		KEY_VALUE, "@CONFIG_FILEPATH@",
	KEY_END),

	keyNew (PREFIX "/x",
		KEY_VALUE, "1",
		KEY_META, "type", "long_long",
		KEY_META, "order", "0",
	KEY_END),

	keyNew (PREFIX "/table_1",
		KEY_META, "type", "simpletable",
		KEY_META, "order", "1",
	KEY_END),

	keyNew (PREFIX "/table_1/y",
		KEY_VALUE, "2",
		KEY_META, "type", "long_long",
		KEY_META, "order", "2",
	KEY_END),

	keyNew (PREFIX "/table_1/z",
		KEY_VALUE, "3",
		KEY_META, "type", "long_long",
		KEY_META, "order", "3",
	KEY_END),

	keyNew (PREFIX "/table_2/sub_1/sub_2",
		KEY_META, "type", "simpletable",
		KEY_META, "order", "4",
	KEY_END),

	keyNew (PREFIX "/table_2/sub_1/sub_2/pi",
		KEY_VALUE, "3.14",
		KEY_META, "type", "double",
		KEY_META, "order", "5",
	KEY_END),

	keyNew (PREFIX "/table_2/sub_1/sub_2/a/b/c/d/fg",
		KEY_VALUE, "test",
		KEY_META, "type", "string",
		KEY_META, "order", "6",
	KEY_END),

KS_END)
