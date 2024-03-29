// clang-format off
ksNew (32,
	keyNew (PREFIX "/basic",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "0",
	KEY_END),
	keyNew (PREFIX "/basic/x",
		KEY_VALUE, "3",
		KEY_META, "type", "long_long",
		KEY_META, "order", "1",
	KEY_END),
	keyNew (PREFIX "/basic/y",
		KEY_VALUE, "1234",
		KEY_META, "type", "long_long",
		KEY_META, "order", "2",
	KEY_END),
	keyNew (PREFIX "/basic/z",
		KEY_VALUE, "500",
		KEY_META, "type", "long_long",
		KEY_META, "order", "3",
	KEY_END),

	keyNew (PREFIX "/inl_with_dot_keys/dot1/dot2",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "4",
	KEY_END),
	keyNew (PREFIX "/inl_with_dot_keys/dot1/dot2/a",
		KEY_VALUE, "123",
		KEY_META, "tomltype", "string_basic",
		KEY_META, "type", "string",
		KEY_META, "order", "5",
	KEY_END),
	keyNew (PREFIX "/inl_with_dot_keys/dot1/dot2/b",
		KEY_VALUE, "456",
		KEY_META, "tomltype", "string_basic",
		KEY_META, "type", "string",
		KEY_META, "order", "6",
	KEY_END),

	keyNew (PREFIX "/inl_with_array",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "7",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/int",
		KEY_META, "array", "#0",
		KEY_META, "order", "8",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/int/#0",
		KEY_VALUE, "1",
		KEY_META, "type", "long_long",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/str",
		KEY_META, "array", "#1",
		KEY_META, "order", "9",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/str/#0",
		KEY_VALUE, "test1",
		KEY_META, "tomltype", "string_basic",
		KEY_META, "type", "string",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/str/#1",
		KEY_VALUE, "test2",
		KEY_META, "tomltype", "string_basic",
		KEY_META, "type", "string",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/bool",
		KEY_META, "array", "#2",
		KEY_META, "order", "10",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/bool/#0",
		KEY_VALUE, "1",
		KEY_META, "type", "boolean",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/bool/#1",
		KEY_VALUE, "0",
		KEY_META, "type", "boolean",
	KEY_END),
	keyNew (PREFIX "/inl_with_array/bool/#2",
		KEY_VALUE, "1",
		KEY_META, "type", "boolean",
	KEY_END),

	keyNew (PREFIX "/inl_nested",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "11",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_1",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "12",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_1/a",
		KEY_VALUE, "1",
		KEY_META, "type", "long_long",
		KEY_META, "order", "13",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_1/b",
		KEY_VALUE, "2",
		KEY_META, "type", "long_long",
		KEY_META, "order", "14",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_1/c",
		KEY_VALUE, "3",
		KEY_META, "type", "long_long",
		KEY_META, "order", "15",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_2",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "16",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_2/x",
		KEY_VALUE, "4",
		KEY_META, "type", "long_long",
		KEY_META, "order", "17",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_2/y",
		KEY_VALUE, "5",
		KEY_META, "type", "long_long",
		KEY_META, "order", "18",
	KEY_END),
	keyNew (PREFIX "/inl_nested/nest_2/z",
		KEY_VALUE, "6",
		KEY_META, "type", "long_long",
		KEY_META, "order", "19",
	KEY_END),

	keyNew (PREFIX "/inl_nested_nested",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "20",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "21",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1/nest_2_1",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "22",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1/nest_2_1/inner_1",
		KEY_VALUE, "666",
		KEY_META, "type", "long_long",
		KEY_META, "order", "23",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1/nest_2_1/inner_2",
		KEY_VALUE, "667",
		KEY_META, "type", "long_long",
		KEY_META, "order", "24",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1/nest_2_2",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "25",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1/nest_2_2/inner_1",
		KEY_VALUE, "1",
		KEY_META, "type", "long_long",
		KEY_META, "order", "26",
	KEY_END),
	keyNew (PREFIX "/inl_nested_nested/nest_1/nest_2_2/inner_2",
		KEY_VALUE, "2",
		KEY_META, "type", "long_long",
		KEY_META, "order", "27",
	KEY_END),
KS_END)
