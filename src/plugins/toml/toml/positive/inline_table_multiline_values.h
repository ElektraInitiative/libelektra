// clang-format off
ksNew (16,
	keyNew (PREFIX "/inline_table",
		KEY_META, "tomltype", "inlinetable",
		KEY_META, "order", "0",
	KEY_END),
	keyNew (PREFIX "/inline_table/multiline_string_basic",
		KEY_VALUE, "Basic\nMulti\nLine\nString\n",
		KEY_META, "origvalue", "\nBasic\nMulti\nLine\nString\n",
		KEY_META, "order", "1",
	KEY_END),
	keyNew (PREFIX "/inline_table/multiline_string_literal",
		KEY_VALUE, "Literal\nMulti\nLine\nString",
		KEY_META, "origvalue", "\nLiteral\nMulti\nLine\nString",
		KEY_META, "order", "2",
	KEY_END),

KS_END)


