// clang-format off
ksNew (16,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),

    keyNew (PREFIX "/inline_table", KEY_META, "inlinetable", "", KEY_END),
    keyNew (PREFIX "/inline_table/multiline_string_basic", KEY_VALUE, "\nBasic\nMulti\nLine\nString\n", KEY_END),
    keyNew (PREFIX "/inline_table/multiline_string_literal", KEY_VALUE, "\nLiteral\nMulti\nLine\nString", KEY_END),
    keyNew (PREFIX "/inline_table/array", KEY_META, "array", "#4", KEY_END),
    keyNew (PREFIX "/inline_table/array/#0", KEY_VALUE, "1",  KEY_END),
    keyNew (PREFIX "/inline_table/array/#1", KEY_VALUE, "2",  KEY_END),
    keyNew (PREFIX "/inline_table/array/#2", KEY_VALUE, "3",  KEY_END),
    keyNew (PREFIX "/inline_table/array/#3", KEY_VALUE, "4",  KEY_END),
    keyNew (PREFIX "/inline_table/array/#4", KEY_VALUE, "5",  KEY_END),

    KS_END
)


