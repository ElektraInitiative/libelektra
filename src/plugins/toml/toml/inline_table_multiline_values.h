// clang-format off
ksNew (16,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),

    keyNew (PREFIX "/inline_table", KEY_META, "inlinetable", "", KEY_END),
    keyNew (PREFIX "/inline_table/multiline_string_basic", KEY_VALUE, "\nBasic\nMulti\nLine\nString\n", KEY_END),
    keyNew (PREFIX "/inline_table/multiline_string_literal", KEY_VALUE, "\nLiteral\nMulti\nLine\nString", KEY_END),

    KS_END
)


