// clang-format off
ksNew (16,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),

    keyNew (PREFIX "/multiline_basic_leading_newline", KEY_VALUE, "This string should be in a single line.", KEY_END),
    keyNew (PREFIX "/multiline_basic_lineending_newlines", KEY_VALUE, "This string should also be in a single line.", KEY_END),
    keyNew (PREFIX "/multiline_literal_leading_newline", KEY_VALUE, "Single line string", KEY_END),
    keyNew (PREFIX "/multiline_literal_linending_newlines", KEY_VALUE, "Single line string.", KEY_END),
    keyNew (PREFIX "/another_single_ling_string", KEY_VALUE, "The quick brown fox jumps over the lazy dog.", KEY_END),

    KS_END
)
