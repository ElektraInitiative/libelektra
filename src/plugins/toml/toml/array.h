// clang-format off
ksNew (16,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),

    keyNew (PREFIX "/array_1", KEY_META, "array", "#4", KEY_END),
    keyNew (PREFIX "/array_1/#0", KEY_VALUE, "1", KEY_END),
    keyNew (PREFIX "/array_1/#1", KEY_VALUE, "2", KEY_END),
    keyNew (PREFIX "/array_1/#2", KEY_VALUE, "3", KEY_END),
    keyNew (PREFIX "/array_1/#3", KEY_VALUE, "4", KEY_END),
    keyNew (PREFIX "/array_1/#4", KEY_VALUE, "5", KEY_END),

    keyNew (PREFIX "/array_multiline_1", KEY_META, "array", "#3", KEY_END),
    keyNew (PREFIX "/array_multiline_1/#0", KEY_VALUE, "a", KEY_END),
    keyNew (PREFIX "/array_multiline_1/#1", KEY_VALUE, "b", KEY_END),
    keyNew (PREFIX "/array_multiline_1/#2", KEY_VALUE, "c", KEY_END),
    keyNew (PREFIX "/array_multiline_1/#3", KEY_VALUE, "d", KEY_END),

    keyNew (PREFIX "/array_multiline_2", KEY_META, "array", "#3", KEY_END),
    keyNew (PREFIX "/array_multiline_2/#0", KEY_VALUE, "test", KEY_END),
    keyNew (PREFIX "/array_multiline_2/#1", KEY_VALUE, "test2", KEY_END),
    keyNew (PREFIX "/array_multiline_2/#2", KEY_VALUE, "test3", KEY_END),
    keyNew (PREFIX "/array_multiline_2/#3", KEY_VALUE, "test4", KEY_END),

    keyNew (PREFIX "/array_empty", KEY_META, "array", "", KEY_END),

    KS_END
)
