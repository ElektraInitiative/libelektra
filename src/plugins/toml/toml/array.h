// clang-format off
ksNew (16,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),

    keyNew (PREFIX "/array_1", KEY_META, "array", "#4", KEY_END),
    keyNew (PREFIX "/array_1/#0", KEY_VALUE, "1", KEY_END),
    keyNew (PREFIX "/array_1/#1", KEY_VALUE, "2", KEY_END),
    keyNew (PREFIX "/array_1/#2", KEY_VALUE, "3", KEY_END),
    keyNew (PREFIX "/array_1/#3", KEY_VALUE, "4", KEY_END),
    keyNew (PREFIX "/array_1/#4", KEY_VALUE, "5", KEY_END),

    keyNew (PREFIX "/array_empty", KEY_META, "array", "", KEY_END),

    KS_END
)
