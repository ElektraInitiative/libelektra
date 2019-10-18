// clang-format off
ksNew (4,
    keyNew (PREFIX, KEY_VALUE, "@CONFIG_FILEPATH@", KEY_END),
    keyNew (PREFIX "/a", KEY_VALUE, "3", KEY_END),
    keyNew (PREFIX "/a/b/c/d", KEY_VALUE, "5", KEY_END),
    keyNew (PREFIX "/x", KEY_VALUE, "hello", KEY_END),
    KS_END
)
