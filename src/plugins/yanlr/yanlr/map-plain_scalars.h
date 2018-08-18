// clang-format off

#define PREFIX "user/tests/yanlr/"

ksNew (10,
       keyNew (PREFIX "key", KEY_VALUE, "value", KEY_END),
       keyNew (PREFIX "hello", KEY_VALUE, "world", KEY_END),
       KS_END)
