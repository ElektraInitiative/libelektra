// clang-format off

#define PREFIX "user/tests/yanlr/"

ksNew (10,
       keyNew (PREFIX "a/b", KEY_VALUE, "c", KEY_END),
       keyNew (PREFIX "d/e", KEY_VALUE, "f", KEY_END),
       keyNew (PREFIX "d/g", KEY_VALUE, "h", KEY_END),
       KS_END)
