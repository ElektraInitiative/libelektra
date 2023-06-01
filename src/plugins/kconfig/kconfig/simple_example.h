// clang-format off
ksNew (10,
       keyNew ("/groupless.key", KEY_VALUE, "Simple Value With Spaces", KEY_END),
       keyNew ("/example/group/example.key", KEY_VALUE, "Key\nWith\tEscaped\\Characters", KEY_END),
       KS_END)
