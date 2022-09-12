// clang-format off
ksNew (10,
       keyNew ("/groupless.key", ELEKTRA_KEY_VALUE, "Simple Value With Spaces", ELEKTRA_KEY_END),
       keyNew ("/example/group/example.key", ELEKTRA_KEY_VALUE, "Key\nWith\tEscaped\\Characters", ELEKTRA_KEY_END),
       ELEKTRA_KS_END)