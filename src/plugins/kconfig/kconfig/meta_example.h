// clang-format off
ksNew (10,
       keyNew (PREFIX "meta_key", KEY_VALUE, "Some meta", KEY_META, "kconfig", "i", KEY_END),
       keyNew (PREFIX "empty_group/with_meta", KEY_META, "kconfig", "a", KEY_END),
       keyNew (PREFIX "group_with/localizations/localized[en]", KEY_VALUE, "Hello", KEY_END),
       keyNew (PREFIX "group_with/localizations/localized[de]", KEY_VALUE, "Hallo", KEY_END),
       KS_END)
