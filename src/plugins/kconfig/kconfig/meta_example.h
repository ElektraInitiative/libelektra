// clang-format off
ksNew (10,
       keyNew ("/meta_key", KEY_VALUE, "Some meta", KEY_META, "kconfig", "i", KEY_END),
       keyNew ("/empty_group/with_meta", KEY_META, "kconfig", "a", KEY_END),
       keyNew ("/group_with/localizations/localized[en]", KEY_VALUE, "Hello", KEY_END),
       keyNew ("/group_with/localizations/localized[de]", KEY_VALUE, "Hallo", KEY_END),
       KS_END)
