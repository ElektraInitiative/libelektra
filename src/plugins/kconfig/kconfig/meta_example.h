// clang-format off
elektraKeysetNew (10,
       elektraKeyNew ("/meta_key", ELEKTRA_KEY_VALUE, "Some meta", ELEKTRA_KEY_META, "kconfig", "i", ELEKTRA_KEY_END),
       elektraKeyNew ("/empty_group/with_meta", ELEKTRA_KEY_META, "kconfig", "a", ELEKTRA_KEY_END),
       elektraKeyNew ("/group_with/localizations/localized[en]", ELEKTRA_KEY_VALUE, "Hello", ELEKTRA_KEY_END),
       elektraKeyNew ("/group_with/localizations/localized[de]", ELEKTRA_KEY_VALUE, "Hallo", ELEKTRA_KEY_END),
       ELEKTRA_KS_END)