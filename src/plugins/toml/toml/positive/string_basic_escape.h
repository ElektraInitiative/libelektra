// clang-format off
ksNew (16,
	keyNew (PREFIX,
		KEY_VALUE, "@CONFIG_FILEPATH@",
	KEY_END),

	keyNew (PREFIX "/basic_escapes",
		KEY_META, "order", "0",
		KEY_VALUE,
		"backspace\b\n"
		"tab\t\n"
		"linefeed\n\n"
		"formfeed\f\n"
		"carriage return\r\n"
		"quote\"\n"
		"backslash\\\n",
	KEY_END),
	keyNew (PREFIX "/unicode_escape",
		KEY_META, "order", "1",
		KEY_VALUE,
		"0030: 0\n"
		"00C0: \xC3\x80\n"
		"0270: \xC9\xB0\n"
		"02DF: \xCB\x9F\n"
		"0376: \xCD\xB6\n"
		"0677: \xD9\xB7\n"
		"0A67: \xE0\xA9\xA7\n"
		"0EAF: \xE0\xBA\xAF\n"
		"11D5: \xE1\x87\x95\n"
		"9ED5: \xE9\xBB\x95\n"
		"b79c: \xEB\x9E\x9C\n"
		"F7D0: \xEF\x9F\x90\n"
		"106D0: \xF0\x90\x9B\x90\n"
		"1447F: \xF0\x94\x91\xBF\n"
		"15A4A: \xF0\x95\xA9\x8A\n"
		"1da8b: \xF0\x9D\xAA\x8B\n"
		"1fffd: \xF0\x9F\xBF\xBD\n",
	KEY_END),

KS_END)
