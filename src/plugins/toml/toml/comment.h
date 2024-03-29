// clang-format off
ksNew (128,

	keyNew (PREFIX,
		KEY_META, "comment/#1/start", "",
		KEY_META, "comment/#1/space", "",
		KEY_META, "comment/#2", " Comment on second-to-last line",
		KEY_META, "comment/#2/start", "#",
		KEY_META, "comment/#2/space", "  ",
		KEY_META, "comment/#3", " Comment on last (non-empty) line",
		KEY_META, "comment/#3/start", "#",
		KEY_META, "comment/#3/space", "",
		KEY_META, "comment/#4/start", "",
		KEY_META, "comment/#4/space", "",
	KEY_END),

	keyNew (PREFIX "/a",
		KEY_VALUE, "3",
		KEY_META, "type", "long_long",
		KEY_META, "order", "0",
		KEY_META, "comment/#0", " Inline comment after keypair",
		KEY_META, "comment/#0/space", "   ",
		KEY_META, "comment/#0/start", "#",
		KEY_META, "comment/#1", " Comment on first line",
		KEY_META, "comment/#1/start", "#",
		KEY_META, "comment/#1/space", "",
		KEY_META, "comment/#2/start", "",
		KEY_META, "comment/#2/space", "",
	KEY_END),

	keyNew (PREFIX "/array",
		KEY_META, "array", "#2",
		KEY_META, "order", "1",
		KEY_META, "comment/#0", " Inline comment after array",
		KEY_META, "comment/#0/space", "   ",
		KEY_META, "comment/#0/start", "#",
		KEY_META, "comment/#1/start", "",
		KEY_META, "comment/#1/space", "",
		KEY_META, "comment/#2", " Comment #1 on full line",
		KEY_META, "comment/#2/start", "#",
		KEY_META, "comment/#2/space", "",
		KEY_META, "comment/#3", " Comment #2 on full line",
		KEY_META, "comment/#3/start", "#",
		KEY_META, "comment/#3/space", "",
		KEY_META, "comment/#4/start", "",
		KEY_META, "comment/#4/space", "",
	KEY_END),

	keyNew (PREFIX "/array/#0",
		KEY_VALUE, "1",
		KEY_META, "type", "long_long",
		KEY_META, "comment/#0", " Comment after first value of array",
		KEY_META, "comment/#0/start", "#",
		KEY_META, "comment/#0/space", "  ",
		KEY_META, "comment/#1", " Comment directly after array opening brackets, before first value",
		KEY_META, "comment/#1/start", "#",
		KEY_META, "comment/#1/space", "   ",
	KEY_END),

	keyNew (PREFIX "/array/#1",
		KEY_VALUE, "2",
		KEY_META, "type", "long_long",
		KEY_META, "comment/#0", " Comment after second value of array",
		KEY_META, "comment/#0/start", "#",
		KEY_META, "comment/#0/space", "  ",
	KEY_END),

	keyNew (PREFIX "/array/#2",
		KEY_VALUE, "3",
		KEY_META, "type", "long_long",
		KEY_META, "comment/#0", " Comment after last value of array",
		KEY_META, "comment/#0/space", "   ",
		KEY_META, "comment/#0/start", "#",
	KEY_END),

	keyNew (PREFIX "/table",
		KEY_META, "tomltype", "simpletable",
		KEY_META, "order", "2",
		KEY_META, "comment/#0", " Comment after table",
		KEY_META, "comment/#0/start", "#",
		KEY_META, "comment/#0/space", " ",
		KEY_META, "comment/#1/start", "",
		KEY_META, "comment/#1/space", "",
	KEY_END),

	keyNew (PREFIX "/table_array",
		KEY_META, "array", "#0",
		KEY_META, "tomltype", "tablearray",
		KEY_META, "order", "3",
	KEY_END),

	keyNew (PREFIX "/table_array/#0",
		KEY_META, "comment/#0", " Comment after table array",
		KEY_META, "comment/#0/space", " ",
		KEY_META, "comment/#0/start", "#",
		KEY_META, "comment/#1/start", "",
		KEY_META, "comment/#1/space", "",
	KEY_END),

KS_END)


		

