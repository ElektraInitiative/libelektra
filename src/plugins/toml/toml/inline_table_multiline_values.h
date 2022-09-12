// clang-format off
elektraKeysetNew (16,
	elektraKeyNew (PREFIX "/inline_table",
		ELEKTRA_KEY_META, "tomltype", "inlinetable",
		ELEKTRA_KEY_META, "order", "0",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/inline_table/multiline_string_basic",
		ELEKTRA_KEY_VALUE, "Basic\nMulti\nLine\nString\n",
		ELEKTRA_KEY_META, "origvalue", "\nBasic\nMulti\nLine\nString\n",
		ELEKTRA_KEY_META, "tomltype", "string_ml_basic",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "1",
	ELEKTRA_KEY_END),
	elektraKeyNew (PREFIX "/inline_table/multiline_string_literal",
		ELEKTRA_KEY_VALUE, "Literal\nMulti\nLine\nString",
		ELEKTRA_KEY_META, "origvalue", "\nLiteral\nMulti\nLine\nString",
		ELEKTRA_KEY_META, "tomltype", "string_ml_literal",
		ELEKTRA_KEY_META, "type", "string",
		ELEKTRA_KEY_META, "order", "2",
	ELEKTRA_KEY_END),

ELEKTRA_KS_END)


