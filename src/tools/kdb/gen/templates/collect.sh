#!/bin/sh

# Copyright (C) 2019 Elektra Initiative.
# BSD License (see LICENSE.md or https://www.libelektra.org)

# shellcheck disable=SC2016
AWK_ESCAPE_SCRIPT='
{
	split($0, chars, "")
	for (i=1; i <= length($0); i++) {
		if (chars[i] ~ /["'"'"'\\?]/) {
			printf("\\%s", chars[i])
		} else if (chars[i] == "\t") {
			printf("%s", "\\t")
		} else if (chars[i] == "\r") {
			printf("%s", "\\r")
		} else if (chars[i] ~ /[[:print:]]/) {
			printf("%s", chars[i])
		} else {
			printf("\\x%02x", chars[i])
		}
	}
	printf ("%s", "\\n")
}
'

OUTFILE=$1
shift

printf "" > "$OUTFILE"

cat << EOF >> "$OUTFILE"
#include <unordered_map>
#include <string>
EOF

for INFILE in "$@"; do
	name=$(echo "$INFILE" | awk '{ s = $0; sub (".mustache$", "", s); gsub ("[^0-9A-Za-z]", "_", s); printf ("%s", s); }')
	printf 'static const char * const _kdbgentemplate_%s = "%s";\n' "$name" "$(awk "$AWK_ESCAPE_SCRIPT" "$INFILE")" >> "$OUTFILE"
done

cat << EOF >> "$OUTFILE"

static const std::unordered_map<std::string, std::string> kdbgenTemplates = {
EOF

for INFILE in "$@"; do
	name=$(echo "$INFILE" | awk '{ s = $0; sub (".mustache$", "", s); gsub ("[^0-9A-Za-z]", "_", s); printf ("%s", s); }')
	printf '\t{ "%s", _kdbgentemplate_%s },\n' "$name" "$name" >> "$OUTFILE"
done

cat << EOF >> "$OUTFILE"
};

EOF
