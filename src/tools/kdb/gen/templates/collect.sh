#!/bin/sh

# copyright BSD License (see LICENSE.md or https://www.libelektra.org)
# BSD License (see LICENSE.md or https://www.libelektra.org)

# shellcheck disable=SC2016
AWK_ESCAPE_SCRIPT='
BEGIN {
	for (n = 0; n < 256; n++)
	{
		ord[sprintf("%c", n)] = n
	}
}

{
	for (i = 1; i <= length; i++) {
	    c = substr($0, i, 1)
		if (c ~ /[\\"'"'"'\?]/) {
			printf("\\%s", c)
		} else if (c == "\t") {
			printf("%s", "\t")
		} else if (c == "\r") {
			printf("%s", "\r")
		} else if (c ~ /[ -~]/) {
			printf("%s", c)
		} else {
			printf("\\x%02x", ord[c])
		}
	}
	printf("%s", "\\n")
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
