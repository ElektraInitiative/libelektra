#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL CODEGEN ECONF
echo

if command -v pkg-config; then
	if ! pkg-config elektra; then
		echo "Elektra not installed, will skip"
		exit 0
	fi
else
	echo "pkg-config not installed, will skip"
	exit 0
fi

check_version

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/codegen/econf"

do_tests() {
	KEY="/sw/example/econf/#0/current"
	UKEY="user:$KEY"
	SPECKEY="spec:$KEY"

	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$UKEY"

	"$KDB" mount "econf_spec.ini" "$SPECKEY" ni
	"$KDB" import "$SPECKEY" ni < "$EXTERNAL_FOLDER/spec.ini"
	"$KDB" spec-mount "$KEY"

	./application
	succeed_if "application could not read default config (spec)"

	"$KDB" set "$UKEY/root" "1"
	"$KDB" meta-set "$UKEY/format" array "#2"

	"$KDB" set "$UKEY/format/#0/pattern" "*.txt"
	"$KDB" set "$UKEY/format/#0/indent/style" "tab"
	"$KDB" set "$UKEY/format/#0/indent/size" "0"
	"$KDB" set "$UKEY/format/#0/tabwidth" "4"
	"$KDB" set "$UKEY/format/#0/eol" "native"
	"$KDB" set "$UKEY/format/#0/charset" "utf-8"
	"$KDB" set "$UKEY/format/#0/trim" "0"
	"$KDB" set "$UKEY/format/#0/eofnewline" "1"
	"$KDB" set "$UKEY/format/#0/linelength" "120"

	"$KDB" set "$UKEY/format/#1/pattern" "*.c"
	"$KDB" set "$UKEY/format/#1/indent/style" "space"
	"$KDB" set "$UKEY/format/#1/indent/size" "8"
	"$KDB" set "$UKEY/format/#1/tabwidth" "0"
	"$KDB" set "$UKEY/format/#1/eol" "lf"
	"$KDB" set "$UKEY/format/#1/charset" "utf-8"
	"$KDB" set "$UKEY/format/#1/trim" "1"
	"$KDB" set "$UKEY/format/#1/eofnewline" "1"
	"$KDB" set "$UKEY/format/#1/linelength" "120"

	"$KDB" set "$UKEY/format/#2/pattern" "*.cpp"
	"$KDB" set "$UKEY/format/#2/indent/style" "tab"
	"$KDB" set "$UKEY/format/#2/indent/size" "8"
	"$KDB" set "$UKEY/format/#2/tabwidth" "4"
	"$KDB" set "$UKEY/format/#2/eol" "crlf"
	"$KDB" set "$UKEY/format/#2/charset" "utf-16le"
	"$KDB" set "$UKEY/format/#2/trim" "1"
	"$KDB" set "$UKEY/format/#2/eofnewline" "0"
	"$KDB" set "$UKEY/format/#2/linelength" "80"

	TEST_CONF=$(
		cat <<- EOF
			root = true
			[*.txt]
			indent_style = tab
			indent_size = tab
			tab_width = 4
			charset = utf-8
			trim_trailing_whitespace = false
			insert_final_newline = true
			max_line_length = 120
			[*.c]
			indent_style = space
			indent_size = 8
			end_of_line = lf
			charset = utf-8
			trim_trailing_whitespace = true
			insert_final_newline = true
			max_line_length = 120
			[*.cpp]
			indent_style = tab
			indent_size = 8
			tab_width = 4
			end_of_line = crlf
			charset = utf-16le
			trim_trailing_whitespace = true
			insert_final_newline = false
			max_line_length = 80
		EOF
	)

	./application
	succeed_if "application could not read test tree"

	[ "$(./application)" = "$TEST_CONF" ]
	succeed_if "application did not read test config correctly"

	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"
	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$UKEY"
	"$KDB" umount "$KEY"
}

echo "Testing build with cmake"

cd "$EXTERNAL_FOLDER"
mkdir build
cd build

# manually set Elektra_DIR and KDB to support non-standard install locations
cmake ../cmake -DElektra_DIR:PATH="$(realpath $(dirname $0)/../../cmake/Elektra)" -DKDB:PATH="$KDB"
succeed_if "could not run cmake"

cmake --build .
succeed_if "could not build cmake project"

do_tests
do_tests

cd ..
rm -r build

echo "Testing build with pkgconfig"

cd "$EXTERNAL_FOLDER/pkgconfig"
# set KDB to support non-standard install locations
KDB="$KDB" make
succeed_if "could not build pkgconfig project"

do_tests
do_tests

make clean

end_script gen
