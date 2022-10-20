#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL
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

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/codegen/menu"

do_tests() {
	KEY="/sw/example/menu/#0/current"
	UKEY="user:$KEY"
	SPECKEY="spec:$KEY"

	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$KEY"
	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"

	"$KDB" mount "menu_spec.ini" "$SPECKEY" ni
	"$KDB" import "$SPECKEY" ni < "$EXTERNAL_FOLDER/spec.ini"
	"$KDB" spec-mount "$KEY"

	./application
	succeed_if "application could not read default config (spec)"

	[ "$(./application)" = "no menu found" ]
	succeed_if "application didn't read empty menu correctly"

	EXPECTED_MENU=$(mktemp)
	cat > "$EXPECTED_MENU" <<- 'EOF'
		Main Menu:

		  [1] Menu 1
		  [2] Menu 2

		Please select what to do (Ctrl-D = quit): 

	EOF

	"$KDB" meta-set -f "user:/sw/example/menu/#0/current/menu" "array" "#4"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#0/name" "Main Menu"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#1/name" "Menu 1"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#2/name" "Menu 2"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#3/name" "Menu 2.1"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#4/name" "Menu 2.2"

	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#1/command" 'echo "Hello from Menu 1"'
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#2/command" 'echo "Hello from Menu 2"'
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#3/command" 'echo "Hello from Menu 2.1"'
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#4/command" 'echo "Hello from Menu 2.2"'

	"$KDB" meta-set -f "user:/sw/example/menu/#0/current/menu/#0/children" "array" "#1"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#0/children/#0" "@/menu/#1"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#0/children/#1" "@/menu/#2"

	"$KDB" meta-set -f "user:/sw/example/menu/#0/current/menu/#2/children" "array" "#1"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#2/children/#0" "@/menu/#3"
	"$KDB" set -f "user:/sw/example/menu/#0/current/menu/#2/children/#1" "@/menu/#4"

	"$KDB" set -f "user:/sw/example/menu/#0/current/main" "@/menu/#0"

	ACTUAL_MENU=$(mktemp)

	: | ./application
	succeed_if "application could not read test menu"

	: | ./application > "$ACTUAL_MENU"
	diff -u "$EXPECTED_MENU" "$ACTUAL_MENU"
	succeed_if "application did not read test menu correctly"

	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"
	"$KDB" umount "$SPECKEY"
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
