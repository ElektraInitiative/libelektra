@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK EXTERNAL CODEGEN TREE
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

EXTERNAL_FOLDER="@CMAKE_SOURCE_DIR@/examples/codegen/tree"

do_tests() {
	KEY="/sw/example/tree/#0/current"
	UKEY="user$KEY"
	SPECKEY="spec$KEY"

	"$KDB" umount "$SPECKEY"
	"$KDB" umount "$KEY"
	"$KDB" rm -r "$UKEY"
	"$KDB" rm -r "$SPECKEY"

	"$KDB" mount "tree_spec.ini" "$SPECKEY" ni
	"$KDB" import "$SPECKEY" ni < "$EXTERNAL_FOLDER/spec.ini"
	"$KDB" spec-mount "$KEY"

	./application
	succeed_if "application could not read default config (spec)"

	[ "$(./application)" = "(empty)" ]
	succeed_if "application didn't read empty tree correctly"

	TEST_TREE=$(
		cat <<- EOF
			root
			  child1
			  child2
			  (empty)
			  child4
			    grandchildA
			      leafA
			    grandchildB
			      leafB
		EOF
	)

	"$KDB" set "$UKEY/tree/root" ""
	"$KDB" set "$UKEY/tree/root/text" "root"
	"$KDB" set "$UKEY/tree/child1" ""
	"$KDB" set "$UKEY/tree/child1/text" "child1"
	"$KDB" set "$UKEY/tree/child2" ""
	"$KDB" set "$UKEY/tree/child2/text" "child2"
	"$KDB" set "$UKEY/tree/child4" ""
	"$KDB" set "$UKEY/tree/child4/text" "child4"
	"$KDB" set "$UKEY/tree/grandchildA" ""
	"$KDB" set "$UKEY/tree/grandchildA/text" "grandchildA"
	"$KDB" set "$UKEY/tree/leafA" ""
	"$KDB" set "$UKEY/tree/leafA/text" "leafA"
	"$KDB" set "$UKEY/tree/grandchildB" ""
	"$KDB" set "$UKEY/tree/grandchildB/text" "grandchildB"
	"$KDB" set "$UKEY/tree/leafB" ""
	"$KDB" set "$UKEY/tree/leafB/text" "leafB"

	"$KDB" meta-set "$UKEY/tree/root/children" "array" "#3"
	"$KDB" set "$UKEY/tree/root/children/#0" "../../../child1"
	"$KDB" set "$UKEY/tree/root/children/#1" "../../../child2"
	"$KDB" set "$UKEY/tree/root/children/#3" "../../../child4"

	"$KDB" meta-set "$UKEY/tree/child4/children" "array" "#1"
	"$KDB" set "$UKEY/tree/child4/children/#0" "../../../grandchildA"
	"$KDB" set "$UKEY/tree/child4/children/#1" "../../../grandchildB"

	"$KDB" meta-set "$UKEY/tree/grandchildA/children" "array" "#0"
	"$KDB" set "$UKEY/tree/grandchildA/children/#0" "../../../leafA"

	"$KDB" meta-set "$UKEY/tree/grandchildB/children" "array" "#0"
	"$KDB" set "$UKEY/tree/grandchildB/children/#0" "../../../leafB"

	"$KDB" set "$UKEY/root" "../tree/root"

	./application
	succeed_if "application could not read test tree"

	[ "$(./application)" = "$TEST_TREE" ]
	succeed_if "application did not read test tree correctly"

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
