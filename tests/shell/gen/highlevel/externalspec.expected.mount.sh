#!/bin/sh

if [ -z "$SPEC_FILE" ]; then
	# TODO: set SPEC_FILE to the installed path of your spec.eqd file
	SPEC_FILE='/usr/local/share/tests_script_gen_highlevel_externalspec.spec.eqd'
fi

if ! [ -f "$SPEC_FILE" ]; then
	echo "ERROR: SPEC_FILE points to non-existent file" 1>&2
	exit 1
fi

error_other_mp() {
	echo "ERROR: another mountpoint already exists on spec:/tests/script/gen/highlevel/externalspec. Please umount first." 1>&2
	exit 1
}

if kdb mount -13 | grep -Fxq 'spec:/tests/script/gen/highlevel/externalspec'; then
	if ! kdb mount | grep -Fxq 'tests_script_gen_highlevel_externalspec.overlay.spec.eqd on spec:/tests/script/gen/highlevel/externalspec with name spec:/tests/script/gen/highlevel/externalspec'; then
		error_other_mp
	fi

	MP=$(echo "spec:/tests/script/gen/highlevel/externalspec" | sed 's:\\:\\\\:g' | sed 's:/:\\/:g')
	if [ "$(kdb get "system:/elektra/mountpoints/$MP/getplugins/#5#specload#specload#/config/file")" != "$SPEC_FILE" ]; then
		error_other_mp
	fi
else
	sudo kdb mount -R noresolver "tests_script_gen_highlevel_externalspec.overlay.spec.eqd" "spec:/tests/script/gen/highlevel/externalspec" specload "file=$SPEC_FILE"
fi

if kdb mount -13 | grep -Fxq '/tests/script/gen/highlevel/externalspec'; then
	if ! kdb mount | grep -Fxq 'tests_gen_elektra_simple.ini on /tests/script/gen/highlevel/externalspec with name /tests/script/gen/highlevel/externalspec'; then
		echo "ERROR: another mountpoint already exists on /tests/script/gen/highlevel/externalspec. Please umount first." 1>&2
		exit 1
	fi
else
	sudo kdb spec-mount '/tests/script/gen/highlevel/externalspec'
fi
