#!/bin/sh

if [ -z "$APP_PATH" ]; then
	# TODO: set APP_PATH to the installed path of your application
	APP_PATH='/usr/local/bin/tests_script_gen_highlevel_struct'
fi

if ! [ -f "$APP_PATH" ]; then
	echo "ERROR: APP_PATH points to non-existent file" 1>&2
	exit 1
fi

if kdb mount -13 | grep -Fxq 'spec/tests/script/gen/highlevel/struct'; then
	if ! kdb mount | grep -Fxq 'tests_script_gen_highlevel_struct.overlay.spec.eqd on spec/tests/script/gen/highlevel/struct with name spec/tests/script/gen/highlevel/struct'; then
		echo "ERROR: another mountpoint already exists on spec/tests/script/gen/highlevel/struct. Please umount first." 1>&2
		exit 1
	fi
else
	sudo kdb mount -R noresolver "tests_script_gen_highlevel_struct.overlay.spec.eqd" "spec/tests/script/gen/highlevel/struct" specload "app=$APP_PATH"
fi

if kdb mount -13 | grep -Fxq '/tests/script/gen/highlevel/struct'; then
	if ! kdb mount | grep -Fxq 'tests_gen_elektra_struct.ini on /tests/script/gen/highlevel/struct with name /tests/script/gen/highlevel/struct'; then
		echo "ERROR: another mountpoint already exists on /tests/script/gen/highlevel/struct. Please umount first." 1>&2
		exit 1
	fi
else
	sudo kdb spec-mount '/tests/script/gen/highlevel/struct'
fi
