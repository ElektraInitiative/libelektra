#!/bin/sh

# test script for use on Cirrus CI and Github Actions
# Inputs:
#   BUILD_DIR         - path to build directory
#   AUTO_RERUN_TESTS  - regex for tests that should run with --repeat=until-pass:5
#   KDB               - path to kdb executable, if not empty '"$KDB" run_all' will be executed at the end
#   ENABLE_ASAN       - set to 'ON', if built with ASAN (disables script tests)

if [ "$ENABLE_ASAN" = 'ON' ]; then
        EXCLUDED_TESTS="testscr_check"
elif [ "$BUILD_FULL" = 'ON' ]; then
        EXCLUDED_TESTS="testscr_check_external"
else
        EXCLUDED_TESTS="testscr_check_external"
fi

if [ -n "$AUTO_RERUN_TESTS" ]; then
        EXCLUDED_TESTS="$AUTO_RERUN_TESTS|$EXCLUDED_TESTS"
fi

# use subshell for temporary directory switch
(
        set -e
        cd "$BUILD_DIR"
        ctest -j --force-new-ctest-process --output-on-failure -E "$EXCLUDED_TESTS"
        ctest -j --force-new-ctest-process --output-on-failure --repeat "until-pass:5" -R "$AUTO_RERUN_TESTS"
        set +e
)

if [ -n "$KDB" ]; then
        "$KDB" run_all
fi
