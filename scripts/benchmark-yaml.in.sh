#!/usr/bin/env bash
#
# @author René Schwaiger <sanssecours@me.com>
# @brief Benchmark the runtime of different YAML plugins
# @date 12.04.2019
# @tags benchmark

BUILD_DIRECTORY="@CMAKE_BINARY_DIR@"
SOURCE_DIRECTORY="@CMAKE_SOURCE_DIR@"

cd "$SOURCE_DIRECTORY" || {
	printf >&2 'Unable to change working directory to “%s”\n' "$SOURCE_DIRECTORY"
	exit 1
}

command -v hyperfine > /dev/null 2>&1 || {
	printf >&2 'This test requires the command `hyperfine`\n'
	exit 1
}

PLUGINS=(yamlcpp yanlr yambi yawn yaypeg)
DATA_DIRECTORY="benchmarks/data"
BENCHMARK_TOOL="$BUILD_DIRECTORY/bin/benchmark_plugingetset"

for PLUGIN in "${PLUGINS[@]}"; do
	cp "$DATA_DIRECTORY/test.yaml" "$DATA_DIRECTORY/test.$PLUGIN.in"
done

hyperfine \
	"\"$BENCHMARK_TOOL\" \"$DATA_DIRECTORY\" user ${PLUGINS[0]} get" \
	"\"$BENCHMARK_TOOL\" \"$DATA_DIRECTORY\" user ${PLUGINS[1]} get" \
	"\"$BENCHMARK_TOOL\" \"$DATA_DIRECTORY\" user ${PLUGINS[2]} get" \
	"\"$BENCHMARK_TOOL\" \"$DATA_DIRECTORY\" user ${PLUGINS[3]} get" \
	"\"$BENCHMARK_TOOL\" \"$DATA_DIRECTORY\" user ${PLUGINS[4]} get" | sed -e "s~$BUILD_DIRECTORY/bin/~~g"

for PLUGIN in "${PLUGINS[@]}"; do
	rm "$DATA_DIRECTORY/test.$PLUGIN.in"
done
