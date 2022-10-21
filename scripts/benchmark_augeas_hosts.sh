#!/usr/bin/env bash
# bash required for platform independent time
#
# @author Felix Berlakovich <elektra@berlakovich.net>
# @tags benchmark
# @date 21.02.2016

if [ -z "$KDB" ]; then
	KDB=kdb
fi

if [ -z "$1" ]; then
	echo "Usage: $0 <hosts file name>"
	exit 1
fi

measure_time() {
	{ time -f "%e" "$1" > /dev/null; } 2>&1
}

HOSTSFILE=$(echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")")
AUGEASPATH="system:/benchmarks/$$/augeasplugin-hostsfile"
KEYTOMETAPATH="system:/benchmarks/$$/augeaskeytometahostsfile"
HOSTSPATH="system:/benchmarks/$$/hostsplugin-hostsfile"

$KDB mount "$HOSTSFILE" $AUGEASPATH augeas lens=Hosts.lns
$KDB mount "$HOSTSFILE" $HOSTSPATH hosts
$KDB mount "$HOSTSFILE" $KEYTOMETAPATH augeas lens=Hosts.lns glob keytometa

for run in $(seq 0 11); do
	echo "RUN: #$run"
	augeaswalltime=$(measure_time "$KDB ls $AUGEASPATH")
	hostswalltime=$(measure_time "$KDB ls $HOSTSPATH")
	keytometawalltime=$(measure_time "$KDB ls $KEYTOMETAPATH")
	echo "augeas: $augeaswalltime; hosts: $hostswalltime; augeas with keytometa: $keytometawalltime"
done

for x in $($KDB mount | grep system:/benchmarks/$$ | awk '{print $3}'); do $KDB umount "$x"; done
