#!/bin/sh

NL='
'

result_value="$1"

case "$result_value" in
"success" | "noupdate" | "error") ;;
"")
	result_value="success"
	;;
*)
	exit 1
	;;
esac

read -r header init_cmd version

if [ "$header" != "ELEKTRA_STDIOPROC" ]; then
	exit 1
fi

if [ "$init_cmd" != "INIT" ]; then
	exit 1
fi

if [ "$version" != "v1" ]; then
	exit 1
fi

printf "ELEKTRA_STDIOPROC ACK v1\n"
printf "testapp\n"
# shellcheck disable=SC2016
printf 'kdbOpen 2
$key string 47 1
system:/elektra/modules/stdioproc/exports/close
1
$key string 45 1
system:/elektra/modules/stdioproc/exports/get
1
$key string 46 1
system:/elektra/modules/stdioproc/exports/open
1
$key string 45 1
system:/elektra/modules/stdioproc/exports/set
1
$key string 39 60
system:/elektra/modules/stdioproc/infos
Information about the stdioproc test plugin is in keys below
$key string 46 45
system:/elektra/modules/stdioproc/infos/author
Klemens Böswirth <k.boeswirth+git@gmail.com>
$key string 51 25
system:/elektra/modules/stdioproc/infos/description
test plugin for stdioproc
$key string 47 3
system:/elektra/modules/stdioproc/infos/licence
BSD
$key string 48 0
system:/elektra/modules/stdioproc/infos/metadata

$key string 45 0
system:/elektra/modules/stdioproc/infos/needs

$key string 50 21
system:/elektra/modules/stdioproc/infos/placements
getstorage setstorage
$key string 48 0
system:/elektra/modules/stdioproc/infos/provides

$key string 50 0
system:/elektra/modules/stdioproc/infos/recommends

$key string 46 42
system:/elektra/modules/stdioproc/infos/status
maintained unittest shelltest experimental
$end
'

read_keyset() {
	read -r header
	if [ "$header" != "kdbOpen 2" ]; then
		exit 1
	fi

	keyset="kdbOpen 2$NL"
	while read -r line; do
		keyset="$keyset$line$NL"
		# shellcheck disable=SC2016
		if [ "$line" = '$end' ]; then
			echo "$keyset"
			return 0
		fi
	done
}

while read -r cmd; do
	case "$cmd" in
	"open")
		parent_ks=$(read_keyset)
		# shellcheck disable=SC2034
		config_ks=$(read_keyset) # config_ks ignored

		parent=$(echo "$parent_ks" | sed -n 3p)
		plen=${#parent}

		printf "%s\n" "$result_value"
		# shellcheck disable=SC2016
		printf 'kdbOpen 2\n$key string %d %d\n%s\n%s\n$end\n' "${#parent}" "${#cmd}" "$parent" "$cmd"
		;;
	"close")
		parent_ks=$(read_keyset)

		parent=$(echo "$parent_ks" | sed -n 3p)
		plen=${#parent}

		printf "%s\n" "$result_value"
		# shellcheck disable=SC2016
		printf 'kdbOpen 2\n$key string %d %d\n%s\n%s\n$end\n' "${#parent}" "${#cmd}" "$parent" "$cmd"
		;;
	"get" | "set")
		parent_ks=$(read_keyset)
		data_ks=$(read_keyset)

		parent=$(echo "$parent_ks" | sed -n 3p)
		plen=${#parent}
		klen=$((plen + 10))
		extra_key="\$key string $klen ${#cmd}$NL"
		extra_key="$extra_key$parent/operation$NL"
		extra_key="$extra_key$cmd$NL"

		data_ks=$(
			echo "$data_ks" | head -n-1
			echo "$extra_key\$end$NL"
		)
		printf "%s\n" "$result_value"
		# shellcheck disable=SC2016
		printf 'kdbOpen 2\n$key string %d %d\n%s\n%s\n$end\n' "$plen" "${#cmd}" "$parent" "$cmd"
		printf "%s\n" "$data_ks"
		;;
	"ELEKTRA_STDIOPROC TERMINATE")
		exit 0
		;;
	*)
		# shouldn't happen
		exit 1
		;;
	esac
done
