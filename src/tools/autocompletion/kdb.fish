#/usr/bin/env fish

# all known commands can be found here
set -l commands '(python3 ~/workspace/src/tools/autocompletion/find_autocompletion_options.py -m system:/spec/autocomplete/kdb)'


complete --command kdb -f
complete  --arguments "$commands" --command kdb
