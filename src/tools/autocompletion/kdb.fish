#/usr/bin/env fish

# all known commands can be found here
set -l commands '(python3 ~/src/tools/autocompletion/src/autocompletion.py -m system:/spec/autocomplete/kdb)'

complete --command kdb -f
complete --arguments "$commands" --command kdb
