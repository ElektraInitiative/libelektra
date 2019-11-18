# Autocompletion

To get autocompletion to run you need to have python3 installed and the kdb-python Module up and running.

## Bash

## How to get it to run
To get the bash completion to run you need to run `source find_autocompletion_options.sh`. After that in your console you should be able to type `kdb <TAB><TAB>` and have completion options show up.

## How to test
Currently it is necessary to call the test-script from the folder the `find_autocompletion_options.py` is located.
From this folder call `python3 test_SOME_TEST.py`.