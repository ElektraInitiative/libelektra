# Autocompletion

To get autocompletion to run you need to have python3 installed and the kdb-python Module up and running.
For the kdb-python Module to run, the `~/.bashrc` needs to contain `export PYTHONPATH=${PYTHONPATH}:/PATH/TO/PYTHON`.

## Bash

### How to get it to run

Assuming you are in the folder in which the spec file is located, you can run `sudo kdb mount $(pwd)/spec spec/autocomplete/name_of_program -f ni` to mount the file to `spec/autocomplete/name_of_program`.
If a program has multiple commands, there needs to be one file for each command at the location `spec/autocomplete/name_of_program/name_of_command` mounted at the location `$(pwd)/spec spec/autocomplete/name_of_program/name_of_command`.
To get the bash completion to run you need to run `source find_autocompletion_options.sh`. The specifiaction File that should be used needs to be mounted
After that in your console you should be able to type `kdb <TAB><TAB>` and have completion options show up.

### How to Run Tests

Currently it is necessary to call the test-script from the folder the `find_autocompletion_options.py` is located.
From this folder call `python3 test_SOME_TEST.py`.
