# Autocompletion
To get autocompletion to run you need to have python3 installed and the kdb-python Module up and running.
For the kdb-python Module to run, the `~/.bashrc` needs to contain `export PYTHONPATH=${PYTHONPATH}:/PATH/TO/PYTHON`.

## Bash
### How to get it to run
First you need to mount the specification file with kdb by running `sudo kdb mount ~/PATH/TO/SPEC/FILE/spec_ spec/tests/autocomplete -f ni`.
To get the bash completion to run you need to run `source find_autocompletion_options.sh`. The specifiaction File that should be used needs to be mounted
After that in your console you should be able to type `kdb <TAB><TAB>` and have completion options show up.

### How to Run Tests
Currently it is necessary to call the test-script from the folder the `find_autocompletion_options.py` is located.
From this folder call `python3 test_SOME_TEST.py`.