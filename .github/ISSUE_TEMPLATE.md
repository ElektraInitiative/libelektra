Please remove this template if you have
a question or proposal and do not want
to report a bug.

## Steps to Reproduce the Problem

Please provide a step by step guide on how to reproduce the problem here. If possible, please use
[Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper)
syntax:

```sh
kdb set /examples/hello world
#> Using name user/examples/hello
#> Create a new key user/examples/hello with string "world"

kdb get /examples/hello
#> world

kdb get user/does/not/exist
# RET: 1
# STDERR: [Dd]id not find key

kdb rm /examples/hello
```

If your key database (KDB) might influence the outcome, please use `kdb stash`
to temporarily have an empty KDB. (Restore instructions are printed.)

## Expected Result

Please describe what should happen if you follow the steps described above.

## Actual Result

Please describe what actually happened.

## System Information

- Elektra Version: master
- Versions of other relevant software?

## Further Log Files and Output
