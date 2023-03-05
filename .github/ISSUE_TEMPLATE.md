<!--
Please remove this template if you have a question or proposal and do not want to report a bug.
-->

## Steps to Reproduce the Problem

<!--
Please provide a step by step guide on how to reproduce the problem here.
If possible, please use [Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) syntax:

```sh
kdb set user:/tests/hello world
#> Create a new key user:/tests/hello with string "world"

kdb get user:/tests/hello
#> world

kdb get user:/tests/hello/does/not/exist
# RET: 11
# STDERR: [Dd]id not find key 'user:/tests/hello/does/not/exist'

kdb rm user:/tests/hello
```

If your key database (KDB) might influence the outcome, please use `kdb stash` to temporarily have an empty KDB.
(Restore instructions are printed.)
-->

## Expected Result

<!--
Please describe what should happen if you follow the steps described above.
-->

## Actual Result

<!--
Please describe what actually happened.
-->

## System Information

- Elektra: master
- Operating System: <!-- which OS with which version was used? -->

<!--
- Versions of other relevant software?
-->

## Further Log Files and Output

<!--
Please add further information here.
-->
