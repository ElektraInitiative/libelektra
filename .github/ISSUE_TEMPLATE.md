Please remove this template if you have
a question or proposal and do not want
to report an issue.

# Steps to Reproduce the Problem

Please provide a step by step guide on how to reproduce the problem here. If applicable, please use
[Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) syntax:

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

.

# Expected Result

Please describe what should happen if you follow the steps described above.

# Actual Result

Please describe what actually happened.

# System Information

- Elektra Version: master
- Versions of other relevant software?

# Further Log Files and Output
