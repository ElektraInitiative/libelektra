# Syntax Check

- Test 1

  ```sh
  # Backup-and-Restore:/tests/msr
  # testcomment
  kdb set /tests/msr/a a
  kdb set /tests/msr/b b
  # RET:0
  # should yield 'a'
  kdb get /tests/msr/a
  #> a
  # STDERR:
  kdb get /tests/msr/c
  # Expected:
  # RET:11
  # STDERR:Did not find key '/tests/msr/c'
  kdb rm -r /tests/msr
  ```

- Test 2

  ```sh
  kdb set /tests/msr/x x
  kdb set /tests/msr/y y
  kdb get /tests/msr/x
  #> x
  kdb get /tests/msr/y
  #> y
  kdb export /tests/msr mini
  # STDOUT-REGEX: (\[\]⏎)?x=x⏎y=y
  kdb ls /tests/msr
  kdb rm -r /tests/msr
  ```

- Test 3

  ```sh
  ls

  echo test

  #> test

  printf 'test\nbla'
  #> test

  #> bla
  if [ -e `kdb file user` ]; then cat `kdb file user`; fi
  ```

- Sudo

  ```sh
  sudo echo `sudo kdb file system`
  sudo ls
  echo `    sudo kdb file system`
  ```
