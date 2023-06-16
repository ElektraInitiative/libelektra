# Syntax Check

- Test 1

  ```sh
  # Backup-and-Restore:user:/tests/msr
  # testcomment
  kdb set user:/tests/msr/a a
  kdb set user:/tests/msr/b b
  # RET:0
  # should yield 'a'
  kdb get user:/tests/msr/a
  #> a
  # STDERR:
  kdb get user:/tests/msr/c
  # Expected:
  # RET:11
  # STDERR:.*Did not find key 'user:/tests/msr/c'
  kdb rm -r user:/tests/msr
  ```

- Test 2

  ```sh
  kdb set user:/tests/msr/x x
  kdb set user:/tests/msr/y y
  kdb get user:/tests/msr/x
  #> x
  kdb get user:/tests/msr/y
  #> y
  kdb export user:/tests/msr mini
  # STDOUT-REGEX: (\[\]⏎)?x=x⏎y=y
  kdb ls user:/tests/msr
  kdb rm -r user:/tests/msr
  ```

- Test 3

  ```sh
  ls

  echo test

  #> test

  printf 'test\nbla'
  #> test

  #> bla
  if [ -e `kdb file user:/` ]; then cat `kdb file user:/`; fi
  ```

- Sudo

  ```sh
  sudo echo `sudo kdb file system:/`
  sudo ls
  echo `    sudo kdb file system:/`
  ```
