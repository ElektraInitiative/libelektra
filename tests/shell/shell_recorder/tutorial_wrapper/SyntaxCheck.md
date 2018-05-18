# Syntax Check

- Test 1

  ```sh
  # Backup-and-Restore:/test
  # testcomment
  kdb set /test/a a
  kdb set /test/b b
  # RET:0
  # should yield 'a'
  kdb get /test/a
  #> a
  # STDERR:
  kdb get /test/c
  # Expected:
  # RET:11
  # STDERR:Did not find key '/test/c'
  kdb rm -r /test
  ```

- Test 2

  ```sh
  kdb set /test/x x
  kdb set /test/y y
  kdb get /test/x
  #> x
  kdb get /test/y
  #> y
  kdb export /test ini
  # STDOUT-REGEX: (\[\]⏎)?x = x⏎y = y
  kdb ls /test
  kdb rm -r /test
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
