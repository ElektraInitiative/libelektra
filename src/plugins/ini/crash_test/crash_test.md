# Crash Test

The following Markdown Shell Recorder test checks that the INI plugin does not crash, if the input file contains invalid characters or is
not properly formatted.

```sh
sudo kdb mount config_crash_test.ini user/examples/ini ini

for file in $(find -E src/plugins/ini/crash_test -regex '.*crash[0-9]{3}.ini$'); do \
	cp -f "$file" "$(kdb file user/examples/ini)"                                     \
	# Check if the file causes a crash of the INI plugin                              \
	kdb ls user/examples/ini 2>&1 | grep -q 'SIG' && echo "File $file caused a crash" \
	rm "$(kdb file user/examples/ini 2> /dev/null)"                                   \
done                                                                                \
echo 'OK'
#> OK

sudo kdb umount user/examples/ini
```
