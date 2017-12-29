# Crash Test

The following Markdown Shell Recorder test checks that the INI plugin does not crash, if the input file contains invalid characters or is
not properly formatted.

```sh
for file in $(find -E src/plugins/ini/crash_test -regex '.*crash[0-9]{3}.ini$'); do                       \
	cat "$file" | kdb import user/examples/ini ini 2>&1 | grep -q 'SIG' && echo "File $file caused a crash" \
	kdb rm -rf user/examples/ini 2>&1 | grep -q 'SIG' && echo "File $file caused a crash"                   \
	# Check if we successfully removed all keys                                                             \
	kdb ls user/examples/ini                                                                                \
done                                                                                                      \
echo 'OK'
#> OK
```
