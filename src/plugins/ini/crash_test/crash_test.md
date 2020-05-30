# Crash Test

The following Markdown Shell Recorder test checks that the INI plugin does not crash, if the input file contains invalid characters or is
not properly formatted.

```sh
for file in $(find src/plugins/ini/crash_test -regex '.*crash[0-9][0-9]*.ini$' | sort); do                \
	cat "$file" | kdb import user:/tests/ini ini 2>&1 | grep -q 'SIG' && echo "File $file caused a crash"    \
	kdb rm -rf user:/tests/ini 2>&1 | grep -q 'SIG' && echo "File $file caused a crash"                      \
	# Check if we successfully removed all keys                                                             \
	kdb ls user:/tests/ini                                                                                   \
done
#>
```
