# Crash Test

The following Markdown Shell Recorder test checks that the INI plugin does not crash, if the input file contains invalid characters or is
not properly formatted.

```sh
sudo kdb mount config.ini user/examples/ini ini

for file in $(find -E src/plugins/ini/crash_test -regex '.*crash[0-9]{3}.ini$'); do \
	cp -f "$file" "$(kdb file user/examples/ini)"                                     \
	kdb ls user/examples/ini                                                          \
	rm "$(kdb file user/examples/ini)"                                                \
done

sudo kdb umount user/examples/ini
```
