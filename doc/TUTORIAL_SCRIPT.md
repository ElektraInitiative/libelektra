# Script

```shell script
docker run -it elektra/elektra                                      > NEW LINE

sudo kdb mount hello.js /hello json
kdb set user/hello/hello/world "Hello World"
kdb get user/hello/hello/world
kdb file user/hello/hello/world
vi /home/elektra/.config/hello.js
# Edit file to say "Hello World from File!"
kdb get user/hello/hello/world
kdb editor user/hello ini
# Edit file to say "Hello World from INI!"
kdb get user/hello/hello/world
echo "Elektra can export to various formats!"
kdb export user/hello
kdb export user/hello tcl
kdb export user/hello yaml                                      > NEW LINE
echo "Remove entries and file"
kdb rm user/hello/hello/world
cat /home/elektra/.config/hello.ini
sudo kdb umount /hello
echo "Now let us change something.. With validation"
vi specification.ini                                       > NEW START
# content:
# []
# mountpoint = specification.ini
# [hostip]
# check/ipaddr =
# [timeout]
# check/range = 0-100
sudo kdb mount /home/elektra/specification.ini spec/validation ni
sudo kdb spec-mount /validation
kdb set /validation/hostip 123.123.123.1233
kdb set /validation/timeout 500
kdb editor spec/validation ni
#edit range to 1000
echo "Edit specification to fit needs"
kdb set /validation/timeout 500
sudo kdb umount spec/validation
kdb rm -r /validation                                           > NEW END
echo "What to do next???"
kdb
kdb --help
kdb --find-tools
```
