- infos = Information about the curlget plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = getresolver setresolver commit rollback
- infos/status = unittest configurable readonly preview unfinished nodoc
- infos/metadata =
- infos/description = mount remote config files via curl

## Example ##

```sh
rm /tmp/curltest.ini || $(exit 0)
kdb mount -R curlget -c url/get="http://127.0.0.1:8000/curltest.ini",url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" /tmp/curltest.ini system/curl ini 
kdb ls system/curl
#> system/curl/section1
#> system/curl/section1/key1
stat /tmp/curltest.ini
# RET:0
kdb umount system/curl
rm /tmp/curltest.ini || $(exit 0)
kdb mount -R curlget -c url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" "http://127.0.0.1:8000/curltest.ini" system/curl ini 
kdb ls system/curl
#> system/curl/section1
#> system/curl/section1/key1
stat /tmp/curltest.ini
# RET:1
mv /tmp/httproot/curltest.ini /tmp/httproot/curltest.ini_moved
kdb ls system/curl
# RET:5
kdb umount system/curl
mv /tmp/httproot/curltest.ini_moved /tmp/httproot/curltest.ini
```



### GET + POST ###


### FTP GET + PUT ###

kdb mount -R curlget -c url/get="ftp://127.0.0.1:21/test.ini",url/put="ftp://127.0.0.1:21/test.ini",user="thomas",password="pass",upload/method="FTP" /tmp/curltest.ini system/curl ini 


### no local copy ###

kdb mount -R curlget -c url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" "http://127.0.0.1:8000/curltest.ini" system/curl ini 
