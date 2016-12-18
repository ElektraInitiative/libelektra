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

### GET + POST ###

kdb mount -R curlget -c get="http://127.0.0.1:8000/curltest.ini",upload="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" /tmp/curltest.ini system/curl ini 

### FTP GET + PUT ###

kdb mount -R curlget -c get="ftp://127.0.0.1:21/test.ini",upload="ftp://127.0.0.1:21/test.ini",user="thomas",password="pass",upload/method="FTP" /tmp/curltest.ini system/curl ini 

