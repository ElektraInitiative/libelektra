- infos = Information about the curlget plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = getresolver setresolver commit rollback
- infos/status = configurable readonly preview unfinished
- infos/metadata =
- infos/description = mount remote config files via curl

## Description

The `curlget` plugin is a resolver using libcurl to upload and download files from/to remote hosts. When mounted with a `URL` as configuration file there will be no changes to the file system. When mounted with a (local) path to a configuration a copy of the remote configuration is kept and used as fallback in `kdbGet()` if fetching the remote file from the server fails.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-curl`.

## Configuration

### definitions

`URL`:

an URL has to be prefixed with the protocol. valid protocols: `http://`, `https://`, `ftp://`, `ftps://`, `scp://`, `sftp://`, `smb://` (currently not supported)

`Filename`:

can bei either an `URL` or a local configuration file.

if the filename is an `URL` the plugin operates on temporary files only and keeps no local copy of the configuration. unless specified otherwise the `URL` is used for both upload and download.

### plugin configuration

- `url`:

  `URL` used for both upload and download of the configuration. might be overwritten by `url/get` and `url/put`.

- `url/get`:

  the `URL` of the remote configuration file.

- `url/put`:

  the `URL` used to upload the configuration on kdbSet

- `upload/method`:

  only used for `HTTP` requests. use `POST` for `POST`-requests, or `PUT` for `PUT`-requests.

- `upload/postfield`:

  for `HTTP POST`-requests: the name of the field containing the file

- `upload/filename`:

  name of the uploaded file. if present it will be appended to `url/put` on uploads except for `HTTP POST` uploads where it overrides the `filename` field of the header.
  if not specified the value defaults to `url/put` is assumed to be a valid upload `URL` already containing the filename, except for `HTTP POST`-requests where the `basename(3)` of the local mounted configuration file is used if present, or of `url/get`

- `user`:

  username for authentication

- `password`:

  password for authentication

- `ssl/verify`:

  if set `1` enforce the use of `SSL`. `HOSTNAME` and `PEER` verifications are enabled but might be overwritten by `ssl/verify/host` and `ssl/verify/peer`.

  if not set or set to `0` the plugin will try to use `SSL` but not fail if not possible.

- `ssl/verify/host`:

  if set to `1` use hostname verification, if set to `0`, skip it.

- `ssl/verify/peer`:

  if set to `1` verify the ssl certificate, if set to `0`, skip the verification.

- `prefer`:

  if set to `local` don't update the configuration if the remote version has changed **within** succeeding `kdbGet()` calls.

- `ssh/auth`:

  ssh authentication method for sftp and scp. possible values: `agent` for using `ssh-agent`, `password` for password authentication, `pubkey` for public key authentication and `pubkeypw` for public key + password authentication.

- `ssh/key`:

  path of the private key file for ssh public key authentication. if not set, default to `$HOME/.ssh/id_dsa` or `$HOME/.ssh/id_rsa`

- `ssh/key/passwd`:

  password for the private key file

## Example

```sh
rm /tmp/curltest.ini || $(exit 0)
sudo kdb mount -R curlget -c url/get="http://127.0.0.1:8000/curltest.ini",url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" /tmp/curltest.ini system:/curl ini
kdb ls system:/curl
#> system:/curl/section1
#> system:/curl/section1/key1
stat /tmp/curltest.ini
# RET:0
kdb set system:/curl/section1/key2 val2
sudo kdb umount system:/curl
stat /tmp/curltest.ini
# RET:0
cat /tmp/curltest.ini
#> [section1]
#> key1=val1
#> key2=val2
rm /tmp/curltest.ini || $(exit 0)
sudo kdb mount -R curlget -c url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" "http://127.0.0.1:8000/curltest.ini" system:/curl ini
kdb ls system:/curl
#> system:/curl/section1
#> system:/curl/section1/key1
#> system:/curl/section1/key2
stat /tmp/curltest.ini
# RET:1
mv /tmp/httproot/curltest.ini /tmp/httproot/curltest.ini_moved
kdb ls system:/curl
# RET:5
mv /tmp/httproot/curltest.ini_moved /tmp/httproot/curltest.ini
kdb rm system:/curl/section1/key2
sudo kdb umount system:/curl
cat /tmp/httproot/curltest.ini
#> [section1]
#> key1=val1
```

### Mount with HTTP GET + POST and keep local copy

```
kdb mount -R curlget -c url/get="http://127.0.0.1:8000/curltest.ini",url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" /tmp/curltest.ini system:/curl ini
```

### Mount with HTTP GET + POST and keep no local copys

```
kdb mount -R curlget -c url/put="http://127.0.0.1:8000",user="thomas",password="pass",upload/method="POST",upload/postfield="file" "http://127.0.0.1:8000/curltest.ini" system:/curl ini
```

### Mount with FTP GET + PUT and keep local copy

```
kdb mount -R curlget -c url/get="ftp://127.0.0.1:21/test.ini",url/put="ftp://127.0.0.1:21/test.ini",user="thomas",password="pass",upload/method="FTP" /tmp/curltest.ini system:/curl ini
```
