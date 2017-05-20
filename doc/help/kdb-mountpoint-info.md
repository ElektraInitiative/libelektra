kdb-mountpoint-info -- Print information about the default storage and resolver or a mountpoint
======================================================================================

## SYNOPSIS

`kdb mountpoint-info [mountpoint]`

## DESCRIPTION

This command will print information about the version, default resolver and default storage plugin. When a mountpoint is given as argument additional informations about the mountpoint (e.g. configuration) is printed.

## EXAMPLES

```
kdb info_2
#> Version: 0.8.19
#> Default resolver: resolver_fm_hpu_b
#> Default storage: ini

kdb mount /tmp/test.ini system/test ini hello=ini -c hello=world

kdb info system/test
#> Version: 0.8.19
#> Default resolver: resolver_fm_hpu_b
#> Default storage: ini
#> Mountpoint: system/test
#> File: /tmp/test.ini
#>                 hello = world
#>                 path = /tmp/test.ini
#> getplugins:
#>         #0#resolver
#>         #5#ini#ini#
#>                 hello = ini
#> setplugins:
#>         #0#resolver
#>         #5#ini
#>         #6#sync#sync#
#>         #7#resolver
#> errorplugins:
#>         #5#resolver_fm_hpu_b#resolver#
```
