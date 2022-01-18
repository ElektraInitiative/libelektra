# kdb-mountpoint-info -- Print information about the default storage and resolver or a mount point

## SYNOPSIS

`kdb mountpoint-info [mount point]`

## DESCRIPTION

This command will print information about the version, default resolver and default storage plugin. When a mount point is given as argument additional information about the mount point (e.g. configuration) is printed.

## EXAMPLES

```
kdb mountpoint-info
#> Version: 0.8.19
#> Default resolver: resolver_fm_hpu_b
#> Default storage: ini

kdb mount /tmp/test.ini system:/test ini hello=ini -c hello=world

kdb mountpoint-info system:/test
#> Version: 0.8.19
#> Default resolver: resolver_fm_hpu_b
#> Default storage: ini
#> Mountpoint: system:/test
#> File: /tmp/test.ini
#>	   config:
#>         hello = world
#>         path = /tmp/test.ini
#> getplugins:
#>         #0#resolver
#>         #5#ini#ini#
#>		   config:
#>                 hello = ini
#> setplugins:
#>         #0#resolver
#>         #5#ini
#>         #6#sync#sync#
#>         #7#resolver
#> errorplugins:
#>         #5#resolver_fm_hpu_b#resolver#
```
