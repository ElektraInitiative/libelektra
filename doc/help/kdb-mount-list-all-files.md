# kdb-mount-list-all-files -- List all mounted files

## SYNOPSIS

`kdb mount-list-all-files`

## DESCRIPTION

This command prints a sorted list of all files under the control of Elektra

## EXAMPLES

```
kdb mount
#> none on system:/info/elektra/constants with name system:/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system:/info/elektra/contract/#0 with name system:/info/elektra/contract/#0
#> none on system:/info/elektra/desktop with name system:/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system:/info/elektra/metadata/#0 with name system:/info/elektra/metadata/#0
#> none on system:/info/elektra/uname with name system:/info/elektra/uname
#> /tmp/test.ini on system:/test with name system:/test

kdb mount-list-all-files
#> none
#> /tmp/test.ini
#> /usr/local/share/doc/elektra/CONTRACT.ini
#> /usr/local/share/doc/elektra/METADATA.ini
```
