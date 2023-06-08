# Using Xfconf with Elektra

Elektra provides the ability to work hand in hand with Xfconf.
Depending on what you want, you can use Elektra in different ways.

## Altering Existing Xfconf Settings

Elektra provides an Xfconf storage plugin that allows you to change all the configuration settings made by Xfconf.

To access the properties which are owned by Xfconf, the appropriate channel must be mounted.
Some known channel names are

- **Thunar**: thunar
- **Xfwm**: xfwm4
- **Xfce Panel**: xfce4-panel

Assuming that the properties of Thunar are to be changed, the following command can be used to mount the channel.

```shell
kdb mount -R noresolver none /sw/xfce4/thunar xfconf channel=thunar
```

**Warning**: The following operations will cause permanent changes to your system, please handle with care.

Thunar should now be ready to be configured by Elektra.
For example, the following command can be used to use single clicks instead of double clicks.

```shell
# Setting the value to `false` beforehand
xfconf-query -c thunar -p /misc-single-click -s false -t bool -n

kdb set /sw/xfce4/thunar/misc-single-click TRUE
#> Set string to "TRUE"
#> Using name dir:/sw/xfce4/thunar/misc-single-click
```

The result can then be verified using both Elektra and `xfconf-query`.

```shell
kdb get /sw/xfce4/thunar/misc-single-click
#> TRUE

xfconf-query -c thunar -p /misc-single-click
#> true
```

Using a text editor, you can also view the changes in the
file `${XDG_CONFIG_HOME:-$HOME/.config}/xfce4/xfconf/xfce-perchannel-xml/thunar.xml`.

When you are finished configuring the Xfce component, you should unmount the corresponding channel with the following command.

```shell
kdb umount /sw/xfce4/thunar/
```

## Replacing Xfconf with Elektra

## Data-Types Used in Xfconf
