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

It is also possible to replace Xfconf entirely with Elektra.
The Xfconf binding in Elektra implements the Xfconf API in such a way that it can be used as a drop-in replacement.

### Setup

To start, the Xfconf library must be replaced with the Xfconf binding.
Replacing the system libraries may require root privileges.

```shell
# Use Elektra instead of Xfconf system-wide
kdb xfconf-system-lib-replace

# Use Elektra instead of Xfconf only for the current user
kdb xfconf-user-lib replace

# When replacing only the users library, either restart the session or run
source ~/.xprofile
```

Note that a system upgrade may reset the system library replacement.

All applications that normally use Xfconf will now use Elektra instead.
This can be verified using both `xfconf-query` and `kdb`.

```shell
# Set an Xfconf property with Elektra
kdb set system:/sw/org/xfce/xfconf/test/hello world
#> Create a new key system:/sw/org/xfce/xfconf/test/hello with string "world"

# Verify it with Xfconf
xfconf-query -c test -p hello
#> world
```

### Run Xfce Using Elektra

The Xfce desktop provides some default settings which are loaded on first login.
However, if you are using Elektra instead of Xfconf, you will need to load these defaults manually.
This can be done entirely using `kdb`.

```shell
kdb xfconf-populate
```

Once this is done, an Xfce session can be started either from the login manager or manually.
From this point on, the Xfce desktop will read and write to the Elektra database.
However, Xfce also uses some properties that are not stored in Xfconf.
These properties will not work properly.
An example of such a property is the Gtk theme.

### Reverting the Libraries

If you want to stop using Elektra instead of Xfconf, you have to choose the right option depending on how you did the setup.

If you did the setup using the system libraries, it is sufficient to run it:

```shell
kdb xfconf-system-lib-restore
```

If you have chosen the user only way, you will need to edit your `~/.xprofile` file and remove the statements where the `LD_*` variables contain anything related to Xfconf.

## Data-Types Used in Xfconf
