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

Assuming that the properties of Thunar anf Xfwm are to be changed, the following command can be used to mount the channels.
If you are operating in a headless console session (e.g. docker), make sure that dbus is running.
If it is not, `export $(dbus-launch)` can be used for that.

```shell
touch none
kdb mount -R noresolver none /sw/xfce4/thunar xfconf channel=thunar
kdb mount -R noresolver none /sw/xfce4/xfwm4 xfconf channel=xfwm4
```

**Warning**: The following operations will cause permanent changes to your system, please handle with care.

Thunar should now be ready to be configured by Elektra.
The following commands can be used to configure some selected options of Xfce.

```shell
# Use the details view instead of the grid view in Thunar
kdb set system:/sw/xfce4/thunar/last-view ThunarDetailsView
#> Create a new key system:/sw/xfce4/thunar/last-view with string "ThunarDetailsView"

# Do not perform a recursive search on network directories
kdb set system:/sw/xfce4/thunar/misc-recursive-search THUNAR_RECURSIVE_SEARCH_LOCAL
#> Create a new key system:/sw/xfce4/thunar/misc-recursive-search with string "THUNAR_RECURSIVE_SEARCH_LOCAL"

# Move the window buttons to the left
kdb set system:/sw/xfce4/xfwm4/general/button_layout "CM|O"
#> Create a new key system:/sw/xfce4/xfwm4/general/button_layout with string "CM|O"
```

The result can then be verified using both Elektra and `xfconf-query`.

```shell
kdb get /sw/xfce4/thunar/last-view
#> ThunarDetailsView

xfconf-query -c thunar -p /last-view
#> ThunarDetailsView

kdb get /sw/xfce4/thunar/misc-recursive-search
#> THUNAR_RECURSIVE_SEARCH_LOCAL

xfconf-query -c thunar -p /misc-recursive-search
#> THUNAR_RECURSIVE_SEARCH_LOCAL

kdb get /sw/xfce4/xfwm4/general/button_layout
#> CM|O

xfconf-query -c thunar -p /xfwm4/general/button_layout
#> CM|O
```

Using a text editor, you can also view the changes in the
file `${XDG_CONFIG_HOME:-$HOME/.config}/xfce4/xfconf/xfce-perchannel-xml/thunar.xml`.
However, this file may not be up-to-date since the Xfconf daemon has a caching mechanism.

When you are finished configuring the Xfce component, you should unmount the corresponding channel with the following command.

```shell
kdb umount /sw/xfce4/thunar/
kdb umount /sw/xfce4/xfwm4/
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
kdb xfconf-user-lib-replace && source ~/.xprofile
```

Note that a system upgrade may reset the system library replacement.

All applications that normally use Xfconf will now use Elektra instead.
This can be verified using both `xfconf-query` and `kdb`.

```shell
# Set an Xfconf property with Elektra
kdb set system:/sw/org/xfce/xfconf/test/hello world
#> Create a new key system:/sw/org/xfce/xfconf/test/hello with string "world"

# Verify it with Xfconf
xfconf-query -c test -p /hello
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

If you have chosen the user only way, you will need to edit your `~/.xprofile` file and remove the statements where the `LD_*` variables
contain anything related to Xfconf.

## Data-Types Used in Xfconf

Xfconf uses the glib2 type system.
This system is not fully compatible with Elektra.
However, all values are stored in Elektra in their string representation when used as a drop-in replacement.
In addition, the name of the glib2 type is stored in Elektra as a metakey.
This makes Xfconf's type system completely independent of Elektra, at the cost of memory and computational resources.

The following table shows a comparison between the different type systems.

| Xfconf   | Elektra                    | Xfce Property Example (Channel, Property)        | Note                                                                                         |
| :------- | -------------------------- | ------------------------------------------------ | -------------------------------------------------------------------------------------------- |
| `string` | `char*`                    | `xfce4-panel`, `/panels/panel-1/position`        |                                                                                              |
| `uchar`  | `kdb_octet_t`              |                                                  | Not implemented in the Xfconf binding.                                                       |
| `char`   | `kdb_char_t`               |                                                  | Not implemented in the Xfconf binding.                                                       |
| `uint16` | `kdb_unsigned_short_t`     |                                                  | Not implemented in the Xfconf binding.                                                       |
| `int16`  | `kdb_short_t`              |                                                  | Not implemented in the Xfconf binding.                                                       |
| `uint`   | `kdb_unsigned_long_t`      | `xfce4-panel`, `/panels/panel-1/length`          |                                                                                              |
| `int`    | `kdb_long_t`               | `xfce4-session`, `/sessions/Count`               |                                                                                              |
| `uint64` | `kdb_unsigned_long_long_t` |                                                  |                                                                                              |
| `int64`  | `kdb_long_long_t`          |                                                  |                                                                                              |
| `float`  | `kdb_float_t`              |                                                  |                                                                                              |
| `double` | `kdb_double_t`             |                                                  |                                                                                              |
| `bool`   | `kdb_boolean_t`            | `xfce4-panel`, `/panels/panel-1/position-locked` |                                                                                              |
| `array`  |                            | `xfce4-panel`, `/panels`                         | No direct type in Elektra. The key name structure determines whether it is an array or not.  |
| `empty`  |                            | `xfce4-panel`, `/panels/panel-1`                 | Does not exist in Elektra. Paths in Elektra which are no leaf nodes will be equally handled. |
