#!/bin/sh

XFCONF_QUERY_COMMAND="xfconf-query"

eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /general/FailsafeSessionName -s Failsafe -n -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client0_Command -s "xfwm4" -a -n -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client0_PerScreen -s false -n -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client0_Priority -s 15 -n -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client1_Command -s "xfsettingsd" -a -n -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client1_PerScreen -s false -n -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client1_Priority -s 20 -n -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client2_Command -s "xfce4-panel" -a -n -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client2_PerScreen -s false -n -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client2_Priority -s 25 -n -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client3_Command -s "Thunar" -s "--daemon" -a -n -t string -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client3_PerScreen -s false -n -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client3_Priority -s 30 -n -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client4_Command -s "xfdesktop" -a -n -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client4_PerScreen -s false -n -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Client4_Priority -s 35 -n -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/Count -s 5 -n -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-session -p /sessions/Failsafe/IsFailsafe -s true -n -t bool

eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/ThemeName -n -s "Adwaita" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/IconThemeName -n -s "elementary" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/DoubleClickTime -n -s 400 -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/DoubleClickDistance -n -s 5 -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/DndDragThreshold -n -s 8 -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/Blink -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/SoundThemeName -n -s "default" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/EnableEventSounds -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Net/EnableInputFeedbackSounds -n -s true -t bool

eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Xft/Antialias -n -s 1 -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Xft/Hinting -n -s "-1" -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Xft/HintStyle -n -s "hintfull" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Xft/RGBA -n -s "none" -t string

eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/CanChangeAccels -n -s false -t bool
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/ColorPalette -n -s "black:white:gray50:red:purple:blue:light blue:green:yellow:orange:lavender:brown:goldenrod4:dodger blue:pink:light green:gray10:gray30:gray75:gray90" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/FontName -n -s "Sans 10" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/MonospaceFontName -n -s "Monospace 10" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/IconSizes -n -s "" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/KeyThemeName -n -s "" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/ToolbarStyle -n -s "icons" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/ToolbarIconSize -n -s 3 -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/MenuImages -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/ButtonImages -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/MenubarAccel -n -s "F10" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/CursorThemeName -n -s "" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/CursorThemeSize -n -s 0 -t int
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/DecorationLayout -n -s "menu:minimize,maximize,close" -t string
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/DialogsUseHeader -n -s false -t bool
eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gtk/TitlebarMiddleClick -n -s "lower" -t string

eval "$XFCONF_QUERY_COMMAND" -c xsettings -p /Gdk/WindowScalingFactor -n -s 1 -t int
