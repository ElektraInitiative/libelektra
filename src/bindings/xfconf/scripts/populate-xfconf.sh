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

eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /configver -n -s 2 -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels -n -s 1 -s 2 -a -t int -t int
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/dark-mode -n -s true -t bool

eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1 -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1/position -n -s "'p=6;x=0;y=0'" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1/length -n -s 100 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1/position-locked -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1/icon-size -n -s 16 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1/size -n -s 26 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-1/plugin-ids -n -a -s 1 -s 2 -s 3 -s 4 -s 5 -s 6 -s 7 -s 8 -s 9 -s 10 -s 11 -s 12 -s 13 -s 14 -t int -t int -t int -t int -t int -t int -t int -t int -t int -t int -t int -t int -t int -t int

eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2 -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2/position -n -s "'p=10;x=0;y=0'" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2/length -n -s 1 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2/position-locked -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2/icon-size -n -s 48 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2/size -n -s 26 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /panels/panel-2/plugin-ids -n -a -s 15 -s 16 -s 17 -s 18 -s 19 -s 20 -s 21 -s 22 -t int -t int -t int -t int -t int -t int -t int -t int

eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-1 -n -s "applicationsmenu" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-2 -n -s "tasklist" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-2/grouping -n -s 1 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-3 -n -s "separator" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-3/expand -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-3/style -n -s 0 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-4 -n -s "pager" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-5 -n -s "separator" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-5/style -n -s 0 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-6 -n -s "systray" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-6/square-icons -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-8 -n -s "pulseaudio" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-8/enable-keyboard-shortcuts -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-8/show-notifications -n -s true -t bool
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-9 -n -s "power-manager-plugin" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-10 -n -s "notification-plugin" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-11 -n -s "separator" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-11/style -n -s 0 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-12 -n -s "clock" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-13 -n -s "separator" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-13/style -n -s 0 -t uint
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-14 -n -s "actions" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-15 -n -s "showdesktop" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-16 -n -s "separator" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-17 -n -s "launcher" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-17/items -n -a -s "xfce4-terminal-emulator.desktop" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-18 -n -s "launcher" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-18/items -n -a -s "xfce4-file-manager.desktop" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-19 -n -s "launcher" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-19/items -n -a -s "xfce4-web-browser.desktop" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-20 -n -s "launcher" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-20/items -n -a -s "xfce4-appfinder.desktop" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-21 -n -s "separator" -t string
eval "$XFCONF_QUERY_COMMAND" -c xfce4-panel -p /plugins/plugin-22 -n -s "directorymenu" -t string
