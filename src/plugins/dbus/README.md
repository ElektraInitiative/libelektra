- infos = Information about the dbus plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = notification
- infos/placements = postcommit
- infos/description = Prints timestamps when a method is called.

## Introduction ##

This plugin is a notification plugin which sends a signal to dbus when a method is called. This allows external programs to take action when dbus notifies the program that a certain method has taken place with Elektra. 

## Usage ##

to log notification events:

	dbus-monitor type='signal',interface='org.libelektra',path='/org/libelektra/configuration'	
