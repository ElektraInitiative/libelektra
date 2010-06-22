/***************************************************************************
            template.h  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid plugin.                              *
 *   Replace all occurences of "template" with your plugin name.           *
 *   Simple fill the empty template functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/

#ifndef ELEKTRA_PLUGIN_template_H
#define ELEKTRA_PLUGIN_template_H

#include <kdbplugin.h>


int kdbOpen_template(Plugin *handle, Key *errorKey);
int kdbClose_template(Plugin *handle, Key *errorKey);
int kdbGet_template(Plugin *handle, KeySet *ks, Key *parentKey);
int kdbSet_template(Plugin *handle, KeySet *ks, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(template);

#endif
