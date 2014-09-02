/***************************************************************************
                     xmltool.c  -  Skeleton of a plugin
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
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef ELEKTRA_PLUGIN_XMLTOOL_H
#define ELEKTRA_PLUGIN_XMLTOOL_H

#include <kdbplugin.h>
#include <stdio.h>

ssize_t ksToStream(const KeySet *ks, FILE* stream, option_t options);
int ksFromXML(KeySet *ks, int fd);
size_t elektraStrLen(const char *s);

int elektraXmltoolOpen(Plugin *handle, Key *errorKey);
int elektraXmltoolClose(Plugin *handle, Key *errorKey);
int elektraXmltoolGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraXmltoolSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraXmltoolError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(xmltool);

#endif
