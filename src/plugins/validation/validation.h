/***************************************************************************
                     validation.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_VALIDATION_H
#define ELEKTRA_PLUGIN_VALIDATION_H

#include <sys/types.h>
#include <regex.h>

#include <kdbplugin.h>
#include <kdberrors.h>

int elektraValidationOpen(Plugin *handle, Key *errorKey);
int elektraValidationClose(Plugin *handle, Key *errorKey);
int elektraValidationGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraValidationSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraValidationError(Plugin *handle, KeySet *ks, Key *parentKey);

Key *ksLookupRE(KeySet *ks, const regex_t *regexp);

Plugin *ELEKTRA_PLUGIN_EXPORT(validation);

#endif
