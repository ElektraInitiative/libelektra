/**Some common functions in use for testing framework*/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <kdb>

#include <exception>
#include <iostream>
#include <string>
#include <cstring>

using namespace std;
using namespace kdb;

extern int nbError;
extern int nbTest;

#define warn_if_fail(x,y) {nbTest++; if (!(x)) { printf("%s:%d: warn in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); }}
#define succeed_if(x,y) nbTest++; if (!(x)) { nbError++; printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y);}
#define exit_if_fail(x,y) nbTest++; if (!(x)) { printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); exit(1); }

