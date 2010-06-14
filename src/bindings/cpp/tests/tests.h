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

#define warn_if_fail(x,y) {++nbTest; if (!(x)) { cout << __FILE__ << ":" << __LINE__ << " warning in " <<  __FUNCTION__ << ": " << y << endl;}}
#define succeed_if(x,y) {++nbTest; if (!(x)) { cout << __FILE__ << ":" << __LINE__ << " error in " <<  __FUNCTION__ << ": " << y << endl; ++nbError;}}
#define exit_if_fail(x,y) {++nbTest; if (!(x)) { cout << __FILE__ << ":" << __LINE__ << " fatal in " <<  __FUNCTION__ << ": " << y << endl;  exit(1);}}

