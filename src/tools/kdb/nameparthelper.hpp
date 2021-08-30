/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef NAMEPARTHELPER_HPP_
#define NAMEPARTHELPER_HPP_

#include <kdb.hpp>
#include <cmdline.hpp>
#include <string>

using namespace kdb;
using namespace std;

string keyNamespace (Key const & key);
string keyBasename (Key const & key);
string keyDirname (Key const & key);
string keyDirkey (Key const & key);
int executeNamepartcommand (Cmdline const & cl, string (*namepart_getter)(Key const &));

#endif /* NAMEPARTHELPER_HPP_ */
