/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef EXTERNAL_HPP
#define EXTERNAL_HPP

#include <stdexcept>
#include <string>

struct UnknownCommand : std::exception
{};

void elektraExecve(const char *filename, char *const argv[]);
void tryExternalCommand(char** argv);
void runManPage(std::string command);

#endif
