#ifndef EXTERNAL_HPP
#define EXTERNAL_HPP

#include <stdexcept>
#include <string>

struct UnknownCommand : std::exception
{};

void tryExternalCommand(char** argv);

void runManPage(std::string command);

#endif
