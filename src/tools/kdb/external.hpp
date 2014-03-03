#ifndef EXTERNAL_HPP
#define EXTERNAL_HPP

#include <stdexcept>

struct UnknownCommand : std::exception
{};

void tryExternalCommand(char** argv);

#endif
