#ifndef EXTERNAL_HPP
#define EXTERNAL_HPP

#include <stdexcept>
#include <string>

struct UnknownCommand : std::exception
{};

void elektraExecve(const char *filename, char *const argv[]);
void tryExternalCommand(char** argv);
void runManPage(std::string command);
void runEditor(std::string file);

#endif
