#ifndef ELEKTRA_PLUGIN_TOML_CODEPOINT_H
#define ELEKTRA_PLUGIN_TOML_CODEPOINT_H

#include <stdbool.h>

int utf8FromUnicode (const char * codepoint, int len, unsigned char * utf8);
bool validUtf8FromUnicode (const char * codepointStr, int codepointLen);
int utf8LenFromHeadChar (unsigned char head);

#endif // ELEKTRA_PLUGIN_TOML_CODEPOINT_H
