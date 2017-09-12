#ifndef KDBUTILITY_H
#define KDBUTILITY_H

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/* Convenience Methods for String Handling */
char * elektraLskip (char const * const keyname);
char * elektraRstrip (char * const start, char ** end);
char * elektraStrip (char * text);
char * elektraReplace (char const * const text, char const * const pattern, char const * replacement);

#ifdef __cplusplus
}
}
#endif


#endif
