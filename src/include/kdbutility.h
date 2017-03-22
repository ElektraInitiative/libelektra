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

#ifdef __cplusplus
}
}
#endif


#endif
