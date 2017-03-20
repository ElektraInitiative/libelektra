#ifndef KDBUTILITY_H
#define KDBUTILITY_H

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/* Convenience Methods for String Handling */
char * lskip (char const * const keyname);
char * rstrip (char * const start, char ** end);
char * strip (char * text);

#ifdef __cplusplus
}
}
#endif


#endif
