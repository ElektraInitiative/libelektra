/**
 * @file
 *
 * @brief Elektra Error.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_ERROR_PRIVATE_H
#define ELEKTRA_ERROR_PRIVATE_H

struct _ElektraError
{
    int code;
    const char * message;
};

#endif //ELEKTRA_ERROR_PRIVATE_H
