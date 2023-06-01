#ifndef ELEKTRA_LOOKUP_H
#define ELEKTRA_LOOKUP_H

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * @brief More lookup options
 *
 * @ingroup proposal
 */
enum elektraLookupOptions
{
	KDB_O_SPEC = 1 << 15,	     ///< Use the passed key as specification, instead of looking up the specification first
	KDB_O_CREATE = 1 << 16,	     ///< Create the key if it was not found
	KDB_O_NOCASCADING = 1 << 17, ///< Disable cascading search for keys starting with /
	KDB_O_NOSPEC = 1 << 18,	     ///< Do not use specification for cascading keys (internal)
	KDB_O_NODEFAULT = 1 << 19,   ///< Do not honor the default spec (internal)
	KDB_O_CALLBACK = 1 << 20,    ///< For spec:/ lookups that traverse deeper into hierarchy (callback in ksLookup())
	KDB_O_OPMPHM = 1 << 21,	  ///< Overrule ksLookup search predictor to use OPMPHM, make sure to set ENABLE_OPTIMIZATIONS=ON at cmake
	KDB_O_BINSEARCH = 1 << 22 ///< Overrule ksLookup search predictor to use Binary search for lookup
};

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_LOOKUP_H
