/* This code is moved to a separate file to make its removal easier */

#ifndef KDBCAP_H
#define KDBCAP_H

typedef struct _KDBCap	KDBCap;

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif


/* Old capability API */
KDBCap *kdbGetCapability(KDB *handle, const Key *where);
int kdbhGetErrno(const KDB *handle);

/* Capability methods */
KDBCap* kdbhSetCapability(KDB *handle, KDBCap *cap);
KDBCap* kdbhGetCapability(const KDB *handle);

KDBCap *capNew (void);
void capDel (KDBCap *cap);

const char *kdbcGetName (const KDBCap *cap);
const char *kdbcGetVersion (const KDBCap *cap);
const char *kdbcGetDescription (const KDBCap *cap);
const char *kdbcGetAuthor (const KDBCap *cap);
const char *kdbcGetLicence (const KDBCap *cap);

/* too many functions, use flags */
unsigned int kdbcGetonlyFullGet (const KDBCap *cap);
unsigned int kdbcGetonlyFullSet (const KDBCap *cap);
unsigned int kdbcGetonlyRemoveAll (const KDBCap *cap);
unsigned int kdbcGetonlyAddKeys (const KDBCap *cap);
unsigned int kdbcGetonlySystem (const KDBCap *cap);
unsigned int kdbcGetonlyUser (const KDBCap *cap);
unsigned int kdbcGetonlyFullSync (const KDBCap *cap);
unsigned int kdbcGetnoOwner (const KDBCap *cap);
unsigned int kdbcGetnoValue (const KDBCap *cap);
unsigned int kdbcGetnoComment (const KDBCap *cap);
unsigned int kdbcGetnoUID (const KDBCap *cap);
unsigned int kdbcGetnoGID (const KDBCap *cap);
unsigned int kdbcGetnoMode (const KDBCap *cap);
unsigned int kdbcGetnoDir (const KDBCap *cap);
unsigned int kdbcGetnoATime (const KDBCap *cap);
unsigned int kdbcGetnoMTime (const KDBCap *cap);
unsigned int kdbcGetnoCTime (const KDBCap *cap);
unsigned int kdbcGetnoRemove (const KDBCap *cap);
unsigned int kdbcGetnoStat (const KDBCap *cap);
unsigned int kdbcGetnoMount (const KDBCap *cap);
unsigned int kdbcGetnoBinary (const KDBCap *cap);
unsigned int kdbcGetnoString (const KDBCap *cap);
unsigned int kdbcGetnoTypes (const KDBCap *cap);
unsigned int kdbcGetnoError (const KDBCap *cap);
unsigned int kdbcGetnoLock (const KDBCap *cap);
unsigned int kdbcGetnoThread (const KDBCap *cap);

#ifdef __cplusplus
}
}
#endif


/**
 * The private Kdb Capability structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs. Use the "KDBCapability access methods" instead.
 * Only a backend writer needs to have access to the private attributes of the
 * KeySet object which is defined as:
 * @code
typedef struct _KDBCap KDBCap;
 * @endcode
 * 
 * @see kdbGetCapabilty()
 * @see commandInfo() of the 'kdb info' command to see it in action
 * @ingroup capability
 */
struct _KDBCap {
        /* we'll point only to static strings. We won't allocate anything for each member. */
	const char *version;		/*!< Version of the library. */
	const char *name;		/*!< Name of backend being or that will be used. */
	const char *description;	/*!< Any text describing the backend. */
	const char *author;		/*!< The author of the backend. */
	const char *licence;		/*!< The licence of the backend,
				  because of BSD licence even commercial is allowed. */
	unsigned int onlyFullGet:1;	/*!< You can't get specific keys. */
	unsigned int onlyRemoveAll:1;	/*!< You can only remove all keys at once. */
	unsigned int onlyAddKeys:1;	/*!< When setting keys, they will be added. */
	unsigned int onlyFullSet:1;	/*!< All keys need to be in keyset when setting. */
	unsigned int onlySystem:1;	/*!< Only system namespace supported. */
	unsigned int onlyUser:1;	/*!< Only user namespace supported. */
	unsigned int noOwner:1;		/*!< The backend does not support a owner of keys. */
	unsigned int noValue:1;		/*!< No value is supported in the backend. */
	unsigned int noComment:1;	/*!< No comment is supported in the backend. */
	unsigned int noUID:1;		/*!< No uid is supported in the backend. */
	unsigned int noGID:1;		/*!< No gid is supported in the backend. */
	unsigned int noMode:1;		/*!< No mode is supported in the backend. */
	unsigned int noDir:1;		/*!< Directories are not supported in the backend. */
	unsigned int noATime:1;		/*!< Mode Time not supported. */
	unsigned int noMTime:1;		/*!< Modification Time not supported. */
	unsigned int noCTime:1;		/*!< Meta Info Change Time not supported. */
	unsigned int noRemove:1;	/*!< The backend does not support removing keys. */
	unsigned int noStat:1;		/*!< When getting keys they can't be stated. */
	unsigned int noMount:1;		/*!< Mount type not supported. */
	unsigned int noBinary:1;	/*!< Binary types not supported. */
	unsigned int noString:1;	/*!< String types not supported. */
	unsigned int noTypes:1;		/*!< Typing of keys is not supported. */
	unsigned int noError:1;		/*!< Don't expect errno to be set correctly. */
	unsigned int noLock:1;		/*!< Backend does not lock. */
	unsigned int noThread:1;	/*!< Backend uses global variables and is not threadsafe. */
};

#endif
