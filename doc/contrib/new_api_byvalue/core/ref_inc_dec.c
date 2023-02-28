#include "public.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

///// API description
//
// refcounts start at 0
// *IncRef() creates new reference
// *DecRef() destroys one reference
// *Del() destroys the object, if there are no more references
// elektraSetInsert() creates new reference internally

void useEntry (ElektraEntry * e1);

// assume this function is not under our control
void fooExternalDanger (ElektraSet * s, ElektraEntry * e)
{
	elektraSetInsert (s, e);
}

void fooConsume (ElektraEntry * e1, ElektraEntry * e2)
{
	// REFS: e1.refs = n, e2.refs = n (with n >= 0)

	ElektraSet * set = elektraSetNew (2);

	// we want to use e1 independently of set
	// -> increment refcount
	elektraEntryIncRefCount (e1);
	// REFS: e1.refs = n + 1, e2.refs = n

	elektraSetInsert (set, e1);
	// REFS: e1.refs = n + 2, e2.refs = n

	useEntry (e1);

	elektraSetInsert (set, e2);
	// REFS(A): e1.refs = n + 2, e2.refs = n + 1
	// REFS(B): e1.refs = n + 1, e2.refs = n + 1
	//
	// NOTE: there are two cases from now on, because inserting entry with same name (e2),
	//       removes previous entry (e1) i.e., refcount of e1 is decremented and without the
	//       elektraEntryIncRefCount(e1) above, e1 would be destroyed

	useEntry (e1);

	// we no longer need e1
	elektraEntryDecRefCount (e1);
	// REFS(A): e1.refs = n + 1, e2.refs = n + 1
	// REFS(B): e1.refs = n, e2.refs = n + 1

	// because we might have the last reference (case B)
	// we must call del on e1, otherwise we leak it

	// REFS(A): e1.refs = n + 1, e2.refs = n + 1
	// REFS(B): e1.refs = n, e2.refs = n + 1
	elektraEntryDel (e1);
	// REFS(A): e1.refs = n + 1, e2.refs = n + 1
	// REFS(B): e1.refs = n, e2.refs = n + 1 (e1 is deleted if n == 0)
	elektraSetDel (set);
	// REFS(A): e1.refs = n, e2.refs = n (e1, e2 are deleted if n == 0)
	// REFS(B): e1.refs = n, e2.refs = n (e1, e2 are deleted if n == 0)
	//
	// NOTE: in case B set only contains e2

	// REFS: e1.refs = n, e2.refs = n (e1, e2 are deleted if n == 0)
}

// same as fooConsume, but without deleting entries
void fooKeep (ElektraEntry * e1, ElektraEntry * e2)
{
	// REFS: e1.refs = n, e2.refs = n

	// increment refcounts, because we don't want to delete e1 or e2
	elektraEntryIncRefCount (e1);
	elektraEntryIncRefCount (e2);
	// REFS: e1.refs = n + 1, e2.refs = n + 1

	ElektraSet * set = elektraSetNew (2);

	// no need for extra increment on e1, we already did it

	elektraSetInsert (set, e1);
	// REFS: e1.refs = n + 2, e2.refs = n + 1

	useEntry (e1);

	elektraSetInsert (set, e2);
	// REFS(A): e1.refs = n + 2, e2.refs = n + 2
	// REFS(B): e1.refs = n + 1, e2.refs = n + 2
	//
	// NOTE: again two cases, because of names

	useEntry (e1);

	// we no longer need e1, but we decrement at the end

	// we don't call del on e1, beccause we don't want to delete it

	// REFS(A): e1.refs = n + 2, e2.refs = n + 2
	// REFS(B): e1.refs = n + 1, e2.refs = n + 2
	elektraSetDel (set);
	// REFS(A): e1.refs = n + 1, e2.refs = n + 1
	// REFS(B): e1.refs = n + 1, e2.refs = n + 1
	//
	// NOTE: in case B set only contains e2, therefore e1.refs is unchanged

	// restore refcounts
	elektraEntryDecRefCount (e1);
	elektraEntryDecRefCount (e2);

	// REFS(A): e1.refs = n, e2.refs = n
	// REFS(B): e1.refs = n, e2.refs = n
}

int main (void)
{
	///// basic examples

	ElektraEntry * e1 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e1.refs = 0
	ElektraEntry * e2 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e1.refs = 0, e2.refs = 0

	fooKeep (e1, e2);
	// REFS: e1.refs = 0, e2.refs = 0

	useEntry (e1);
	useEntry (e2);

	elektraEntryDel (e1);
	// REFS: e1 deleted, e2.refs = 0
	elektraEntryDel (e2);
	// REFS: e1 deleted, e2 deleted


	ElektraEntry * e3 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e3.refs = 0
	ElektraEntry * e4 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e3.refs = 0, e4.refs = 0

	fooConsume (e3, e4);
	// REFS: e3 deleted, e4 deleted

	// not possible:
	// useEntry(e3);
	// useEntry(e4);


	///// defensive examples

	ElektraEntry * e5 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e5.refs = 0
	ElektraEntry * e6 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e5.refs = 0, e6.refs = 0

	// unnecessary, but fooKeep may not document what it does w.r.t refcounts
	elektraEntryIncRefCount (e5);
	// REFS: e5.refs = 1, e6.refs = 0
	elektraEntryIncRefCount (e6);
	// REFS: e5.refs = 1, e6.refs = 1

	fooKeep (e5, e6);
	// REFS: e5.refs = 1, e6.refs = 1

	useEntry (e5);
	useEntry (e6);

	elektraEntryDecRefCount (e5);
	// REFS: e5.refs = 0, e6.refs = 1
	elektraEntryDecRefCount (e6);
	// REFS: e5.refs = 0, e6.refs = 0

	elektraEntryDel (e5);
	// REFS: e5 deleted, e6.refs = 0
	elektraEntryDel (e6);
	// REFS: e5 deleted, e6 deleted


	ElektraEntry * e7 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e7.refs = 0
	ElektraEntry * e8 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e7.refs = 0, e8.refs = 0

	elektraEntryIncRefCount (e7);
	// REFS: e7.refs = 1, e8.refs = 0
	elektraEntryIncRefCount (e8);
	// REFS: e7.refs = 1, e8.refs = 1

	fooConsume (e7, e8);
	// REFS: e7.refs = 1, e8.refs = 1

	useEntry (e7);
	useEntry (e8);

	// unnecessary, but fooConsume may not document what it does w.r.t refcounts
	elektraEntryDecRefCount (e7);
	elektraEntryDecRefCount (e8);

	elektraEntryDel (e7);
	// REFS: e7 deleted, e8.refs = 0
	elektraEntryDel (e8);
	// REFS: e7 deleted, e8 deleted

	return 0;
}