#include "public.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

///// API description
//
// refcounts start at 1
// *Retain() creates new reference
// *Release() destroys one reference, and if it was the last reference also the object itself
// elektraSetInsert() uses existing reference


void useEntry (ElektraEntry * e1);

// assume this function is not under our control
void fooExternalDanger (ElektraSet * s, ElektraEntry * e)
{
	elektraSetInsert (s, e);
}

void fooConsume (ElektraEntry * e1, ElektraEntry * e2)
{
	// REFS: e1.refs = n, e2.refs = n (with n >= 1)

	ElektraSet * set = elektraSetNew (2);

	// we want to use e1 independently of set
	// -> retain new reference before inserting
	elektraEntryRetain (e1);
	elektraSetInsert (set, e1);
	// REFS: e1.refs = n + 1, e2.refs = n

	useEntry (e1);

	elektraSetInsert (set, e2);
	// REFS(A): e1.refs = n + 1, e2.refs = n
	// REFS(B): e1.refs = n, e2.refs = n
	//
	// NOTE: there are two cases from now on, because inserting entry with same name (e2),
	//       removes previous entry (e1) i.e., elektraEntryRelease(e1) is called

	useEntry (e1);

	// we no longer need e1
	elektraEntryRelease (e1);
	// REFS(A): e1.refs = n, e2.refs = n
	// REFS(B): e1.refs = n - 1, e2.refs = n (e1 is deleted if n == 1)


	elektraSetRelease (set);
	// REFS(A): e1.refs = n - 1, e2.refs = n - 1 (e1, e2 are deleted if n == 1)
	// REFS(B): e1.refs = n - 1, e2.refs = n - 1 (e1, e2 are deleted if n == 1)
	//
	// NOTE: in case B set only contains e2

	// REFS: e1.refs = n - 1, e2.refs = n - 1 (e1, e2 are deleted if n == 1)
}

// same as fooConsume, but without deleting entries
void fooKeep (ElektraEntry * e1, ElektraEntry * e2)
{
	// REFS: e1.refs = n, e2.refs = n (with n >= 1)

	ElektraSet * set = elektraSetNew (2);

	// we want to keep e1
	// -> retain new reference before inserting
	elektraEntryRetain (e1);
	elektraSetInsert (set, e1);
	// REFS: e1.refs = n + 1, e2.refs = n

	useEntry (e1);

	// we want to keep e2
	// -> retain new reference before inserting
	elektraEntryRetain (e2);
	elektraSetInsert (set, e2);
	// REFS(A): e1.refs = n + 1, e2.refs = n + 1
	// REFS(B): e1.refs = n, e2.refs = n + 1
	//
	// NOTE: again two cases, because of names

	useEntry (e1);

	elektraSetRelease (set);
	// REFS(A): e1.refs = n, e2.refs = n
	// REFS(B): e1.refs = n, e2.refs = n
	//
	// NOTE: in case B set only contains e2

	// REFS: e1.refs = n, e2.refs = n
}

int main (void)
{
	///// basic examples

	ElektraEntry * e1 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e1.refs = 1
	ElektraEntry * e2 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e1.refs = 1, e2.refs = 1

	fooKeep (e1, e2);
	// REFS: e1.refs = 1, e2.refs = 1

	useEntry (e1);
	useEntry (e2);

	elektraEntryRelease (e1);
	// REFS: e1 deleted, e2.refs = 1
	elektraEntryRelease (e2);
	// REFS: e1 deleted, e2 deleted


	ElektraEntry * e3 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e3.refs = 1
	ElektraEntry * e4 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e3.refs = 1, e4.refs = 1

	fooConsume (e3, e4);
	// REFS: e3 deleted, e4 deleted

	// not possible:
	// useEntry(e3);
	// useEntry(e4);


	///// defensive examples

	ElektraEntry * e5 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e5.refs = 1
	ElektraEntry * e6 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e5.refs = 1, e6.refs = 1

	// unnecessary, but fooKeep may not document what it does w.r.t refcounts
	elektraEntryRetain (e5);
	// REFS: e5.refs = 1, e6.refs = 1
	elektraEntryRetain (e6);
	// REFS: e5.refs = 1, e6.refs = 1

	fooKeep (e5, e6);
	// REFS: e5.refs = 1, e6.refs = 1

	useEntry (e5);
	useEntry (e6);

	elektraEntryRelease (e5);
	// REFS: e5 deleted, e6.refs = 1
	elektraEntryRelease (e6);
	// REFS: e5 deleted, e6 deleted


	ElektraEntry * e7 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e7.refs = 1
	ElektraEntry * e8 = elektraEntryNew (ELEKTRA_NS_SYSTEM, (ElektraName){ .name = "foo\0bar\0baz", .size = 14 });
	// REFS: e7.refs = 1, e8.refs = 1

	elektraEntryRetain (e7);
	// REFS: e7.refs = 1, e8.refs = 1
	elektraEntryRetain (e8);
	// REFS: e7.refs = 1, e8.refs = 1

	fooConsume (e7, e8);
	// REFS: e7.refs = 1, e8.refs = 1

	useEntry (e7);
	useEntry (e8);

	elektraEntryRelease (e7);
	// REFS: e7 deleted, e8.refs = 1
	elektraEntryRelease (e8);
	// REFS: e7 deleted, e8 deleted

	return 0;
}