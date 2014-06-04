#include "xmltool.h"

/*
 * Compare 2 KeySets.
 *
 * TODO: this is not really comparing
 * TODO: untested and buggy, does not work with array
 *
 * This method behavior is the following:
 * - A key (by full name) that is present on @p ks1 and @p ks2, and has
 *   something different, will be transfered from @p ks2 to @p ks1, and
 *   @p ks1's (old) version deleted.
 * - Keys present in @p ks1, but not in @p ks2 will be transfered from @p ks1
 *   to @p removed.
 * - Keys that are keyCompare() equal in @p ks1 and @p ks2 will be
 *   keyDel()eted from @p ks2.
 * - Keys present in @p ks2 but not in @p ks1 will
 *   be transfered to @p ks1.
 *
 * In the end, @p ks1 will have all the keys that matter, and @p ks2
 * will be empty.
 *
 * After ksCompare(), you should, in this order:
 * -# ksDel(ks2)
 * -# call kdbSetKeys() on @p ks1 to commit all changed keys
 * -# kdbRemoveKey() for all keys in the @p removed KeySet
 * -# ksDel(removed)
 *
 * @param ks1 first and main KeySet
 * @param ks2 second KeySet
 * @param removed (generally empty) KeySet that will be filled with keys
 * 	removed from @p ks1
 * @see keyCompare()
 * @see commandEdit() at the kdb command
 * @return always 0
 * @par Example
 * @code
KeySet *ks1,*ks2,*removed;
Key *key;

ks1=ksNew(0);
ks2=ksNew(0);
removed=ksNew(0);

// ...
// Populate ks1 and ks2....
// ...

ksCompare(ks1,ks2,removed);

ksDel(ks2);  // second KeySet is allways empty after ksCompare()
kdbSetKeys(ks1); // commit changed keys
ksDel(ks1);  // don't need ks1 anymore

// Remove all keys that disapeared from ks1...
while (key=ksPop(removed)) {
	kdbRemoveKey(key);
	keyDel(key);
}

ksDel(removed); // free the KeySet memory
 * @endcode
 */
#if 0
int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed) {
	int flagRemoved=1;
	Key *ks1Cursor=0;
	Key *ks2Cursor=0;

	Key *ks1PrevCursor=0;

	ks1Cursor=ks1->start;
	while (ks1Cursor) {
		Key *ks2PrevCursor=0;
		flagRemoved=1;
		
		for (ks2Cursor=ks2->start; ks2Cursor; ks2Cursor=ks2Cursor->next) {
			uint32_t flags=keyCompare(ks1Cursor,ks2Cursor);
			
			if (!(flags & (KEY_SWITCH_NAME | KEY_SWITCH_OWNER))) {
				/* Comparing fullname-equal keys */
				flagRemoved=0; /* key was not removed */
					
				/* First remove from ks2 */
				if (ks2PrevCursor) ks2PrevCursor->next=ks2Cursor->next;
				else ks2->start=ks2Cursor->next;
				if (ks2->end==ks2Cursor) ks2->end=ks2PrevCursor;
				ks2->size--;
					
				/* Now check if we still can find differences between keys, but
				 * we are not interested in the NEEDSYNC flag: he allone is not
				 * enough to determine a key as different */
				if (flags & ~KEY_FLAG_SYNC) {
					/* keys are different. Transfer to ks1. */
					
					/* Put in ks1 */
					if (ks1PrevCursor) ks1PrevCursor->next=ks2Cursor;
					else ks1->start=ks2Cursor;
					if (ks1->end==ks1Cursor) ks1->end=ks2Cursor;
					ks2Cursor->next=ks1Cursor->next;
					
					/* delete old version */
					keyDel(ks1Cursor);
					
					/* Reset pointers */
					ks1Cursor=ks2Cursor;
				} else {
					/* Keys are identical. Delete ks2's key. */

					/* Delete ks2Cusrsor */
					keyDel(ks2Cursor);
				}
				/* Don't need to walk through ks2 anymore */
				break;
			}
			ks2PrevCursor=ks2Cursor;
			
		} /* ks2 iteration */
		
		if (flagRemoved) {
			/* This ks1 key was not found in ks2 */
			/* Transfer it from ks1 to removed */
			
			/* Remove from ks1 */
			if (ks1PrevCursor) ks1PrevCursor->next=ks1Cursor->next;
			else ks1->start=ks1Cursor->next;
			if (ks1->end==ks1Cursor) ks1->end=ks1PrevCursor;
			ks1->size--;

			/* Append to removed */
			ksAppendKey(removed,ks1Cursor);
			
			/* Reset pointers */
			if (ks1PrevCursor) ks1Cursor=ks1PrevCursor->next;
			else ks1Cursor=ks1->start;
		} else {
			ks1PrevCursor=ks1Cursor;
			ks1Cursor=ks1Cursor->next;
		}
	} /* ks1 iteration */
	
	/* Now transfer all remaining ks2 keys to ks1 */
	ksAppend(ks1,ks2);
	
	return 0;
}
#endif

