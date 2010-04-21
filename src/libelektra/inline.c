/*Returns the key where a meta value is stored.
  This should not passed to the user because it
  is an implementation detail.

  Is implemented in a own file so that it can be
  included in tests/test_meta.c
 */
static inline Key *keyMetaKey(const Key* key, const char* metaName)
{
	Key *ret;
	Key *search;

	if (!key) return 0;
	if (!metaName) return 0;
	if (!key->meta) return 0;

	search = keyNew (KEY_END);
	search->key = kdbiStrDup(metaName);

	if (!search->key) return 0; /*Duplication did not work*/

	ret = ksLookup(key->meta, search, 0);

	keyDel (search);

	return ret;
}
