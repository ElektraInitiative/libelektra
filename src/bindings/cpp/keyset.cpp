#include <keyset>

namespace kdb {

KeySet::KeySet (size_t alloc, va_list ap)
{
	ks = ckdb::ksVNew (alloc, ap);
}

KeySet::KeySet (size_t alloc, ...)
{
	va_list va;

	va_start (va, alloc);
	ks = ckdb::ksVNew (alloc, va);
	va_end (va);
}

KeySet::KeySet (const KeySet &other)
{
	ks = ckdb::ksNew(0);
	ckdb::ksCopy(ks, other.ks);
}

KeySet::~KeySet ()
{
	ckdb::ksDel (ks);
}

size_t KeySet::size () const
{
	return ckdb::ksGetSize(ks);
}

void KeySet::rewind () const
{
	ckdb::ksRewind(ks);
}


void KeySet::setCursor(cursor_t cursor)
{
	ckdb::ksSetCursor(ks, cursor);
}

cursor_t KeySet::getCursor() const
{
	return ckdb::ksGetCursor(ks);
}


Key KeySet::next()
{
	ckdb::Key *k = ckdb::ksNext(ks);
	if (k==0) throw KeySetOutOfRange();
	return Key(k);
}

Key KeySet::current() const
{
	return Key(ckdb::ksCurrent(ks));
}

Key KeySet::head() const
{
	return Key(ckdb::ksHead(ks));
}

Key KeySet::tail() const
{
	return Key(ckdb::ksTail(ks));
}

Key KeySet::pop()
{
	ckdb::Key *k = ckdb::ksPop(ks);
	if (k==0) throw KeySetOutOfRange();
	Key key(k);
	return key;
}

Key KeySet::lookup (const Key &key, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookup(ks, key.getKey(), options);
	if (k==0) throw KeySetNotFound();
	return Key(k);
}

Key KeySet::lookup (const std::string &name, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookupByName(ks, name.c_str(), options);
	if (k==0) throw KeySetNotFound();
	return Key(k);
}

Key KeySet::lookup (const char *name, const option_t options) const
{
	ckdb::Key *k = ckdb::ksLookupByName(ks, name, options);
	if (k==0) throw KeySetNotFound();
	return Key(k);
}

size_t KeySet::append (const Key &toAppend)
{
	return ckdb::ksAppendKey(ks, toAppend.getKey());
}

size_t KeySet::append (const KeySet &toAppend)
{
	return ckdb::ksAppend(ks, toAppend.getKeySet());
}

void KeySet::sort()
{
	ckdb::ksSort(ks);
}

/*
ssize_t KeySet::toStream(FILE* stream, option_t options) const
{
	return ckdb::ksToStream (getKeySet(), stream, options);
}

ssize_t KeySet::output(FILE* stream, option_t options) const
{
	return ckdb::ksOutput (getKeySet(), stream, options);
}

ssize_t KeySet::generate(FILE* stream, option_t options) const
{
	return ckdb::ksGenerate(getKeySet(), stream, options);
}
*/

std::ostream & operator << (std::ostream & os, const KeySet &k)
{
	os << "Keyset";
	return os;
}

std::istream & operator >> (std::istream & is, KeySet &k)
{
	return is;
}


} // end of KeySet

