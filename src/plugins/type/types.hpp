/**
 * @file
 *
 * @brief Implementation of data types
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_TYPES_HPP
#define ELEKTRA_TYPES_HPP

#include <locale>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include <key.hpp>
#include <keyset.hpp>

#include <iostream>


namespace elektra
{

using namespace kdb;
using namespace std;

class Type
{
public:
	virtual bool check (Key k) = 0;
	virtual ~Type ();
};

class AnyType : public Type
{
public:
	bool check (Key) override { return true; }
};

class EmptyType : public Type
{
public:
	bool check (Key k) override { return k.getString ().empty (); }
};

class StringType : public Type
{
public:
	bool check (Key k) override { return !k.getString ().empty (); }
};

template <typename T>
class TType : public Type
{
public:
	bool check (Key k) override
	{
		istringstream i (k.getString ());
		i.imbue (locale ("C"));
		T n;
		i >> n;
		if (i.bad ())
			return false;
		if (i.fail ())
			return false;
		if (!i.eof ())
			return false;
		return true;
	}
};

/**
  * Reversible Type
  * This checks even more pedantic, if the type is reversible
  * to the same string.
  * E.g. -1 might get to highest value (but this is not guaranteed)!
  * */
template <typename T>
class RType : public Type
{
public:
	bool check (Key k) override
	{
		istringstream i (k.getString ());
		i.imbue (locale ("C"));
		T n;
		i >> n;
		if (i.bad ())
			return false;
		if (i.fail ())
			return false;
		if (!i.eof ())
			return false;

		ostringstream o;
		o << n;
		if (o.str () != k.getString ())
			return false;

		return true;
	}
};

/**
  * Reversible Type with min, max values
  * This checks even more pedantic, if the type is reversible
  * to the same string.
  * E.g. -1 might get to highest value (but this is not guaranteed)!
  * */
template <typename T>
class MType : public Type
{
public:
	bool check (Key k) override
	{
		istringstream i (k.getString ());
		i.imbue (locale ("C"));
		T n;
		i >> n;
		if (i.bad ())
			return false;
		if (i.fail ())
			return false;
		if (!i.eof ())
			return false;

		ostringstream o;
		o.imbue (locale ("C"));
		o << n;
		if (o.fail ())
			return false;
		if (o.str () != k.getString ())
			return false;

		Key const min = k.getMeta<const Key> ("check/type/min");
		if (min)
		{
			istringstream i_min (min.getString ());
			i_min.imbue (locale ("C"));
			T n_min;
			i_min >> n_min;
			if (i_min.bad ())
				return false;
			if (i_min.fail ())
				return false;
			if (!i_min.eof ())
				return false;
			if (n < n_min)
				return false;
		}

		Key const max = k.getMeta<const Key> ("check/type/max");
		if (max)
		{
			istringstream i_max (max.getString ());
			i_max.imbue (locale ("C"));
			T n_max;
			i_max >> n_max;
			if (i_max.bad ())
				return false;
			if (i_max.fail ())
				return false;
			if (!i_max.eof ())
				return false;
			if (n > n_max)
				return false;
		}

		return true;
	}
};

class FSType : public Type
{
	std::set<std::string> choices;

public:
	FSType ()
	{
		choices.insert ("auto");
		choices.insert ("swap");
		choices.insert ("adfs");
		choices.insert ("affs");
		choices.insert ("autofs");
		choices.insert ("cifs");
		choices.insert ("coda");
		choices.insert ("coherent");
		choices.insert ("cramfs");
		choices.insert ("debugfs");
		choices.insert ("devpts");
		choices.insert ("efs");
		choices.insert ("ext");
		choices.insert ("ext2");
		choices.insert ("ext3");
		choices.insert ("ext4");
		choices.insert ("hfs");
		choices.insert ("hfsplus");
		choices.insert ("hpfs");
		choices.insert ("iso9660");
		choices.insert ("jfs");
		choices.insert ("minix");
		choices.insert ("msdos");
		choices.insert ("ncpfs");
		choices.insert ("nfs");
		choices.insert ("nfs4");
		choices.insert ("ntfs");
		choices.insert ("proc");
		choices.insert ("qnx4");
		choices.insert ("ramfs");
		choices.insert ("reiserfs");
		choices.insert ("romfs");
		choices.insert ("smbfs");
		choices.insert ("sysv");
		choices.insert ("tmpfs");
		choices.insert ("udf");
		choices.insert ("ufs");
		choices.insert ("umsdos");
		choices.insert ("usbfs");
		choices.insert ("vfat");
		choices.insert ("xenix");
		choices.insert ("xfs");
		choices.insert ("xiafs");
	}

	bool check (Key k) override
	{
		std::string label = k.getString ();
		size_t oldpos = 0;
		size_t pos = label.find (',');
		;
		while (pos != string::npos)
		{
			std::string type = label.substr (oldpos, pos - oldpos);
			if (choices.find (type) == choices.end ())
				return false;

			oldpos = pos + 1;
			pos = label.find (',', oldpos);
		}

		std::string lastType = label.substr (oldpos, string::npos);
		if (choices.find (lastType) == choices.end ())
			return false;

		return true;
	}
};

} // end namespace elektra

#endif
