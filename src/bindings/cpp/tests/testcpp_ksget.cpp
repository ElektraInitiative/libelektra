/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keysetget.hpp>

#include <tests.hpp>

TEST (ks, get)
{
	KeySet ks (5, *Key ("user:/map", ELEKTRA_KEY_END), *Key ("user:/map/a", ELEKTRA_KEY_END), *Key ("user:/map/b", ELEKTRA_KEY_END),
		   *Key ("user:/map/c", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	EXPECT_EQ (ks.get<std::string> ("user:/map/c"), "value");
	EXPECT_THROW (ks.get<std::string> ("user:/map/x"), KeyNotFoundException);
	typedef std::map<std::string, std::string> map;
	map m = ks.get<map> ("user:/map");
	EXPECT_EQ (m.size (), 3);
	EXPECT_EQ (m["a"], "");
	EXPECT_EQ (m["b"], "");
	EXPECT_EQ (m["c"], "value");
	EXPECT_EQ (m.size (), 3) << "we inserted a value during test";
}

namespace kdb
{

template <>
struct KeySetTypeWrapper<int>
{
	int operator() (KeySet const & ks, std::string const & name, elektraLookupFlags const options) const
	{
		Key k = ks.lookup (name, options);
		if (!k) return -5;
		if (k.getStringSize () <= 1) return -3;
		return k.get<int> () + 5;
	}
};

class Point
{
public:
	int x, y;

	Point (int a, int b) : x (a), y (b)
	{
	}
};

bool operator== (Point const & our, Point const & other)
{
	return our.x == other.x && our.y == other.y;
}

ostream & operator<< (ostream & os, Point const & p)
{
	os << p.x << " " << p.y;
	return os;
}

template <>
struct KeySetTypeWrapper<Point>
{
	Point operator() (KeySet const & ks, std::string const & name, elektraLookupFlags const options) const
	{
		Key x = ks.lookup (name + "/x", options);
		if (!x) throw KeyNotFoundException (name + "/x not found");
		Key y = ks.lookup (name + "/y", options);
		if (!y) throw KeyNotFoundException (name + "/y not found");

		return Point (x.get<int> (), y.get<int> ());
	}
};
} // namespace kdb

TEST (ks, getOwnType)
{
	KeySet ks (5, *Key ("user:/owntype", ELEKTRA_KEY_END), *Key ("user:/owntype/a", ELEKTRA_KEY_END), *Key ("user:/owntype/b", ELEKTRA_KEY_END),
		   *Key ("user:/owntype/c", ELEKTRA_KEY_VALUE, "20", ELEKTRA_KEY_END), *Key ("user:/owntype/x", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END),
		   *Key ("user:/owntype/y", ELEKTRA_KEY_VALUE, "12", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	EXPECT_EQ (ks.get<int> ("user:/owntype/a"), -3);
	EXPECT_EQ (ks.get<int> ("user:/owntype/c"), 25);
	EXPECT_EQ (ks.get<int> ("user:/owntype/n"), -5);
	EXPECT_EQ (ks.get<Point> ("user:/owntype"), Point (5, 12));
}

TEST (ks, getCascading)
{
	KeySet ks (5, *Key ("user:/map", ELEKTRA_KEY_END), *Key ("user:/map/a", ELEKTRA_KEY_END), *Key ("user:/map/b", ELEKTRA_KEY_END),
		   *Key ("user:/map/c", ELEKTRA_KEY_VALUE, "winvalue", ELEKTRA_KEY_END), *Key ("user:/map/d", ELEKTRA_KEY_VALUE, "dvalue", ELEKTRA_KEY_END),
		   *Key ("system:/map", ELEKTRA_KEY_END), *Key ("system:/map/a", ELEKTRA_KEY_END), *Key ("system:/map/b", ELEKTRA_KEY_END),
		   *Key ("system:/map/c", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_END), *Key ("system:/map/e", ELEKTRA_KEY_VALUE, "evalue", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	EXPECT_EQ (ks.get<std::string> ("/map/c"), "winvalue");
	typedef std::map<std::string, std::string> map;
	map m = ks.get<map> ("/map");
	EXPECT_EQ (m.size (), 5);
	EXPECT_EQ (m["a"], "");
	EXPECT_EQ (m["b"], "");
	EXPECT_EQ (m["c"], "winvalue");
	EXPECT_EQ (m["d"], "dvalue");
	EXPECT_EQ (m["e"], "evalue");
	EXPECT_EQ (m.size (), 5);
}
