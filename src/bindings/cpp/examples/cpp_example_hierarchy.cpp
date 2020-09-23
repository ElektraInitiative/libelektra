/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyset.hpp>

#include <cassert>
#include <iostream>
#include <map>
#include <string>

namespace kdb
{

/**
 * @brief Search for nth level in a name
 *
 * @param k the key to search in
 * @param n the level
 *
 * nth_level_of_name(Key("user:/hello", KEY_END), 0) -> "user:"
 * nth_level_of_name(Key("user:/hello", KEY_END), 1) -> "hello"
 *
 * @return the searched string (without slashes)
 */
std::string nth_level_of_name (Key k, unsigned long n)
{
	size_t prev = 0;
	std::string name = k.getName ();

	for (unsigned long i = 0; i < n; ++i)
	{
		size_t pos = name.find ("/", prev);
		prev = pos + 1;
	}

	return name.substr (prev, name.find ("/", prev) - prev);
}

/**
 * @brief The number of levels a name has
 *
 * @param k the key to search within
 *
 * @return the hierarchy depth of a key
 */
unsigned long name_depth (Key k)
{
	std::string name = k.getName ();
	size_t pos = name.find ("/", 0);
	unsigned long depth = 0;

	while (pos != std::string::npos)
	{
		pos = name.find ("/", pos + 1);
		++depth;
	}

	return depth;
}

/**
 * @brief Visitor (of Visitor pattern)
 */
class Visitor
{
public:
	virtual void visit (std::string name, unsigned long depth, Key k) = 0;
};

class KeyHierarchy;

/**
 * @brief A KeyNode builds up the structure of the KeyHierarchy
 *
 * Every KeyNode contains children (subnodes).
 * KeyNodes that are for structure only, do not contain a Key (0-key)
 * other KeyNodes have a reference to a key
 */
class KeyNode
{
public:
	typedef std::map<std::string, KeyNode> KeyNodeMap;
	typedef KeyNodeMap::iterator KeyNodeIterator;

	KeyNode (unsigned long depth, Key k = static_cast<ckdb::Key *> (nullptr)) : m_self (k), m_subnodes (), m_depth (depth)
	{
	}

	/**
	 * @brief (Recursively) add or update a key to nodes
	 *
	 * If the key exists, it will be updated
	 *
	 * If the key does not exist, node(s) will be created
	 *
	 * @param k the key to add
	 * @param depth current depth of recursion
	 */
	void add (Key k, unsigned long depth)
	{
		assert (k);
		assert (m_self ? m_self.isBelow (k) : true);
		depth++;

		if (m_self.isDirectBelow (k) || depth == name_depth (k))
		{
			for (auto & elem : m_subnodes)
			{
				if (elem.first == k.getBaseName ())
				{
					// found node, update it
					elem.second.m_self = k;
					return;
				}
			}
			// will add new subnode (direct below+not found)
			m_subnodes.insert (std::make_pair (k.getBaseName (), KeyNode (depth, k)));
			return;
		}

		for (auto & elem : m_subnodes)
		{
			if (k.isBelow (elem.second.m_self))
			{
				// found subnode, call recursively
				elem.second.add (k, depth);
				return;
			}
		}

		// create a structure key (without key, only name)
		std::string name = nth_level_of_name (k, depth);
		std::pair<KeyNodeIterator, bool> p = m_subnodes.insert (std::make_pair (name, KeyNode (depth)));
		// structure keys get a null key
		auto it = p.first;
		it->second.add (k, depth);
	}

	/**
	 * @brief Accept a visitor for iteration
	 *
	 * @param visitor defines the action
	 */
	void accept (Visitor & visitor)
	{
		for (auto & elem : m_subnodes)
		{
			visitor.visit (elem.first, elem.second.m_depth, elem.second.m_self);
			elem.second.accept (visitor);
		}
	}

private:
	friend class KeyHierarchy; // they are tightly coupled

	Key m_self;
	KeyNodeMap m_subnodes;
	unsigned long m_depth;
};

/**
 * @brief Builds up a hierarchy of Keys
 *
 * Also keeps a KeySet in sync with what is in the hierarchy.
 * The keys will be shared between the keyset and the hierarchy.
 */
class KeyHierarchy
{
public:
	explicit KeyHierarchy (KeySet & keyset) : m_userRootNode (0), m_systemRootNode (0), m_keyset (keyset)
	{
		add (keyset);
	}

	/**
	 * @brief Add all keys of a keyset
	 *
	 * Will not update the underlying keyset (is fixed at
	 * construction)
	 *
	 * @param ks
	 */
	void add (KeySet const & ks)
	{
		for (auto && k : ks)
		{
			add (k);
		}
	}

	/**
	 * @brief Add a single key to hierarchy
	 *
	 * @param k the key to add
	 */
	void add (Key k)
	{
		// update root nodes
		if (k.getName () == "user:/")
		{
			m_userRootNode.m_self = k;
		}
		else if (k.getName () == "system:/")
		{
			m_systemRootNode.m_self = k;
		}
		// if it is not a root node, update hierarchy
		else if (k.isUser ())
		{
			m_userRootNode.add (k, 0);
		}
		else
		{
			m_systemRootNode.add (k, 0);
		}

		// update keyset
		m_keyset.lookup (k, KDB_O_POP);
		m_keyset.append (k);
	}

	/**
	 * @brief Allow visitor to traverse the hierarchy
	 *
	 * @param visitor to traverse
	 */
	void accept (Visitor & visitor)
	{
		visitor.visit ("user:/", 0, m_userRootNode.m_self);
		m_userRootNode.accept (visitor);
		visitor.visit ("system:/", 0, m_systemRootNode.m_self);
		m_systemRootNode.accept (visitor);
	}

private:
	KeyNode m_userRootNode;
	KeyNode m_systemRootNode;

	KeySet & m_keyset;
};
} // namespace kdb


/**
 * @brief Example visitor that prints the hierarchy
 */
class PrintVisitor : public kdb::Visitor
{
public:
	void visit (std::string name, unsigned long depth, kdb::Key k) override
	{
		for (unsigned long i = 0; i < depth; ++i)
		{
			std::cout << "   ";
		}

		std::cout << name;

		if (k)
		{
			std::cout << "(" << k.getName () << ") = " << k.getString ();
		}

		std::cout << std::endl;
	}
};

int main ()
{
	using namespace kdb;
	KeySet ks;
	KeyHierarchy kh (ks);
	kh.add (Key ("user:/hello", KEY_VALUE, "Hello world", KEY_END));
	PrintVisitor pv;
	kh.accept (pv);
	std::cout << std::endl;

	kh.add (Key ("system:/b/s/t", KEY_VALUE, "Below", KEY_END));
	kh.accept (pv);
	std::cout << std::endl;

	kh.add (Key ("system:/b/s/t", KEY_VALUE, "Updated", KEY_END));
	kh.accept (pv);
	std::cout << std::endl;

	kh.add (Key ("system:/", KEY_VALUE, "root value", KEY_END));
	kh.accept (pv);
}
