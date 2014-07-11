#ifndef KEYNODE_HPP
#define KEYNODE_HPP

#include "visitor.hpp"
#include <map>
#include <kdb.hpp>
#include <cassert>

class KeyNode
{

public:
    typedef std::map<std::string, KeyNode> KeyNodeMap;
    typedef KeyNodeMap::iterator KeyNodeIterator;

    explicit KeyNode(unsigned long depth, kdb::Key k = 0);
    ~KeyNode();

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
    void add(kdb::Key k, unsigned long depth);

    /**
    * @brief Accept a visitor for iteration
    *
    * @param visitor defines the action
    */
    void accept(Visitor & visitor);

private:
    friend class KeyHierarchy;
    kdb::Key m_self;
    KeyNodeMap m_subnodes;
    unsigned long m_depth;

    /**
    * @brief Search for nth level in a name
    *
    * @param k the key to search in
    * @param n the level
    *
    * nth_level_of_name(Key("user/hello", KEY_END), 0) -> "user"
    * nth_level_of_name(Key("user/hello", KEY_END), 1) -> "hello"
    *
    * @return the searched string (without slashes)
    */
    std::string nth_level_of_name(kdb::Key k, unsigned long n);

    /**
    * @brief The number of levels a name has
    *
    * @param k the key to search within
    *
    * @return the hierarchy depth of a key
    */
    unsigned long name_depth(kdb::Key k);

};

#endif // KEYNODE_HPP
