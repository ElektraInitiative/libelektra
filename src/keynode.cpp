#include "keynode.hpp"

using namespace kdb;

KeyNode::KeyNode(unsigned long depth, Key k) : m_self(k), m_subnodes(), m_depth(depth)
{}

KeyNode::~KeyNode()
{

}

void KeyNode::add(Key k, unsigned long depth)
{
    assert(k);
    assert(m_self ? k.isBelow(m_self) : true);
    depth++;

    if (k.isDirectBelow(m_self) || depth == name_depth(k))
    {
        for (KeyNodeIterator it = m_subnodes.begin();
             it != m_subnodes.end();
             ++it)
        {
            if (it->first == k.getBaseName())
            {
                // found node, update it
                it->second.m_self = k;
                return;
            }
        }
        // will add new subnode (direct below+not found)
        m_subnodes.insert(std::make_pair(k.getBaseName(), KeyNode(depth, k)));
        return;
    }

    for (KeyNodeIterator it = m_subnodes.begin();
         it != m_subnodes.end();
         ++it)
    {
        if (k.isBelow(it->second.m_self))
        {
            // found subnode, call recursively
            it->second.add(k, depth);
            return;
        }
    }

    // create a structure key (without key, only name)
    std::string name = nth_level_of_name(k, depth);
    std::pair<KeyNodeIterator, bool> p = m_subnodes.insert(
                std::make_pair(name, KeyNode(depth)));
    // structure keys get a null key
    KeyNodeIterator it = p.first;
    it->second.add(k, depth);
}

void KeyNode::accept(Visitor &visitor)
{
    for (KeyNodeIterator it = m_subnodes.begin();
         it != m_subnodes.end();
         ++it)
    {
        visitor.visit(it->first, it->second.m_depth, it->second.m_self);
        it->second.accept(visitor);
    }
}

std::string KeyNode::nth_level_of_name(Key k, unsigned long n)
{
    size_t pos = 0;
    size_t prev = 0;
    std::string name = k.getName();

    for (unsigned long i=0; i<n; ++i)
    {
        pos = name.find("/", prev);
        prev = pos+1;
    }

    return name.substr(prev, name.find("/", prev)-prev);
}

unsigned long KeyNode::name_depth(Key k)
{
    std::string name = k.getName();
    unsigned long pos = name.find("/", 0);
    unsigned long depth = 0;

    while (pos != std::string::npos)
    {
        pos = name.find("/", pos+1);
        ++ depth;
    }

    return depth;
}

