#ifndef KEYSETVISITOR_HPP
#define KEYSETVISITOR_HPP

#include "visitor.hpp"
#include "confignode.hpp"
#include <kdb.hpp>

class KeySetVisitor : public Visitor
{
public:
    explicit KeySetVisitor(kdb::KeySet &keySet);
    void visit(ConfigNode *node);
    void visit(TreeViewModel *model);

private:
    kdb::KDB m_kdb;
    kdb::KeySet m_set;
};

#endif // KEYSETVISITOR_HPP
