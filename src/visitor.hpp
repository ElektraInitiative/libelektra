#ifndef VISITOR_H
#define VISITOR_H

#include <string>
#include <kdb.hpp>

/**
* @brief Visitor (of Visitor pattern)
*/
class Visitor
{
public:
    virtual void visit(std::string name, unsigned long depth, kdb::Key k) = 0;
};

#endif // VISITOR_H
