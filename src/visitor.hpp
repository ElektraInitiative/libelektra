#ifndef VISITOR_H
#define VISITOR_H

#include "confignode.hpp"

class ConfigNode;

class Visitor {

public:
    virtual void visit(ConfigNode *node) = 0;
};

#endif // VISITOR_H
