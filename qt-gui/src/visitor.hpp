#ifndef VISITOR_H
#define VISITOR_H

class ConfigNode;
class TreeViewModel;

class Visitor {

public:
    virtual void visit(ConfigNode *node) = 0;
    virtual void visit(TreeViewModel *model) = 0;
};

#endif // VISITOR_H
