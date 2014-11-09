#ifndef VISITOR_H
#define VISITOR_H

#include <QSharedPointer>

class ConfigNode;
class TreeViewModel;

/**
 * @brief  The Visitor class to support the visitor pattern.
 */
class Visitor {

public:
    /**
     * @brief The abstract method a visitor who wants to visit a ConfigNode needs to implement.
     *
     * @fn visit
     * @param node
     */
    virtual void visit(ConfigNode &node) = 0;

    /**
     * @brief The abstract method a visitor who wants to visit a TreeViewModel needs to implement.
     *
     * @fn visit
     * @param model
     */
    virtual void visit(TreeViewModel *model) = 0;
};

#endif // VISITOR_H
