#ifndef DELETECOMMAND_HPP
#define DELETECOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class DeleteCommand : public QUndoCommand
{

public:
    explicit DeleteCommand(TreeViewModel *model, ConfigNode *node, int index);

    virtual void undo();
    virtual void redo();

private:

    TreeViewModel *m_model;
    ConfigNode m_node;
    int m_index;

};

#endif // DELETECOMMAND_HPP
