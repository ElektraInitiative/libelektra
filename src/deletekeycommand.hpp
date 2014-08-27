#ifndef DELETEKEYCOMMAND_HPP
#define DELETEKEYCOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class DeleteKeyCommand : public QUndoCommand
{

public:
    explicit DeleteKeyCommand(TreeViewModel *model, ConfigNode *node, int index, QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

private:

    TreeViewModel *m_model;
    ConfigNode m_node;
    int m_index;

};

#endif // DELETEKEYCOMMAND_HPP
