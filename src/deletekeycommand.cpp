#include "deletekeycommand.hpp"

DeleteKeyCommand::DeleteKeyCommand(const QString &type, TreeViewModel *model, ConfigNode *node, int index, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_node(*node)
    , m_index(index)
{
    setText(type);
}

void DeleteKeyCommand::undo()
{
    m_model->insertRow(m_index, new ConfigNode(m_node));
}

void DeleteKeyCommand::redo()
{
    m_model->removeRow(m_index);
}
