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
    ConfigNode *node = new ConfigNode(m_node);
    node->setParentModel(m_model);
    m_model->insertRow(m_index, node);
}

void DeleteKeyCommand::redo()
{
    m_model->removeRow(m_index);
}
