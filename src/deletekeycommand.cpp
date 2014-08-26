#include "deletekeycommand.hpp"

DeleteKeyCommand::DeleteKeyCommand(TreeViewModel *model, ConfigNode *node, int index)
    : m_model(model)
    , m_node(*node)
    , m_index(index)
{
    setText("delete");
}

void DeleteKeyCommand::undo()
{
    m_model->insertRow(m_index, new ConfigNode(m_node));
}

void DeleteKeyCommand::redo()
{
    m_model->removeRow(m_index);
}
