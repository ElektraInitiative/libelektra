#include "deletecommand.hpp"

DeleteCommand::DeleteCommand(TreeViewModel *model, ConfigNode *node, int index)
    : m_model(model)
    , m_node(*node)
    , m_index(index)
{
}

void DeleteCommand::undo()
{
    m_model->insertRow(m_index, new ConfigNode(m_node));
}

void DeleteCommand::redo()
{
    m_model->removeRow(m_index);
}
