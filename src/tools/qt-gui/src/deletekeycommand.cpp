#include "deletekeycommand.hpp"

DeleteKeyCommand::DeleteKeyCommand(const QString &type, TreeViewModel *model, ConfigNode *node, int index, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_node(node)
    , m_index(index)
{
    setText(type);
}

/**
 * @brief After DeleteKeyCommand is destructed, the node will be lost forever.
 */
DeleteKeyCommand::~DeleteKeyCommand()
{
    //TODO: This node will be deleted twice: once here and once when the parentmodel is destructed. This
    //leads to a crash on exit.
//    delete m_node;
}

void DeleteKeyCommand::undo()
{
    m_model->insertRow(m_index, m_node);
}

void DeleteKeyCommand::redo()
{
    m_model->removeRow(m_index);
}
