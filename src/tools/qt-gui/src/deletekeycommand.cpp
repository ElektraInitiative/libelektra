#include "deletekeycommand.hpp"

DeleteKeyCommand::DeleteKeyCommand(const QString& type, TreeViewModel* model, ConfigNodePtr node, int index, QUndoCommand* parent)
	: QUndoCommand(parent)
	, m_model(model)
	, m_node(node)
	, m_index(index)
{
	setText(type);
}

DeleteKeyCommand::~DeleteKeyCommand()
{
}

void DeleteKeyCommand::undo()
{
	m_model->insertRow(m_index, m_node);
}

void DeleteKeyCommand::redo()
{
	m_model->removeRow(m_index);
}
