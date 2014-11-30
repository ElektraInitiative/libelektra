#include "deletekeycommand.hpp"

DeleteKeyCommand::DeleteKeyCommand(const QString& type, TreeViewModel* model, int index, QUndoCommand* parent)
	: QUndoCommand(parent)
	, m_model(model)
	, m_node(model->model().at(index))
	, m_index(index)
{
	setText(type);
}

void DeleteKeyCommand::undo()
{
	m_model->insertRow(m_index, m_node);
	m_model->refreshArrayNumbers();
	m_model->refresh();
}

void DeleteKeyCommand::redo()
{
	m_model->removeRow(m_index);
	m_model->refreshArrayNumbers();
}
