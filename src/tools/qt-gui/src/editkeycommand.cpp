#include "editkeycommand.hpp"

EditKeyCommand::EditKeyCommand(TreeViewModel* model, int index, QVariantList data, QUndoCommand* parent)
	:  QUndoCommand(parent)
	, m_model(model)
	, m_index(index)
	, m_oldName(data.at(0).toString())
	, m_oldValue(data.at(1).toString())
	, m_oldMetaData(data.at(2).toMap())
	, m_newName(data.at(3).toString())
	, m_newValue(data.at(4).toString())
	, m_newMetaData(data.at(5).toMap())
{
	setText("edit");
}

void EditKeyCommand::undo()
{
	m_model->setData(m_index, m_oldName, "Name");
	m_model->setData(m_index, m_oldValue, "Value");
	m_model->model().at(m_index)->setMeta(m_oldMetaData);
}

void EditKeyCommand::redo()
{
	m_model->setData(m_index, m_newName, "Name");
	m_model->setData(m_index, m_newValue, "Value");
	m_model->model().at(m_index)->setMeta(m_newMetaData);
}
