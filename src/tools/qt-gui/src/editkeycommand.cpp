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
	QModelIndex index = m_model->index(m_index);
	m_model->setData(index, m_oldName, TreeViewModel::NameRole);
	m_model->setData(index, m_oldValue, TreeViewModel::ValueRole);
	m_model->model().at(m_index)->setMeta(m_oldMetaData);
}

void EditKeyCommand::redo()
{
	QModelIndex index = m_model->index(m_index);
	m_model->setData(index, m_newName, TreeViewModel::NameRole);
	m_model->setData(index, m_newValue, TreeViewModel::ValueRole);
	m_model->model().at(m_index)->setMeta(m_newMetaData);
}
