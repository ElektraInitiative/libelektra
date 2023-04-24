/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./editkeycommand.hpp"

EditKeyCommand::EditKeyCommand (TreeViewModel * model, int index, DataContainer * data, QUndoCommand * parent)
: QUndoCommand (parent), m_model (model), m_index (index), m_oldName (data->oldName ()), m_oldValue (data->oldValue ()),
  m_oldMetaData (data->oldMetadata ()), m_newName (data->newName ()), m_newValue (data->newValue ()), m_newMetaData (data->newMetadata ())
{
	setText ("edit");
}

void EditKeyCommand::undo ()
{
	QModelIndex index = m_model->index (m_index);
	m_model->setData (index, m_oldName, TreeViewModel::NameRole);
	m_model->setData (index, m_oldValue, TreeViewModel::ValueRole);
	m_model->model ().at (m_index)->setMeta (m_oldMetaData);
}

void EditKeyCommand::redo ()
{
	QModelIndex index = m_model->index (m_index);
	m_model->setData (index, m_newName, TreeViewModel::NameRole);
	m_model->setData (index, m_newValue, TreeViewModel::ValueRole);
	m_model->model ().at (m_index)->setMeta (m_newMetaData);
}
