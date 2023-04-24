/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./deletekeycommand.hpp"

DeleteKeyCommand::DeleteKeyCommand (const QString & type, TreeViewModel * model, int index, QUndoCommand * parent)
: QUndoCommand (parent), m_model (model), m_node (model->model ().at (index)), m_index (index), m_isRoot (false)
{
	setText (type);

	if (!m_node->getPath ().contains ('/'))
	{
		m_isRoot = true;
		m_root = ConfigNodePtr (new ConfigNode (m_node->getPath (), m_node->getPath (), nullptr, m_model));
	}
}

void DeleteKeyCommand::undo ()
{
	if (m_isRoot) m_model->removeRow (m_index);
	m_model->insertRow (m_index, m_node);
	m_model->refreshArrayNumbers ();
	m_model->refresh ();
}

void DeleteKeyCommand::redo ()
{
	m_model->removeRow (m_index);
	if (m_isRoot) m_model->insertRow (m_index, m_root, false);
	m_model->refreshArrayNumbers ();
}
