/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./copykeycommand.hpp"
#include "./treeviewmodel.hpp"

CopyKeyCommand::CopyKeyCommand (QString type, ConfigNodePtr source, ConfigNodePtr target, QUndoCommand * parent)
: QUndoCommand (parent), m_source (new ConfigNode (*source)), m_target (target), m_isExpanded (target->isExpanded ()), m_index (-1)
{
	setText (type);

	QString newPath = m_target->getPath () + "/" + m_source->getName ();
	m_source->setPath (newPath);
}

void CopyKeyCommand::undo ()
{
	m_isExpanded = m_target->isExpanded ();
	m_target->getChildren ()->removeRow (m_index);

	if (m_target->isExpanded () && m_target->childrenHaveNoChildren ()) m_target->setIsExpanded (false);
}

void CopyKeyCommand::redo ()
{
	m_target->setIsExpanded (m_isExpanded);
	m_target->appendChild (m_source);
	m_index = m_target->getChildCount () - 1;
}
