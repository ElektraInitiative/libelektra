/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./cutkeycommand.hpp"

CutKeyCommand::CutKeyCommand (QString type, ConfigNodePtr source, ConfigNodePtr target, int sourceIndex, QUndoCommand * parent)
: QUndoCommand (parent), m_sourceParentModel (source->getParentModel ()), m_source (new ConfigNode (*source)), m_target (target),
  m_isExpanded (target->isExpanded ()), m_sourceIndex (sourceIndex), m_targetIndex (-1)
{
	setText (type);

	QString newPath = m_target->getPath () + "/" + m_source->getName ();
	m_source->setPath (newPath);
}

void CutKeyCommand::undo ()
{
	m_isExpanded = m_target->isExpanded ();
	m_sourceParentModel->insertRow (m_sourceIndex, m_source);
	m_target->getChildren ()->removeRow (m_targetIndex);

	if (m_sourceParentModel == m_target->getChildren ())
	{
		m_sourceParentModel->refresh ();
	}
}

void CutKeyCommand::redo ()
{
	m_target->setIsExpanded (m_isExpanded);
	m_target->appendChild (m_source);
	m_sourceParentModel->removeRow (m_sourceIndex);

	if (m_sourceParentModel == m_target->getChildren ())
	{
		m_targetIndex = m_target->getChildCount ();
		m_sourceParentModel->refresh ();
	}
	else
		m_targetIndex = m_target->getChildCount () - 1;
}
