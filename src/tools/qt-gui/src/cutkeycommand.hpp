/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CUTKEYCOMMAND_H
#define CUTKEYCOMMAND_H

#include "./treeviewmodel.hpp"
#include <QUndoCommand>

/**
 * @brief The CutKeyCommand class
 *
 * This class allows undoing/redoing copy and paste of a ConfigNode.
 */
class CutKeyCommand : public QUndoCommand
{
public:
	/**
	 * @brief The command to cut and paste a ConfigNode.
	 *
	 * @param type Declares if the ConfigNode is a single key or a branch.
	 * @param source The ConfigNode that is cut.
	 * @param target The ConfigNode that is the new parent node of the cut ConfigNode.
	 * @param sourceIndex The index of the cut ConfigNode, needed to remove the cut ConfigNode.
	 * @param parent
	 */
	explicit CutKeyCommand (QString type, ConfigNodePtr source, ConfigNodePtr target, int sourceIndex, QUndoCommand * parent = nullptr);

	/**
	 * @copydoc QUndoCommand::undo()
	 */
	virtual void undo () override;

	/**
	 * @copydoc QUndoCommand::redo()
	 */
	virtual void redo () override;

private:
	TreeViewModel * m_sourceParentModel;
	ConfigNodePtr m_source;
	ConfigNodePtr m_target;
	bool m_isExpanded;
	int m_sourceIndex;
	int m_targetIndex;
};

#endif // CUTKEYCOMMAND_H
