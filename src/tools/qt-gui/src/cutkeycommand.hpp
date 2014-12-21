#ifndef CUTKEYCOMMAND_H
#define CUTKEYCOMMAND_H

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class CutKeyCommand : public QUndoCommand
{
public:
	/**
	 * @brief The command to cut and paste a ConfigNode.
	 *
	 * @param type Declares if the ConfigNode is a single key or a branch.
	 * @param source The ConfigNode that is cut.
	 * @param target The ConfigNode that is the new parent node of the cut ConfigNode.
	 * @param index The index of the cut ConfigNode, needed to remove the cut ConfigNode.
	 */
	explicit CutKeyCommand(QString type, ConfigNodePtr source, ConfigNodePtr target, int sourceIndex, QUndoCommand* parent = 0);

	virtual void undo();
	virtual void redo();

private:

	TreeViewModel*  m_sourceParentModel;
	ConfigNodePtr   m_source;
	ConfigNodePtr   m_target;
	bool            m_isExpanded;
	int             m_sourceIndex;
	int             m_targetIndex;
};

#endif // CUTKEYCOMMAND_H
