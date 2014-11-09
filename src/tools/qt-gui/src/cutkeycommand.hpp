#ifndef CUTKEYCOMMAND_H
#define CUTKEYCOMMAND_H

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class CutKeyCommand : public QUndoCommand
{
public:
	explicit CutKeyCommand(QString type, ConfigNodePtr source, ConfigNodePtr target, int index, QUndoCommand* parent = 0);

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
