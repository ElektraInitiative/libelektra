#ifndef CUTKEYCOMMAND_H
#define CUTKEYCOMMAND_H

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class CutKeyCommand : public QUndoCommand
{
public:
    explicit CutKeyCommand(QString type, TreeViewModel *sourceParentModel, ConfigNode *source, ConfigNode *target, int index, QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

private:

    TreeViewModel*  m_sourceParentModel;
    ConfigNode*     m_source;
    ConfigNode*     m_target;
    bool            m_isExpanded;
    int             m_sourceIndex;
    int             m_targetIndex;
};

#endif // CUTKEYCOMMAND_H
