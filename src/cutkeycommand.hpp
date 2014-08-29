#ifndef CUTKEYCOMMAND_H
#define CUTKEYCOMMAND_H

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class CutKeyCommand : public QUndoCommand
{
public:
    explicit CutKeyCommand(TreeViewModel *model, ConfigNode *source, ConfigNode *target, int index, QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

private:

    TreeViewModel   *m_model;
    ConfigNode       m_source;
    ConfigNode      *m_target;
    int              m_index;
};

#endif // CUTKEYCOMMAND_H
