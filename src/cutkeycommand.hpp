#ifndef CUTKEYCOMMAND_H
#define CUTKEYCOMMAND_H

#include <QUndoCommand>

class CutKeyCommand : public QUndoCommand
{
public:
    explicit CutKeyCommand(QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();
};

#endif // CUTKEYCOMMAND_H
