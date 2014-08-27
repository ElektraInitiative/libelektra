#ifndef COPYKEYCOMMAND_H
#define COPYKEYCOMMAND_H

#include <QUndoCommand>

class CopyKeyCommand : public QUndoCommand
{
public:
    explicit CopyKeyCommand(QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();
};

#endif // COPYKEYCOMMAND_H
