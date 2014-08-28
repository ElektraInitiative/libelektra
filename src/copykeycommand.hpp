#ifndef COPYKEYCOMMAND_H
#define COPYKEYCOMMAND_H

#include <QUndoCommand>
#include <QApplication>
#include <QClipboard>
#include <QDebug>

class CopyKeyCommand : public QUndoCommand
{
public:
    explicit CopyKeyCommand(QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

private:

    QClipboard *m_clipboard;
};

#endif // COPYKEYCOMMAND_H
