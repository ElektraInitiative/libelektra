#ifndef COPYKEYCOMMAND_H
#define COPYKEYCOMMAND_H

#include <QUndoCommand>
#include "confignode.hpp"

#include <QDebug>

class CopyKeyCommand : public QUndoCommand
{
public:
    explicit CopyKeyCommand(ConfigNode *source, ConfigNode *target, QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

private:

    ConfigNode m_source;
    ConfigNode *m_target;
};

#endif // COPYKEYCOMMAND_H
