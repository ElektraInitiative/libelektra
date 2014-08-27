#include "copykeycommand.hpp"

CopyKeyCommand::CopyKeyCommand(QUndoCommand *parent)
    : QUndoCommand(parent)
{
    setText("copy");
}

void CopyKeyCommand::undo()
{

}

void CopyKeyCommand::redo()
{

}
