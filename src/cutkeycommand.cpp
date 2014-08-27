#include "cutkeycommand.hpp"

CutKeyCommand::CutKeyCommand(QUndoCommand *parent)
    : QUndoCommand(parent)
{
    setText("cut");
}

void CutKeyCommand::undo()
{

}

void CutKeyCommand::redo()
{

}
