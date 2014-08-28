#include "copykeycommand.hpp"
#include "treeviewmodel.hpp"

CopyKeyCommand::CopyKeyCommand(QUndoCommand *parent)
    : QUndoCommand(parent)
{
    setText("copy");
    m_clipboard = QApplication::clipboard();
    ConfigNode *node = qvariant_cast<ConfigNode*>(m_clipboard->property("node"));
    int index = m_clipboard->property("index").toInt();
    qDebug() << "clipboard node name " << node->getName() << " has index " << index;
}

void CopyKeyCommand::undo()
{

}

void CopyKeyCommand::redo()
{

}
