#include "copykeycommand.hpp"
#include "treeviewmodel.hpp"

CopyKeyCommand::CopyKeyCommand(ConfigNode *source, ConfigNode *target, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_source(*source)
    , m_target(target)
{
    setText("copy");
}

void CopyKeyCommand::undo()
{
    m_target->getChildren()->removeRow(m_target->getIndexByName(m_source.getName()));
}

void CopyKeyCommand::redo()
{
    QString newPath = m_target->getPath() + "/" + m_source.getName();
    m_source.setPath(newPath);
    m_target->appendChild(new ConfigNode(m_source));
}
