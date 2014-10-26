#include "copykeycommand.hpp"
#include "treeviewmodel.hpp"

CopyKeyCommand::CopyKeyCommand(QString type, ConfigNode *source, ConfigNode *target, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_source(source)
    , m_target(target)
    , m_index(-1)
{
    setText(type);
}

void CopyKeyCommand::undo()
{
    m_target->getChildren()->removeRow(m_index);
}

void CopyKeyCommand::redo()
{
    QString newPath = m_target->getPath() + "/" + m_source->getName();
    m_source->setPath(newPath);
    m_target->appendChild(new ConfigNode(*m_source));
    m_index = m_target->getChildCount() - 1;
}
