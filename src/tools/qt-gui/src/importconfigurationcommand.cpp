#include "importconfigurationcommand.hpp"

using namespace kdb;

ImportConfigurationCommand::ImportConfigurationCommand(TreeViewModel *model, const QString &keyName, const QString &format, const QString &file, const QString &mergeStrategy, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_name(keyName)
    , m_format(format)
    , m_file(file)
    , m_mergeStrategy(mergeStrategy)
{
    m_kdb.get(m_set, "");
    qDebug() << "START KEYSET CONSTRUCTOR";
    m_set.rewind();
    while(m_set.next())
        qDebug() << QString::fromStdString(m_set.current().getName());
    qDebug() << "END KEYSET CONSTRUCTOR";
}

void ImportConfigurationCommand::undo()
{
    m_model->setKeySet(m_set.dup());
    m_model->populateModel();
}

void ImportConfigurationCommand::redo()
{
    m_model->importConfiguration(m_name, m_format, m_file, m_mergeStrategy);
}
