#include "importconfigurationcommand.hpp"

using namespace kdb;

ImportConfigurationCommand::ImportConfigurationCommand(TreeViewModel *model, const QString keyName, const QString format, const QString file, const QString mergeStrategy, QUndoCommand *parent)
    : QUndoCommand(parent)
    , m_model(model)
    , m_name(keyName)
    , m_format(format)
    , m_file(file)
    , m_mergeStrategy(mergeStrategy)
{
    setText("import");

    m_kdb.get(m_set, "");
}

void ImportConfigurationCommand::undo()
{
    qDebug() << "UNDO";
    m_model->setKeySet(m_set);
    m_model->populateModel();
}

void ImportConfigurationCommand::redo()
{
    qDebug() << "REDO";
    m_model->importConfiguration(m_name, m_format, m_file, m_mergeStrategy);
}
