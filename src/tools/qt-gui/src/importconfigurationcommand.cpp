#include "importconfigurationcommand.hpp"

using namespace kdb;

ImportConfigurationCommand::ImportConfigurationCommand(TreeViewModel* model, int index, const QString keyName, const QString format, const QString file, const QString mergeStrategy, QUndoCommand* parent)
	: QUndoCommand(parent)
	, m_model(model)
	, m_index(index)
	, m_before(new ConfigNode(*model->model().at(index)))
	, m_after(NULL)
	, m_name(keyName)
	, m_format(format)
	, m_file(file)
	, m_mergeStrategy(mergeStrategy)
{
	setText("import");

	m_model->importConfiguration(m_name, m_format, m_file, m_mergeStrategy);
	m_after = model->model().at(index);
}

void ImportConfigurationCommand::undo()
{
	m_model->removeRow(m_index);
	m_model->insertRow(m_index, m_before);
}

void ImportConfigurationCommand::redo()
{
	m_model->removeRow(m_index);
	m_model->insertRow(m_index, m_after);
}
