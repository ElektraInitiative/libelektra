/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./importconfigurationcommand.hpp"

using namespace kdb;

ImportConfigurationCommand::ImportConfigurationCommand (TreeViewModel * model, int index, DataContainer * data, QUndoCommand * parent)
: QUndoCommand (parent), m_model (model), m_index (index), m_before (new ConfigNode (*model->model ().at (index))), m_after (nullptr),
  m_name (data->importName ()), m_format (data->format ()), m_file (data->file ()), m_mergeStrategies (data->mergeStrategies ())
{
	setText ("import");

	m_model->importConfiguration (m_name, m_format, m_file, m_mergeStrategies);
	m_after = model->model ().at (index);
}

void ImportConfigurationCommand::undo ()
{
	m_model->removeRow (m_index);
	m_model->insertRow (m_index, m_before);
}

void ImportConfigurationCommand::redo ()
{
	m_model->removeRow (m_index);
	m_model->insertRow (m_index, m_after);
}
