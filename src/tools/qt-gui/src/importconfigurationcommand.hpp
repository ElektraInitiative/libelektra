#ifndef IMPORTCONFIGURATIONCOMMAND_H
#define IMPORTCONFIGURATIONCOMMAND_H

#include <QUndoCommand>
#include <kdb.hpp>
#include "treeviewmodel.hpp"
#include "datacontainer.hpp"

/**
 * @brief The ImportConfigurationCommand class
 */

class ImportConfigurationCommand : public QUndoCommand
{

public:
	/**
	 * @brief The command to import a configuration from a file.
	 *
	 * @param model The model the configuration is imported to.
	 * @param index
	 * @param data
	 * @param parent
	 */
	explicit ImportConfigurationCommand(TreeViewModel* model, int index, DataContainer *data, QUndoCommand* parent = 0);

	/**
	 * @brief undo
	 */
	virtual void undo();

	/**
	 * @brief redo
	 */
	virtual void redo();

private:
	TreeViewModel*	m_model;
	int				m_index;
	ConfigNodePtr	m_before;
	ConfigNodePtr	m_after;
	QString			m_name;
	QString			m_format;
	QString			m_file;
	QVariantList	m_mergeStrategies;
};

#endif // IMPORTCONFIGURATIONCOMMAND_H
