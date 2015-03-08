#ifndef IMPORTCONFIGURATIONCOMMAND_H
#define IMPORTCONFIGURATIONCOMMAND_H

#include <QUndoCommand>
#include <kdb.hpp>
#include "treeviewmodel.hpp"
#include "datacontainer.hpp"

class ImportConfigurationCommand : public QUndoCommand
{

public:
	/**
	 * @brief The command to import a configuration from a file.
	 *
	 * @param model The model the configuration is imported to.
	 * @param keyName The
	 * @param format ...
	 * @param file ...
	 * @param mergeStrategy ...
	 * @param parent ...
	 */
	explicit ImportConfigurationCommand(TreeViewModel* model, int index, DataContainer *data, QUndoCommand* parent = 0);

	virtual void undo();
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
