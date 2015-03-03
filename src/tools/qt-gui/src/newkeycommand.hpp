#ifndef NEWKEYCOMMAND_HPP
#define NEWKEYCOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"
#include "datacontainer.hpp"

class NewKeyCommand : public QUndoCommand
{

public:
	/**
	 * @brief ...
	 *
	 * @param parentNode ...
	 * @param path ...
	 * @param value ...
	 * @param metaData ...
	 * @param parent ...
	 */
	explicit NewKeyCommand(ConfigNodePtr parentNode, DataContainer* data, bool isBelow, QUndoCommand* parent = 0);

	virtual void undo();
	virtual void redo();

private:

	ConfigNodePtr m_parentNode;
	ConfigNodePtr m_newNode;
	QString       m_name;
	QString       m_value;
	QVariantMap   m_metaData;
};

#endif // NEWKEYCOMMAND_HPP
