#ifndef NEWKEYCOMMAND_HPP
#define NEWKEYCOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"
#include "datacontainer.hpp"

/**
 * @brief The NewKeyCommand class
 */

class NewKeyCommand : public QUndoCommand
{

public:
	/**
	 * @brief ...
	 *
	 * @param parentNode ...
	 * @param data
	 * @param isBelow
	 * @param parent ...
	 */
	explicit NewKeyCommand(ConfigNodePtr parentNode, DataContainer* data, bool isBelow, QUndoCommand* parent = 0);

	/**
	 * @brief undo
	 */
	virtual void undo();

	/**
	 * @brief redo
	 */
	virtual void redo();

private:
	ConfigNodePtr	m_parentNode;
	ConfigNodePtr	m_newNode;
	QString			m_name;
	QString			m_value;
	QVariantMap		m_metaData;

	/**
	 * @brief cutListAtIndex
	 * @param list
	 * @param index
	 * @return
	 */
	QStringList		cutListAtIndex(QStringList &list, int index);
};

#endif // NEWKEYCOMMAND_HPP
