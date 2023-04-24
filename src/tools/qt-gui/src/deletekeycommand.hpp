/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef DELETEKEYCOMMAND_HPP
#define DELETEKEYCOMMAND_HPP

#include "./treeviewmodel.hpp"
#include <QUndoCommand>

/**
 * @brief The DeleteKeyCommand class. Remembers a node for redo/undo.
 */
class DeleteKeyCommand : public QUndoCommand
{

public:
	/**
	 * @brief The command to delete a ConfigNode.
	 *
	 * @param type Declares if the ConfigNode is a single key or a branch.
	 * @param model The model that holds the ConfigNode that is deleted.
	 * @param index The index of the ConfigNode that is deleted.
	 * @param parent An optional parent command.
	 */
	explicit DeleteKeyCommand (const QString & type, TreeViewModel * model, int index, QUndoCommand * parent = nullptr);

	virtual void undo () override;
	virtual void redo () override;

private:
	TreeViewModel * m_model;
	ConfigNodePtr m_node;
	int m_index;
	bool m_isRoot;
	ConfigNodePtr m_root;
};

#endif // DELETEKEYCOMMAND_HPP
