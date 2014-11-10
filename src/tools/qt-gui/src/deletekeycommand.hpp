#ifndef DELETEKEYCOMMAND_HPP
#define DELETEKEYCOMMAND_HPP

#include <QUndoCommand>
#include "treeviewmodel.hpp"

/**
 * @brief Remembers a node for redo/undo
 */
class DeleteKeyCommand : public QUndoCommand
{

public:
	explicit DeleteKeyCommand(const QString& type, TreeViewModel* model, int index, QUndoCommand* parent = 0);
	~DeleteKeyCommand();

	virtual void undo();
	virtual void redo();

private:

	TreeViewModel*   m_model;
	ConfigNodePtr    m_node;
	int              m_index;

};

#endif // DELETEKEYCOMMAND_HPP
