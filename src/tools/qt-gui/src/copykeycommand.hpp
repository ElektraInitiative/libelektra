/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef COPYKEYCOMMAND_H
#define COPYKEYCOMMAND_H

#include "./confignode.hpp"
#include <QDebug>
#include <QUndoCommand>

/**
 * @brief The CopyKeyCommand class
 */

class CopyKeyCommand : public QUndoCommand
{
public:
	/**
	 * @brief The command to copy and paste a ConfigNode.
	 *
	 * @param type Declares if the ConfigNode is a single key or a branch.
	 * @param source The ConfigNode that is copied.
	 * @param target The ConfigNode that is the new parent node of the copied ConfigNode.
	 * @param parent
	 */
	explicit CopyKeyCommand (QString type, ConfigNodePtr source, ConfigNodePtr target, QUndoCommand * parent = nullptr);

	/**
	 * @brief undo
	 */
	virtual void undo () override;

	/**
	 * @brief redo
	 */
	virtual void redo () override;

private:
	ConfigNodePtr m_source;
	ConfigNodePtr m_target;
	bool m_isExpanded;
	int m_index;
};

#endif // COPYKEYCOMMAND_H
