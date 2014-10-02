#ifndef IMPORTCONFIGURATIONCOMMAND_H
#define IMPORTCONFIGURATIONCOMMAND_H

#include <QUndoCommand>
#include "treeviewmodel.hpp"

class ImportConfigurationCommand : public QUndoCommand
{

public:
    explicit ImportConfigurationCommand(ConfigNode *node, QString format, QString file, QString mergeStrategy, QUndoCommand *parent = 0);

    virtual void undo();
    virtual void redo();

};

#endif // IMPORTCONFIGURATIONCOMMAND_H
