#include "importconfigurationcommand.hpp"

ImportConfigurationCommand::ImportConfigurationCommand(ConfigNode *node, QString format, QString file, QString mergeStrategy, QUndoCommand *parent)
    : QUndoCommand(parent)
{

}

void ImportConfigurationCommand::undo()
{

}

void ImportConfigurationCommand::redo()
{

}
