#ifndef ELEKTRA_PLUGIN_TOML_COMMENT_LIST_H
#define ELEKTRA_PLUGIN_TOML_COMMENT_LIST_H

#include <kdb.h>
#include <stddef.h>

typedef struct _CommentList
{
	char * str;
	size_t spaceCount;
	struct _CommentList * next;
} CommentList;

CommentList * commentListNew (const char * comment, size_t spaceCount);
void commentListFree (CommentList * root);
CommentList * commentListAdd (CommentList * back, const char * comment, size_t spaceCount);
CommentList * commentListAddNewlines (CommentList * back, size_t newlineCount);

int keyAddCommentList (Key * key, CommentList * root);
int keyAddInlineComment (Key * key, CommentList * root);

#endif // ELEKTRA_PLUGIN_TOML_COMMENT_LIST_H
