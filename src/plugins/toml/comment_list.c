#include "comment_list.h"

#include <assert.h>
#include <kdb.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utility.h"

static void keyAddComment (Key * key, const char * commentStr, size_t index, size_t spaceCount);

CommentList * commentListNew (const char * comment, size_t spaceCount)
{
	CommentList * newComment = elektraCalloc (sizeof (CommentList));
	if (comment != NULL)
	{
		newComment->str = strdup (comment);
	}
	newComment->spaceCount = spaceCount;
	return newComment;
}

void commentListFree (CommentList * root)
{
	while (root != NULL)
	{
		CommentList * nextComment = root->next;
		elektraFree (root->str);
		elektraFree (root);
		root = nextComment;
	}
}

CommentList * commentListAdd (CommentList * back, const char * comment, size_t spaceCount)
{
	back->next = commentListNew (comment, spaceCount);
	return back->next;
}

CommentList * commentListAddNewlines (CommentList * back, size_t newlineCount)
{
	CommentList * newBack = back;
	while (newlineCount > 0)
	{
		newBack = commentListAdd (newBack, NULL, 0);
		newlineCount--;
	}
	return newBack;
}

void keyAddCommentList (Key * key, CommentList * root)
{
	size_t index = 1;
	while (root != NULL)
	{
		keyAddComment (key, root->str, index++, root->spaceCount);
		root = root->next;
	}
}

void keyAddInlineComment (Key * key, CommentList * root)
{
	assert (root->next == NULL); // there is only 1 inline comment possible
	keyAddComment (key, root->str, 0, root->spaceCount);
}

static void keyAddComment (Key * key, const char * commentStr, size_t index, size_t spaceCount)
{
	// add comment str
	char * indexStr = indexToArrayString (index);
	size_t metaLen = strlen (indexStr) + 9;
	char * metaName = (char *) elektraCalloc (sizeof (char) * metaLen);
	snprintf (metaName, metaLen, "comment/%s", indexStr);
	elektraFree (indexStr);
	if (commentStr != NULL)
	{
		keySetMeta (key, metaName, commentStr);
	}

	// add start symbol
	size_t metaInfoLen = metaLen + 6;
	char * metaInfoName = (char *) elektraCalloc (sizeof (char) * metaInfoLen);
	snprintf (metaInfoName, metaInfoLen, "%s/start", metaName);
	if (commentStr != NULL)
	{
		keySetMeta (key, metaInfoName, "#");
	}
	else
	{
		keySetMeta (key, metaInfoName, "");
	}

	// add space count
	snprintf (metaInfoName, metaInfoLen, "%s/space", metaName);
	setPlainIntMeta (key, metaInfoName, spaceCount);

	elektraFree (metaInfoName);
	elektraFree (metaName);
}
