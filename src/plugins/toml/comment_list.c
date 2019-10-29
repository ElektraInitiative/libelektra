#include "comment_list.h"

#include <assert.h>
#include <kdb.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "utility.h"

static int keyAddComment (Key * key, const char * commentStr, size_t index, size_t spaceCount);

CommentList * commentListNew (const char * comment, size_t spaceCount)
{
	CommentList * newComment = elektraCalloc (sizeof (CommentList));
	if (newComment == NULL)
	{
		return NULL;
	}
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
	assert (back != NULL);
	assert (back->next == NULL);
	back->next = commentListNew (comment, spaceCount);
	return back->next;
}

CommentList * commentListAddNewlines (CommentList * back, size_t newlineCount)
{
	assert (back != NULL);
	CommentList * newBack = back;
	while (newlineCount > 0)
	{
		newBack = commentListAdd (newBack, NULL, 0);
		if (newBack == NULL)
		{
			return NULL;
		}
		newlineCount--;
	}
	return newBack;
}

int keyAddCommentList (Key * key, CommentList * root)
{
	int errno = 0;
	size_t index = 1;
	while (root != NULL && errno == 0)
	{
		errno = keyAddComment (key, root->str, index++, root->spaceCount);
		root = root->next;
	}
	return errno;
}

int keyAddInlineComment (Key * key, CommentList * root)
{
	if (root->next != NULL)
	{
		return ERROR_INTERNAL;
	}
	return keyAddComment (key, root->str, 0, root->spaceCount);
}

static int keyAddComment (Key * key, const char * commentStr, size_t index, size_t spaceCount)
{
	// add comment str
	char * indexStr = indexToArrayString (index);
	if (indexStr == NULL)
	{
		return ERROR_MEMORY;
	}
	size_t metaLen = elektraStrLen (indexStr) + 8;
	char * metaName = (char *) elektraCalloc (sizeof (char) * metaLen);
	if (metaName == NULL)
	{
		elektraFree (indexStr);
		return ERROR_MEMORY;
	}
	snprintf (metaName, metaLen, "comment/%s", indexStr);
	elektraFree (indexStr);
	if (commentStr != NULL)
	{
		keySetMeta (key, metaName, commentStr);
	}

	// add start symbol
	size_t metaInfoLen = metaLen + 6;
	char * metaInfoName = (char *) elektraCalloc (sizeof (char) * metaInfoLen);
	if (metaInfoName == NULL)
	{
		elektraFree (metaName);
		return ERROR_MEMORY;
	}
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
	return 0;
}
