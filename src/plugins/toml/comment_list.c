/**
 * @file comment_list.c
 *
 * @brief Functions for handling the CommentList struct.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./comment_list.h"

#include <elektra/core/key.h>
#include <internal/utility/assert.h>
#include <internal/utility/string.h>
#include <internal/utility/alloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./error.h"
#include "./utility.h"

static int keyAddComment (Key * key, const char * commentStr, const char * origStr, size_t index);

CommentList * commentListNew (const char * comment, const char * orig)
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
	if (orig != NULL)
	{
		newComment->orig = strdup (orig);
	}
	return newComment;
}

void commentListFree (CommentList * root)
{
	while (root != NULL)
	{
		CommentList * nextComment = root->next;
		if (root->str != NULL)
		{
			elektraFree (root->str);
		}
		if (root->orig != NULL)
		{
			elektraFree (root->orig);
		}
		elektraFree (root);
		root = nextComment;
	}
}

CommentList * commentListAdd (CommentList * back, const char * comment, const char * orig)
{
	ELEKTRA_ASSERT (back != NULL, "Back expected to be non-NULL, but was NULL");
	ELEKTRA_ASSERT (back->next == NULL, "Back->next expected to be NULL, but was not NULL");
	back->next = commentListNew (comment, orig);
	return back->next;
}

CommentList * commentListAddNewlines (CommentList * back, size_t newlineCount)
{
	ELEKTRA_ASSERT (back != NULL, "Back expected to be non-NULL, but was NULL");
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
	size_t index = 1;
	int err = 0;
	while (root != NULL && err == 0)
	{
		err = keyAddComment (key, root->str, root->orig, index++);
		root = root->next;
	}
	return err;
}

int keyAddInlineComment (Key * key, CommentList * root)
{
	if (root->next != NULL)
	{
		return ERROR_INTERNAL;
	}
	return keyAddComment (key, root->str, root->orig, 0);
}

static int keyAddComment (Key * key, const char * commentStr, const char * origStr, size_t index)
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

	// add preceding whitespace
	snprintf (metaInfoName, metaInfoLen, "%s/space", metaName);
	if (commentStr != NULL)
	{
		size_t len = strspn (origStr, " \t");
		char * startSeq = elektraMemDup (origStr, len + 1);
		startSeq[len] = '\0';
		if (startSeq == NULL)
		{
			return ERROR_MEMORY;
		}

		keySetMeta (key, metaInfoName, startSeq);
		elektraFree (startSeq);
	}
	else
	{
		keySetMeta (key, metaInfoName, "");
	}

	elektraFree (metaInfoName);
	elektraFree (metaName);
	return 0;
}
