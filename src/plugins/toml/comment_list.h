/**
 * @file comment_list.h
 *
 * @brief Used to handle comments on reading a TOML file.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TOML_COMMENT_LIST_H
#define ELEKTRA_PLUGIN_TOML_COMMENT_LIST_H

#include <elektra/core/key.h>
#include <stddef.h>

typedef struct _CommentList
{
	char * str;
	char * orig;
	struct _CommentList * next;
} CommentList;


/**
 * @brief Creates a new comment list linked list.
 *
 * @param comment Comment string for the first element in the list.
 * @param orig    The original bytes that appeared in the file,
 *                including preceding whitespace the starting `#`
 *                and leading whitespace after the `#`.
 * @retval Pointer to the new CommentList entry on success.
 * @retval NULL on memory allocation errors.
 *
 */
CommentList * commentListNew (const char * comment, const char * orig);

/**
 * @brief Completeley frees the given linked list.
 *
 * @param root Entry from which to start the deletion.
 */
void commentListFree (CommentList * root);


/**
 * @brief Append a new comment entry to the back of the list.
 *
 * @param back The list entry to which the new entry should be appended. It must not be NULL or have a non-NULL next element.
 *
 * @retval Pointer to the new back of the list.
 * @retval NULL when the new entry could not be allocated.
 */
CommentList * commentListAdd (CommentList * back, const char * comment, const char * orig);


/**
 * @brief Appends newline entries to the list.
 *
 * @param back Last element of the linked list. Must not be NULL.
 * @param newlineCount Amount of newlines which should be appended to the list. One newline equals to one new entry.
 *
 * @retval Pointer to the new back of the list.
 * @retval NULL when a new entry could not be allocated.
 */
CommentList * commentListAddNewlines (CommentList * back, size_t newlineCount);


/**
 * @brief Assigns all entries of the given CommentList to the given key as comment metakeys.
 *
 *
 * @param key The key on which to assign the metakeys
 * @param root The CommentList linked list which should be added to the Key
 *
 * @retval 0 On success, when all metakeys could be added
 * @retval Non-Zero On Error
 */
int keyAddCommentList (Key * key, CommentList * root);


/**
 * @brief Adds the given comment list as comment metakeys to the given key.
 *
 * The comment array numbering always starts from one and is increased for each element in the list.
 * Existing comment metakeys on the given keys are not checked and may be overwritten if existing.
 * For assigning inline comments, use the keyAddInlineComment(Key*, CommentList *) function.
 *
 * @param key The key on which to set the new comments.
 * @param root First element of the comment list.
 *
 * @retval 0 On success
 * @retval ERROR_MEMORY On allocation errors.
 */
int keyAddInlineComment (Key * key, CommentList * root);

#endif // ELEKTRA_PLUGIN_TOML_COMMENT_LIST_H
