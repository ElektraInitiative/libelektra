/******************************************************************************
 * Nickel - a library for hierarchical maps and .ini files
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: ni.h 349 2008-01-19 18:18:22Z chaz $
 ******************************************************************************/


/******************************************************************************
 * Extensive documentation for Nickel is maintained on the Bohr Game Libraries
 * website: http://www.chaoslizard.org/devel/bohr/wiki/Docs/Ni
 *
 * I'll give a brief overview here anyway, for those who're too lazy to look at
 * the real documentation.  But really, check out the website for details.
 *
 * Nickel, at heart, stores nodes, each of which has a name, value, and any
 * number of children.  Children are stored in a hash table for quick lookup.
 * Built around this is a set of functions for building such a map from a
 * standard Windows .ini file, as well as writing back to a file format that, if
 * the map is carefully constructed or read from a .ini file, is roughly
 * compatible with the Windows GetPrivateProfile* functions.
 *
 * The only hard-coded size limit in Nickel is the length of node names, which
 * are capped at elektraNi_KEY_SIZE-1 (127, currently) bytes (not UTF-8 characters) to
 * give a reasonable maximum boundary on search time.  Otherwise, you're limited
 * only by available memory.  Thus, Nickel can parse files as big as the C
 * standard library can handle, and node values can be as big as will fit in
 * memory.  The number of children a node can have, as well as the size of the
 * entire tree, is also limited only by available memory.
 *
 * There is some wasted memory with each node--it allocates a number (32,
 * currently) of initial spaces (each sizeof(void *)) in the hash table of the
 * node's children, so resizing (an expensive operation) occurs less frequently.
 * Also, it doubles the capacity of the hash table when it gets 3/4 full, so
 * searching has a good average-case running time, but this means there's always
 * some wasted space.
 *
 * Following are the details of the files that Nickel can parse and write.
 *
 * Files should be UTF-8 (or plain ol' ASCII) encoded, though Nickel can handle
 * arbitrary binary data too.
 *
 * On reading, Nickel treats all newline flavors (at least, CR, LF, and CRLF) as
 * simply LF (\n in C).  This applies EVEN INSIDE QUOTED STRINGS (see below).
 * If you need a CR (\r in C) character in a node's name/value, you must escape
 * it (see below), that is, put a literal "\r" (backslash, lower r).
 *
 * Though this is not enforced, the first few characters of a Nickel-compatible
 * (as per version 1 of the API) .ini file should be:
 *
 *    ;Ni1
 *
 * (that's semicolon, upper N, lower i, digit 1) followed by a newline.  This
 * gives a machine-parsable sequence that future Nickel versions or other
 * parsers can use to identify the file format, while being a .ini comment (see
 * below) and thus ignored by anything that shouldn't care.  Though it's not
 * currently checked in Nickel (it'll try to parse anything you throw at it), it
 * may be in future versions.
 *
 * There are two kinds of data Nickel looks for in .ini files: names and values.
 * A name is a sequence of characters inside a number of [] (square brackets),
 * which denotes a section heading, OR before an = (equals sign), which denotes
 * that the value for the node follows.  Either way a name is encountered, a
 * node with that name will be created (or, if the named node already exists,
 * its value or children will be modified, depending on what comes next).
 *
 * Both names and values can have surrounding "" (double quotes).  When the
 * parser encounters an opening " (double quote), it parses all data until a
 * closing " (double quote), even spanning newlines, in the context of the name
 * or value.  The parser supports string concatenation whereby:
 *
 *    "some"  "thing"
 *
 * is equivalent to:
 *
 *    "something"
 *
 * Whitespace must be the only characters between closing " (double quote) and
 * re-opening " (double quote) for this to work (or, in values, there can be a
 * \ (backslash) and a newline--see escaping, below).
 *
 * Comments are initiated with the ; (semicolon).  Any ; (semicolon) outside of
 * double quotes begins a comment (except when escaped, see below).  Comments go
 * until the end of the line (and cannot be continued onto the next line with \
 * (backslash), as in some languages).
 *
 * Any line deemed invalid (e.g. blank or without an = (equals sign) or []
 * (square brackets)) is ignored as a comment.
 *
 * Nickel understands escape sequences similar to those in C.  Nickel translates
 * these escape sequences regardless of whether it's inside quotes or not.  It
 * treats the following sequences the same as C:
 *
 *    \a \b \f \n \r \t \v \' \" \? \\ \ooo and \xhh
 *
 * where ooo is up to 3 octal digits (0-7), hh is up to 2 hex digits (0-9, a-f,
 * or A-F), and all other characters are literally as they're written there.
 * The behavior of the hex escape (\xhh) may differ from how C handles it in
 * that I believe C allows the string of hex digits to be longer than 2.  And
 * I'm not sure how C handles octal constants bigger than 255, but Nickel takes
 * the whole octal value and uses the result modulus 256.  But that's just
 * semantics.
 *
 * Nickel also understands the following escape sequences, which exist only to
 * allow .ini reserved characters in names and values in the file:
 *
 *    \; \[ \] \=
 *
 * These are simply replaced with the character following the backslash.
 *
 * Note that escape sequences aren't always necessary, but are always parsed if
 * present.  Thus the following two lines are equivalent:
 *
 *    poo = \ \="\[    ; here, the = and [ are escaped
 *    poo = "\\ =\"["  ; here, double quotes are used and \ and " are escaped
 *
 * and indicate the node has a value of: \ (backslash), space, = (equals sign),
 * " (double quote), and [ (opening square bracket).  In the example, the double
 * quotes aren't necessary on the second line, but are there to illustrate that
 * the escape sequences are the same inside and outside of quotes (except that
 * in quotes, if a " (double quote) is encountered unescaped, it obviously ends
 * the quote).
 *
 * In values only, it is possible to escape a literal newline to instruct Nickel
 * to continue consuming the next line as part of the value.  This escape
 * consists of a \ (backslash) as the last non-whitespace character on a line,
 * ignoring comments.  If following a quoted string, the \ (backslash) should
 * come after the closing " (quote).  If the string immediately before the \
 * (backslash) is quoted, the \ (backslash)-newline pair is simply omitted, but
 * if the previous string was unquoted, the pair is replaced by a single space,
 * regardless of how much intervening space there actually was.  Thus, the
 * following lines all create equivalent nodes:
 *
 *    poo = abc def   ; just the string "abc def"
 *    poo = abc    \  ; no quotes, lots of spaces, and continue on the next line
 *          "def"     ; quotes here just for shits and giggles--not necessary
 *    poo = "abc " \  ; first line with quotes, then continue on the next line
 *          def       ; which could also have quotes ("def"), doesn't matter
 *
 * with the 7-character value "abc def".
 *
 * Surrounding names and values, whitespace is eaten by the parser.  This
 * happens after comments are removed (see the above example).  If you really
 * want a node whose name or value begins or ends with spaces, enclose it in
 * quotes.  Inside names and values, whitespace is maintained.  The exception to
 * this is with the line-continuation escape, where, if the string immediately
 * preceding the \ (backslash) was unquoted, spaces trailing on the top line or
 * beginning the second line are all replaced with a single space, or if the
 * preceding string was quoted, no space is inserted and initial space on the
 * following line is ignored.  See the above example for clarification on this.
 *
 * Lines giving values to named nodes (e.g. name = value) become children of the
 * node given in the most recent previous section (e.g. [name]), or the root
 * node if there have been no sections yet.  You can define subsections (e.g.
 * [[name]]) by specifying increasing numbers of [ (opening square bracket)
 * characters.  Sections with the same number of brackets become children of the
 * most recently preceding parent defined with fewer brackets; sections with
 * more brackets become deeper children, and sections with fewer brackets are
 * closer to the root.  The following example shows the root gaining two
 * children, "child1" (with value "teh suz" and child "sub_child1") and "child2"
 * (with its own child "sub_child2"):
 *
 *    child1 = teh suz  ; create child1 as a child of the root, give it a value
 *    [  child2  ]      ; another root child, anything below is child2's child
 *     [[sub_child2 ]]  ; sub_child2, having more [, is under child2
 *    [child1]          ; back up to root's child child1
 *     sub_child1=      ; this is a child of the previous section, i.e., child1
 *
 * (Spaces were added and the order was slightly obfuscated to show these things
 * are unimportant.)  Note that the parser ignores the number of ] (closing
 * square brackets) for the sake of keeping insignificant typos from causing
 * errors, but to stay compatible with future versions of Nickel or other
 * parsers, you should close each section with the same number of ] (closing
 * square brackets) as it was opened with.
 *
 * Also note that if you skip steps in your subsections, e.g.:
 *
 *    [child1]
 *     [[[ sub_sub_child1 ]]]
 *
 * empty children are inserted to make up the difference (here, "child1" has an
 * unnamed child, which has a child "sub_sub_child1").
 *
 * If a name is given more than once under the same parent in a file, the last
 * instance overrides all previous instances.  Thus:
 *
 *    [child1]
 *     sub = asdf
 *    [child1]
 *     sub = again
 *
 * gives "child1" a child called "sub", and gives it the value "again".  Note
 * that there is only one "child1", and the value "asdf" is overwritten by
 * "again" for "sub".
 *
 * It's valid to specify empty names.  Here:
 *
 *    []
 *    =
 *
 * creates a node with an empty name as the child of the root, and gives it a
 * child with an empty name and empty value.
 *
 * One more note: the parser is pretty lax--it will glean as much valid
 * information from a line as it can, ignoring errors if it can still get
 * something useful.  This can lead to weird results.  Consider the following
 * lines:
 *
 *    [section]  child = value
 *    # comments = cool!
 *
 * Here, because anything other than a comment after the ] (closing square
 * bracket) is invalid, the parser ignores "child = value" on the first line.
 * Also, because # (hash mark) isn't the standard for initiating a comment, the
 * second line creates a child of "section" named "# comments", with the value
 * "cool!".  Keep your files valid and standard and you won't have issues.
 *
 * This gives you an idea how the parser works.  More examples are in order for
 * future revisions of this document, but as I imagine this'll mostly be used
 * for simple .ini parsing, what's above is sufficient for now.
 *
 * While Nickel should be able to parse any Windows-compatible .ini file, it
 * won't necessarily output to a format that Windows will understand.  Some
 * circumstances it will generate an incompatible file are:
 *
 *   * The tree is more than one level deep
 *   * A node's name or value contains a special character: [ = \ ; etc. (as
 *     these may be escaped by Nickel)
 *   * A node's name or value has initial or trailing whitespace (as Nickel will
 *     put such a string inside quotes)
 *
 * Nickel can always parse a file it generates; it's just that Windows doesn't
 * understand some of Nickel's syntax, I believe, like nested headings, escaped
 * characters, and quotes.  Also, if you read a Windows-compatible .ini file
 * into memory and write it right back out to disk, you'll get a Windows-
 * compatible .ini file as long as no special characters are in any node's
 * value.  Nickel is fairly good about escaping special characters only when
 * necessary (as of v1.1.0, anyway), but if maximum compatibility with other
 * parsers is your goal, avoid special characters in names and values.
 *
 * It's also worth noting that since Nickel builds an in-memory map based on the
 * .ini file, and writing files uses the in-memory representation of the data,
 * comments in the file are entirely lost.  This is unfortunate, but means that
 * Nickel runs quickly enough to be used inside, say, a game, since accessing
 * the data doesn't require costly disk reads.
 *
 * It's also unfortunate that order is lost between reading and writing.  This
 * is due to hashing mixing things up.  The speed gain from hashing entries
 * greatly outweighs maintaining order, in my mind, and keeping an index of the
 * order nodes were added would take more overhead than I believe it's worth.
 *
 * Thus concludes my lengthy explanation of Nickel's .ini parser.
 ******************************************************************************/


#ifndef __bohr_ni_h__
#define __bohr_ni_h__

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>


// Controls importing in Windows.
#ifndef elektraNi_PUBLIC
#if (defined(_WIN32))
#define elektraNi_PUBLIC __declspec (dllimport)
#else
#define elektraNi_PUBLIC
#endif
#endif

// Nix non-critical C99 keywords in compilers that don't support them.
#if ((!defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L) && !defined(restrict))
#define restrict
#define _elektraNi_DEFINED_RESTRICT
#endif


#ifdef __cplusplus
extern "C" {
#endif


// The version of Nickel this header comes from.
#define elektraNi_HEADER_VERSION UINT32_C (0x01010000) // v1.1.0(.0)

// The version of Nickel you want when you include this file.
#ifndef elektraNi_VERSION
#define elektraNi_VERSION elektraNi_HEADER_VERSION
#endif


// Buffer size (in bytes) needed to hold keys and section names.
#define elektraNi_KEY_SIZE 128


// A node in a Nickel tree.
#ifndef _elektraNi_NODE_DEFINED
#define _elektraNi_NODE_DEFINED
typedef void * elektraNi_node;
#endif


/* Returns the version of Ni this library was compiled with.  Compare this
 * value to elektraNi_VERSION to see if the header matches.
 */
elektraNi_PUBLIC uint32_t elektraNi_GetVersion (void);

/* Allocates an entirely new node, not connected to any others, and returns it.
 * This new node is the root of ... well, so far, just itself, but anything you
 * add to it will be its children.  Returns NULL if it fails.  Note that to
 * allocate a node that will be a child of another node, you must use
 * elektraNi_GetChild() instead of this function; this only allocates entirely new
 * trees.
 */
elektraNi_PUBLIC elektraNi_node elektraNi_New (void);

/* Frees a node and any of the node's children, and their children, etc.  It
 * also checks if it has a parent and severs itself as a branch, so the entire
 * tree is kept synchronized.
 */
elektraNi_PUBLIC void elektraNi_Free (elektraNi_node restrict n);

/* Returns the name string of a node, or NULL if you pass an invalid node or a
 * root node (roots can't have names).  If len_out is non-NULL, it sticks the
 * returned string's length in len_out.  Note that all nodes have a name except
 * the root--but names can be "".
 */
elektraNi_PUBLIC const char * elektraNi_GetName (elektraNi_node restrict n, int * restrict len_out);

/* Returns the root of the tree the node belongs to.  You can check if a node
 * is a root node if n == elektraNi_GetRoot(n) is nonzero.  Returns NULL if you pass
 * an invalid node.
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetRoot (elektraNi_node restrict n);

/* Returns the parent node of a node, or NULL if you pass an invalid node or a
 * root node (as they have no parent).
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetParent (elektraNi_node restrict n);

/* Returns the number of children a node has, or 0 if you pass an invalid node.
 */
elektraNi_PUBLIC int elektraNi_GetNumChildren (elektraNi_node restrict n);

/* Useful for enumerating children--pass NULL as child to retrieve the node's
 * first child, then pass the previous return value as child to get the next,
 * etc.  Returns NULL if there's an error or there are no more children.
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetNextChild (elektraNi_node restrict n, elektraNi_node restrict child);

/* Returns the named child node of the node.  Specify the length of the name in
 * name_len, or use a negative number to have it calculated for you using
 * strlen().  Note that a NULL name is treated as "".  If add_if_new is
 * nonzero, if the named node is not found, it will be allocated as a child of
 * the node and returned.  You can see whether it was found or added if you
 * specify a non-NULL added_out.  Returns NULL if the node wasn't found AND
 * either add_if_new was 0 or it failed to allocate a new node (in either case,
 * added_out will be 0).
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetChild (elektraNi_node restrict n, const char * restrict name, int name_len, int add_if_new,
						    int * restrict added_out);

/* Returns the modified state of a node.  When nodes are created, they are "not
 * modified".  As soon as you call a elektraNi_SetValue() (or elektraNi_ValuePrint())
 * function, its modified state changes to "modified".  Use this to check
 * whether it's modified or not.  You can tell elektraNi_WriteFile() or -Stream() to
 * only write modified values--this is useful for having a global options file
 * with a local override file (you'd read the global options in, set all nodes
 * to "not modified", then read in the local options over top of it, and any
 * values the local file set will then be "modified", and when you write it
 * out, you only get the options set by the override file or ones you set since
 * it was loaded).
 */
elektraNi_PUBLIC int elektraNi_GetModified (elektraNi_node restrict n);

/* Explicitly sets the modified state for a node, and if recurse is nonzero,
 * all the node's children and their children, etc.  See the note in the above
 * function how this is useful.
 */
elektraNi_PUBLIC void elektraNi_SetModified (elektraNi_node restrict n, int modified, int recurse);

/* Returns a node's value.  Any node except a root node can have a value, but
 * not all of them do.  Until a node's value is set with a elektraNi_SetValue() (or
 * elektraNi_PrintValue()) function, it does NOT have a value.  Returns the value as a
 * string, or NULL if the function doesn't have a value or you pass an invalid
 * or root node.  If you care about the length of the value string, pass a non-
 * NULL len_out and its length is returned there.
 */
elektraNi_PUBLIC const char * elektraNi_GetValue (elektraNi_node restrict n, int * restrict len_out);

/* Returns a node's value interpreted as a long, or 0 if the node doesn't have
 * a value (see elektraNi_GetValue()).  Note that it uses strtol()'s base detection so
 * strings starting with 0 are considered octal, and 0x are considered hex.
 */
elektraNi_PUBLIC long elektraNi_GetValueInt (elektraNi_node restrict n);

/* Returns the node's value interpreted as a double, or 0.0 if the node doesn't
 * have a value (see elektraNi_GetValue()).
 */
elektraNi_PUBLIC double elektraNi_GetValueFloat (elektraNi_node restrict n);

/* Returns the node's value interpreted as a boolean integer (0/1), or 0 if the
 * node doesn't have a value (see elektraNi_GetValue()).  The following strings are
 * considered "true" and have a nonzero return value from this function: any
 * string starting with T or Y, the string "on", (case is ignored in all those
 * cases), or any nonzero integer.  Everything else is considered "false" and
 * will result in a 0 return value.
 */
elektraNi_PUBLIC int elektraNi_GetValueBool (elektraNi_node restrict n);

/* Calls vsscanf() on the node's value string directly.  format is the scanf()-
 * formatted argument string, and any arguments for scanf() are passed after
 * format.  Returns what scanf() returns: the number of translated items.
 */
elektraNi_PUBLIC int elektraNi_ValueScan (elektraNi_node restrict n, const char * restrict format, ...);

/* Same as above, except you pass a va_list instead of the args directly.
 */
elektraNi_PUBLIC int elektraNi_ValueVScan (elektraNi_node restrict n, const char * restrict format, va_list args);

/* Sets or removes a node's value.  If value is non-NULL, the value is set to
 * that string (which can be any length--specify its length in value_len, or
 * pass a negative value in value_len and it'll be calculated automatically
 * using strlen()).  If value is NULL (value_len is ignored in this case), its
 * value is removed--subsequent calls to elektraNi_GetValue() will return NULL (until
 * you set its value to something non-NULL, anyway).  Returns the length of
 * value, either as passed or calculated, or -1 if it fails, or 0 if you're
 * removing a value (so a negative return value always indicates error).  If
 * the value setting or removing succeeds, the node's modified state is set to
 * 1.  If setting the value fails, the contents of the value will NOT have
 * changed (and its modified state won't have changed either).  Note that for
 * setting the value, the value string you pass need not persist after the
 * call--its contents are copied.
 */
elektraNi_PUBLIC int elektraNi_SetValue (elektraNi_node restrict n, const char * restrict value, int value_len);

/* Sets a node's value to the value of a long.  Semantics are similar to those
 * of elektraNi_SetValue(), except you can't remove a node's value with this function.
 */
elektraNi_PUBLIC int elektraNi_SetValueInt (elektraNi_node restrict n, long value);

/* Sets a node's value to the value of a double.  Semantics are similar to
 * those of elektraNi_SetValue(), except you can't remove a node's value with this
 * function.
 */
elektraNi_PUBLIC int elektraNi_SetValueFloat (elektraNi_node restrict n, double value);

/* Sets a node's value to "true" or "false" based on a boolean integer.
 * Semantics are similar to those of elektraNi_SetValue(), except you can't remove a
 * node's value with this function.
 */
elektraNi_PUBLIC int elektraNi_SetValueBool (elektraNi_node restrict n, int value);

/* Uses printf() formatting to set the node's value.  You can't remove a node's
 * value with this function.  format is the printf()-formatted string, and any
 * arguments are passed after it.  Returns the resulting string length, or -1
 * if an error occurs (which may be as mundane as you passing an invalid or
 * root node).  If this function fails, unfortunately the contents of the value
 * MAY have changed due to printf() having complicated internal workings.  If
 * it fails, though, the modified state won't have changed, so you won't write
 * garbage if you're only writing modified values.  I don't know what else to
 * do, really.
 */
elektraNi_PUBLIC int elektraNi_ValuePrint (elektraNi_node restrict n, const char * restrict format, ...);

/* Same as above, except it expects a va_list instead of the args passed after
 * the format string.
 */
elektraNi_PUBLIC int elektraNi_ValueVPrint (elektraNi_node restrict n, const char * restrict format, va_list args);

/* Writes the contents of the tree starting at the node out to a file, in a
 * format that is parsable by elektraNi_ReadFile() or -Stream(), and roughly
 * compatible with .ini files.  Note that you can pass any node of a tree to
 * this function--only its children and downward are output.  If you pass
 * modified_only as nonzero, values are only output if the node's modified
 * state is true (either way, you must manually set the nodes' modified states
 * to 0 after calling this function, if you want to keep track of it that way).
 * Returns 0 on error, or nonzero on success.  The file is opened with
 * fopen(filename, "w"), so its contents will be erased.
 */
elektraNi_PUBLIC int elektraNi_WriteFile (elektraNi_node restrict n, const char * restrict filename, int modified_only);

/* Same as above, except instead of a filename, you can pass an already-open
 * file or a stream (like stdout).  The file must be writable, but need not be
 * seekable.
 */
elektraNi_PUBLIC int elektraNi_WriteStream (elektraNi_node restrict n, FILE * restrict stream, int modified_only);

/* Reads the contents of the file into the node and its children.  The node is
 * treated as the root of the resulting tree, but need not actually be the root
 * of the tree.  Any existing values in the tree are overwritten by the ones
 * from the file, or are created if they don't exist.  Returns 0 if it fails
 * (the contents of the node's children are undefined in this case), or nonzero
 * if it succeeds (invalid lines in a file won't make the function fail--
 * they'll just be skipped).  If fold_case is nonzero, all node names will be
 * converted to lowercase as they're read in.  Since "Name" and "name" are
 * different names, this makes the files less strict with case.
 */
elektraNi_PUBLIC int elektraNi_ReadFile (elektraNi_node restrict n, const char * restrict filename, int fold_case);

/* Same as above, except instead of a filename, you pass an already-open file
 * or stream (like stdin).  The file must be readable, but need not be
 * seekable.
 */
elektraNi_PUBLIC int elektraNi_ReadStream (elektraNi_node restrict n, FILE * restrict stream, int fold_case);


#ifdef __cplusplus
}
#endif

#ifdef _elektraNi_DEFINED_RESTRICT
#undef _elektraNi_DEFINED_RESTRICT
#undef restrict
#endif

#endif
