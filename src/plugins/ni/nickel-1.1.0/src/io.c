/******************************************************************************
 * Nickel - a library for hierarchical maps and .ini files
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: io.c 345 2008-01-19 17:02:54Z chaz $
 ******************************************************************************/


#include "./internal.h"
#include <bohr/ds_str.h>
#include <bohr/ni.h>

#include <stdio.h>
#include <stdlib.h>


// Define some character classes, some of which are duplicates of ctype.h
// classes (NOTE however that we DO NOT want to use ctype.h because we do NOT
// want locale-dependent parsing):

// Is space: ' ' or 9-13, which are tabs, linefeeds, etc.
#define isspace(c) ((c) == ' ' || ((c) >= 9 && (c) <= 13))

// Is octal digit: '0'-'7'.
#define isoctal(c) ((c) >= '0' && (c) <= '7')

// Is digit: '0'-'9' (used only in isxdigit and ascii2hex).
#define isdigit(c) ((c) >= '0' && (c) <= '9')

// Hex lower case: a-f only (not a ctype.h class; used only in isxdigit and
// ascii2hex).
#define isxlower(c) ((c) >= 'a' && (c) <= 'f')

// Hex upper case: A-F only.
#define isxupper(c) ((c) >= 'A' && (c) <= 'F')

// Is hex digit: digit or a-f or A-F.
#define isxdigit(c) (isdigit (c) || isxlower (c) || isxupper (c))


// Conversions between ascii values and integer values:

// Returns int value of octal ascii digit.
#define ascii2oct(c) ((c) - '0')

// Returns int value of hex ascii char
#define ascii2hex(c) (isdigit (c) ? ((c) - '0') : (isxlower (c) ? ((c) - 'a' + 10) : ((c) - 'A' + 10)))

// Sets a to the ascii hex digit of the first bits of c.
#define hex2ascii1(c, a) (a = ((c) >> 4) & 0xf, a = (a < 10 ? a + '0' : a - 10 + 'a'))

// Sets a to the ascii hex digit of the last bits of c.
#define hex2ascii2(c, a) (a = (c) &0xf, a = (a < 10 ? a + '0' : a - 10 + 'a'))


// Tokens for parsing (defined only to make it easier to change them if
// necessary).
#define T_EOL '\n' // end of line
#define T_OB '['   // open bracket, i.e. what introduces a section name
#define T_CB ']'   // close bracket, finishes section name
#define T_EQ '='   // equal sign, switches between key and value
#define T_OQ '"'   // open quote sign, starts off a quoted value
#define T_CQ '"'   // close quote, ends a quoted value
#define T_ESC '\\' // introduces escape sequence
#define T_X 'x'	   // after \, introduces a hex sequence
#define T_CMT ';'  // introduces a comment


// Converts the next char(s) into their escaped value.
static int DoEscape (file_buf * restrict fb, int * restrict out, int eol_valid);

// Writes a section/key name.
static int PutString (FILE * restrict f, const char * restrict str, int str_len, int is_key, int is_section);

// Puts a single UTF-8 character into the file.
static int PutUtf8Char (FILE * restrict f, const unsigned char * restrict str, int str_len);


/* Reads from fb until it finds the next identifier (either a section name or a
 * key of a key/value), and places the identifier name into idfr_out, and the
 * size of the buffer required to hold it into len_out.  Returns 0 if it
 * reaches the eof before it finds a valid identifier, or 1 if it found a
 * section identifier, or 2 if it found a key of a key/value pair.  May return
 * -1 on error.  idfr_out must be at least elektraNi_KEY_SIZE chars in length--this
 * function stops after that minus one, placing a NULL as the last character.
 * If this function returns 0, the contents of idfr_out and len_out may have
 * changed, or they may not've.  Note that to parse a .ini file correctly, if
 * this function returns 2, you must call GetValue() before another call to
 * GetNextIdentifier().  level_out will be filled with how many ['s were before
 * the section name, assuming the function returns 1.
 */
elektraNi_PRIVATE int GetNextIdentifier (file_buf * restrict fb, char * restrict idfr_out, int * restrict len_out, int * restrict level_out)
{
// State values for the FSM.
#define ST_DONE 0	  // stop parsing
#define ST_START 1	  // at start of line, skipping whitespace
#define ST_COMMENT 2	  // invalid character, ignore whole line
#define ST_SKIP 3	  // valid line found, skip rest of line
#define ST_IN_BRACKET 4	  // found [, look for section name identifier
#define ST_IN_SEC_ID 5	  // found identifier after [, put it into idfr_out
#define ST_IN_Q_SEC_ID 6  // found quotes inside [
#define ST_AFTER_Q_SEC 7  // after ["" before ]
#define ST_IN_KEY_ID 8	  // found key identifier as first non-space char, put it into idfr_out
#define ST_IN_Q_KEY_ID 9  // found quotes on the beginning of the line
#define ST_AFTER_Q_KEY 10 // after "" before =

	int rc = 0; // return code, initially set to "we got nothing"

	int len = 0;	   // length of output
	int graph_len = 0; // length of the string up to last graphical character (so we can skip trailing spaces)
	int level = 0;	   // how many ['s we catch at the beginning of this identifier
	int c;		   // current character

// Macro to conserve space in code below--updates graph_len if the input
// character isn't whitespace.
#define chkgr(c)                                                                                                                           \
	if (!isspace (c)) graph_len = len + 1

// Another space-saver--checks size of existing data and puts c into out,
// incrementing len if it'll fit.
#define put(c)                                                                                                                             \
	if (len < elektraNi_KEY_SIZE - 1) idfr_out[len++] = (c)

// Another space-saver--resets len and graph_len to 0, i.e. erases what we
// already had in the output.
#define invalid() (len = 0, graph_len = 0)

	int state = ST_START;	 // holds current state for FSM, duh
	while (state != ST_DONE) // do this until we're done
	{
		// Get char into c; if it's eof, dip out.
		if ((c = BufGetC (fb)) == EOF) break;

		switch (state)
		{
		// What state are we in?  See defines above for description of states.

		// Start state ignores whitespace, looking for [, an identifier, or an
		// invalid character.
		case ST_START:
			if (c == T_OB)
			{
				state = ST_IN_BRACKET; // if [, go to "in bracket" state
				level = 1;
			}
			else if (c == T_CMT)
			{
				state = ST_COMMENT;
			} // if ;, do comment then come back here
			else if (c == T_OQ)
			{
				state = ST_IN_Q_KEY_ID;
			} // if ", go to quoted key id
			else if (c == T_EQ)
			{
				state = ST_DONE; // if =, empty key, we'll allow it
				rc = 2;
			}
			else if (c == T_ESC)
			{
				state = ST_IN_KEY_ID; // if \, let key id handle it
				BufSeekBack (fb, 1);
			}
			else if (!isspace (c))
			{
				state = ST_IN_KEY_ID; // otherwise, if not a space, assume it's an identifier
				chkgr (c);
				put (c);
			}
			break;

		// Comment ignores till eol, goes back to start.
		case ST_COMMENT:
			if (c == T_EOL)
			{
				state = ST_START; // if we hit eol, go back to start
			}
			break;

		// Skip ignores till eol, then finishes.
		case ST_SKIP:
			if (c == T_EOL)
			{
				state = ST_DONE; // if we hit eol, we're done
			}
			break;

		// We found a [, look for an identifier.
		case ST_IN_BRACKET:
			if (c == T_EOL)
			{
				state = ST_START;
			} // if eol, false alarm, go back to start
			else if (c == T_CMT)
			{
				state = ST_COMMENT;
			} // if ;, do comment
			else if (c == T_OB)
			{
				++level;
			} // if another [, just up the bracket level
			else if (c == T_CB)
			{
				state = ST_SKIP; // if ], it's an empty section name--we'll allow it
				rc = 1;
			}
			else if (c == T_OQ)
			{
				state = ST_IN_Q_SEC_ID;
			} // if ", do quoted section name
			else if (c == T_ESC)
			{
				state = ST_IN_SEC_ID; // if \, let section id handle it
				BufSeekBack (fb, 1);
			}
			else if (!isspace (c))
			{
				state = ST_IN_SEC_ID; // otherwise, if it's not space, assume it's an identifier
				chkgr (c);
				put (c);
			}
			break;

		// In an identifier after a [, that is, a section name.
		case ST_IN_SEC_ID:
			if (c == T_EOL)
			{
				state = ST_START; // if eol, invalidate what we had saved and start over
				invalid ();
			}
			else if (c == T_CMT)
			{
				state = ST_COMMENT; // if ;, it's invalid so start over
				invalid ();
			}
			else if (c == T_CB)
			{
				state = ST_SKIP; // if ], it was valid, so set rc and ignore till eol
				rc = 1;
			}
			else
			{
				chkgr (c); // otherwise, if it's an escape sequence
				if (c == T_ESC)
				{
					DoEscape (fb, &c, 0); // translate it
				}
				put (c);
			} // and either way save it
			break;

		// In an identifier in quotes in a [, a quoted section name.
		case ST_IN_Q_SEC_ID:
			if (c == T_CQ)
			{
				state = ST_AFTER_Q_SEC;
			} // if we found close quote, go to after quote logic
			else
			{
				if (c == T_ESC)
				{			      // otherwise, if it's an escape sequence
					DoEscape (fb, &c, 0); // translate it
				}
				put (c);
			} // and either way put it in output
			break;

		// After ["something", looking for ].
		case ST_AFTER_Q_SEC:
			if (c == T_EOL)
			{
				state = ST_START; // if eol, it was bullshit, start over
				invalid ();
			}
			else if (c == T_OQ)
			{
				state = ST_IN_Q_SEC_ID;
			} // if we found another open quote, keep going
			else if (c == T_CB)
			{
				state = ST_SKIP; // if ], skip remainder of line (no trim spaces) and return ok
				rc = 1;
				graph_len = elektraNi_KEY_SIZE - 1;
			}
			else if (!isspace (c))
			{
				state = ST_COMMENT; // if any other char, skip rest of line, start over
				invalid ();
			}
			break;

		// In an identifier as first thing on line, that is, a key name.
		case ST_IN_KEY_ID:
			if (c == T_EOL)
			{
				state = ST_START; // if eol, invalidate and start over
				invalid ();
			}
			else if (c == T_CMT)
			{
				state = ST_COMMENT; // if ;, invalidate and start over
				invalid ();
			}
			else if (c == T_EQ)
			{
				state = ST_DONE; // if =, stop here and set rc to indicate value comes next
				rc = 2;
			}
			else
			{
				chkgr (c); // otherwise, if it's an escape sequence
				if (c == T_ESC)
				{
					DoEscape (fb, &c, 0); // translate that
				}
				put (c);
			} // either way, save it
			break;

		// In quotes at the beginning of the line, potentially a quoted key name.
		case ST_IN_Q_KEY_ID:
			if (c == T_CQ)
			{
				state = ST_AFTER_Q_KEY;
			} // if close quote, go to after quote logic
			else
			{
				if (c == T_ESC)
				{			      // otherwise, if escape sequence
					DoEscape (fb, &c, 0); // translate it
				}
				put (c);
			} // either way, put it into output
			break;

		// After "something", looking for =.
		case ST_AFTER_Q_KEY:
			if (c == T_EOL)
			{
				state = ST_START; // if eol, invalidate and start over
				invalid ();
			}
			else if (c == T_OQ)
			{
				state = ST_IN_Q_KEY_ID;
			} // if another open quote, keep going
			else if (c == T_EQ)
			{
				state = ST_DONE; // if =, we're GOOD and done (and don't strip spaces)
				rc = 2;
				graph_len = elektraNi_KEY_SIZE - 1;
			}
			else if (!isspace (c))
			{
				state = ST_COMMENT; // if any other char, invalidate and start over
				invalid ();
			}
			break;

		// This should never happen.
		default:
			rc = -1;	 // so set rc to error
			state = ST_DONE; // and stop in our tracks
			break;
		}
	}

	// Trim the length down if it was longer than the last graphical character.
	if (graph_len < len)
	{
		len = graph_len;
	}

	idfr_out[len] = '\0'; // null-terminate the output

	if (level_out)
	{
		*level_out = level; // set level_out if it wasn't NULL
	}
	if (len_out)
	{
		*len_out = len; // set len_out if it wasn't NULL
	}

	// Flush the buffer, since we'll never need anything in it again.
	BufFlush (fb);

	return rc;

// We don't need these to be defined anymore.
#undef ST_DONE
#undef ST_START
#undef ST_COMMENT
#undef ST_SKIP
#undef ST_IN_BRACKET
#undef ST_IN_SEC_ID
#undef ST_IN_Q_SEC_ID
#undef ST_AFTER_Q_SEC
#undef ST_IN_KEY_ID
#undef ST_IN_Q_KEY_ID
#undef ST_AFTER_Q_KEY
#undef chkgr
#undef put
#undef invalid
}

/* Parses a value of a key/value pair in the .ini file.  Must be called only
 * after GetNextIdentifier() returns 2, and it must be called then.  Returns 0
 * on error, or 1 if ok.  Puts the value into value_out.  Erases anything that
 * was in value_out before.
 */
elektraNi_PRIVATE int GetValue (file_buf * restrict fb, Ds_str * restrict value_out)
{
// State values for the FSM.
#define ST_DONE 0    // done parsing
#define ST_START 1   // at the start of a value, or on a new line of a continued value
#define ST_IGNORE 2  // ignoring till eol
#define ST_IN_Q 3    // inside the quotes of a quoted value, saving to output
#define ST_AFTER_Q 4 // after the end quote of quoted value, ignoring things (mostly)
#define ST_IN_U 5    // inside unquoted value, saving to output

	int rc = 1; // return code--default to ok

	int graph_len = 0; // length of string up to last graphical char
	int c;		   // current character

	int state = ST_START; // that state


// Macro to conserve space in code below--updates graph_len if the input
// character isn't whitespace.
#define chkgr(c)                                                                                                                           \
	if (!isspace (c)) graph_len = value_out->len + 1

// Macro to conserve space below--puts a char into value_out, dips out if
// error.
#define put(c)                                                                                                                             \
	do                                                                                                                                 \
	{                                                                                                                                  \
		if (value_out->len + 1 > value_out->size		/* check for space */                                              \
		    && !Ds_ResizeStr (value_out, value_out->size << 1)) /* grow if necessary */                                            \
		{                                                                                                                          \
			state = ST_DONE;                                                                                                   \
			rc = 0;                                                                                                            \
			break;                                                                                                             \
		}					/* quit everything if error */                                                     \
		value_out->str[value_out->len++] = (c); /* else set next char */                                                           \
	} while (0)

// Space-conserving macro--sets the state to the start value and sets
// graph_len to be the current length, so we don't go overboard getting rid
// of spaces.
#define cont() (state = ST_START, graph_len = value_out->len)

// Yet another--moves strlen back to the size of up to the last non-space
// character.
#define strip()                                                                                                                            \
	if (graph_len < value_out->len) value_out->len = graph_len


	value_out->len = 0; // set length to 0

	while (state != ST_DONE) // until we decide to stop
	{
		// Get next char; dip out (successfully) if EOF.
		if ((c = BufGetC (fb)) == EOF) break;

		switch (state)
		{
		// What state are we in?  See defines above for what these mean.

		// At the start of a value, or beginning of continued line.
		case ST_START:
			if (c == T_EOL)
			{
				state = ST_DONE;
			} // if eol or eof, it's valid even if we have nothing
			else if (c == T_CMT)
			{
				state = ST_IGNORE;
			} // if ;, ignore the whole thing
			else if (c == T_OQ)
			{
				state = ST_IN_Q;
			} // if ", go to quoted value
			else if (c == T_ESC)
			{
				state = ST_IN_U; // if \, do unquoted value, put \ back so no duplicated code
				BufSeekBack (fb, 1);
			}
			else if (!isspace (c))
			{
				state = ST_IN_U; // other non-ws chars, save and go to unquoted value
				chkgr (c);
				put (c);
			}
			break;

		// Ignoring till end of line--rc should have been set to valid before
		// going to this state if it is indeed valid.
		case ST_IGNORE:
			if (c == T_EOL)
			{
				state = ST_DONE;
			} // if eol/eof, we done an' shit
			break;

		// In quoted value.
		case ST_IN_Q:
			if (c == T_CQ)
			{
				state = ST_AFTER_Q;
			} // if end ", do after quotes deals
			else
			{
				if (c == T_ESC)		      // otherwise, look for escape start
					DoEscape (fb, &c, 0); // if escape sequence, get the escaped value instead
				put (c);
			} // output the maybe-escaped char
			break;

		// After end quote, looking for \ or more ""s.
		case ST_AFTER_Q:
			if (c == T_EOL)
			{
				state = ST_DONE;
			} // if eof/eol, we're done
			else if (c == T_OQ)
			{
				state = ST_IN_Q;
			} // if another ", keep parsing
			else
			{
				if (c == T_ESC // if \, look for eol
				    && DoEscape (fb, NULL, 1))
				{
					cont ();
				}
				else if (!isspace (c))
				{
					state = ST_IGNORE;
				}
			}
			break;

		// In unquoted value.
		case ST_IN_U:
			if (c == T_EOL)
			{
				state = ST_DONE; // if eof or eol, strip trailing space, we done
				strip ();
			}
			else if (c == T_CMT)
			{
				state = ST_IGNORE; // if ;, ignore till eol and we done
				strip ();
			}
			else
			{
				if (c == T_ESC) // otherwise, if escaping
				{
					if (DoEscape (fb, &c, 1)) // if it's the line continue
					{
						strip ();
						cont ();
					} // strip and continue
					else
					{
						chkgr (T_ESC);
					}
				} // if not line continue, it was graphical
				else
				{
					chkgr (c);
				} // if not escaping, check whether it was graphical
				put (c);
			} // and regardless, put something in the output
			break;

		// This should never happen.
		default:
			rc = 0;
			state = ST_DONE;
			break;
		}
	}

	if (rc)
	{
		// Null-terminate if no error.

		put ('\0'); // this might set rc to 0

		// put always adds to strlen, but we don't want that NULL in there
		if (rc) value_out->len--;
	}

	// Flush the buffer, since we'll never need anything in it again.
	BufFlush (fb);

	return rc;

#undef ST_DONE
#undef ST_START
#undef ST_IGNORE
#undef ST_IN_Q
#undef ST_AFTER_Q
#undef ST_IN_U
#undef chkgr
#undef put
#undef cont
#undef strip
}

/* Puts the section name into the file, surrounded by brackets.  Returns
 * nonzero on success, 0 on failure.  May have written only part of the string
 * to f if it fails.
 */
elektraNi_PRIVATE int PutSection (FILE * restrict f, const char * restrict name, int name_len, int level)
{
	int i;
	int success = 0;

	do
	{
		if (fputc (T_EOL, f) == EOF) // put an initial eol
			break;

		for (i = 0; i < level - 1; ++i) // put initial spaces
		{
			if (fputc (' ', f) == EOF) break;
		}
		if (i < level - 1) break;

		for (i = 0; i < level; ++i)
		{
			if (fputc (T_OB, f) == EOF) // put as many ['s as level indicates
				break;
		}
		if (i < level) break;

		if (!PutString (f, name, name_len, 0, 1)) // put section name
			break;

		for (i = 0; i < level; ++i)
		{
			if (fputc (T_CB, f) == EOF) // put as many ]'s as level indicates
				break;
		}
		if (i < level || fputc (T_EOL, f) == EOF) // put eol
			break;

		success = 1;
	} while (0);

	return success;
}

/* Puts the key/value pair into the file, separated by an =.  Returns nonzero
 * on success, 0 on failure.  May have written only part of the string to f if
 * it fails.
 */
elektraNi_PRIVATE int PutEntry (FILE * restrict f, const char * restrict key, int key_len, const char * restrict value, int value_len,
				int level)
{
	int i;
	int success = 0;

	do
	{
		for (i = 0; i < level - 1; ++i) // initial spaces
		{
			if (fputc (' ', f) == EOF) break;
		}
		if (i < level - 1) break;

		if (!PutString (f, key, key_len, 1, 0)) // key
			break;

		if (fputc (' ', f) == EOF     // space
		    || fputc (T_EQ, f) == EOF //=
		    || fputc (' ', f) == EOF) // space
			break;

		if (!PutString (f, value, value_len, 0, 0)) // value
			break;

		if (fputc (T_EOL, f) == EOF) // eol
			break;

		success = 1;
	} while (0);

	return success;
}

/* Internal to GetNextIdentifier() and GetValue()--assumes fb is on the
 * character AFTER a \ in an identifier/value.  Parses the next characters for
 * a valid escape sequence, returning the result in out, using a '\\' if it
 * wasn't valid.  GetNextIdentifier() and GetValue() put this character into
 * the output.  If eol_valid is nonzero, the function will accept \<ws>\n (the
 * line-continue escape) as a valid escape sequence, replacing it with a single
 * space.  Returns 1/0 indicating whether the line-continue escape sequence is
 * what was just parsed (thus, can only return 1 if eol_valid is 1).  Positions
 * fb so the next character will be the first character after the (maybe
 * invalid) escape sequence.  Either way, putting *out then the next characters
 * in fb into the output will result in the correct sequence.
 */
static int DoEscape (file_buf * restrict fb, int * restrict out, int eol_valid)
{
	int c;		   // current character
	int esc = -1;	   // value of escape sequence
	int line_cont = 0; // whether the line-continue escape is what we just parsed

	switch (c = BufGetC (fb))
	{

	// Normal escapes--put them in esc.
	case 'a':
		esc = '\a';
		break;
	case 'b':
		esc = '\b';
		break;
	case 'f':
		esc = '\f';
		break;
	case 'n':
		esc = '\n';
		break;
	case 'r':
		esc = '\r';
		break;
	case 't':
		esc = '\t';
		break;
	case 'v':
		esc = '\v';
		break;

	// These are the same after translation.
	case '\'':
	case '?':
	case T_ESC:
	case T_OQ:
#if (T_OQ != T_CQ)
	case T_CQ:
#endif
	case T_CMT:
	case T_OB:
	case T_CB:
	case T_EQ:
		esc = c;
		break;

	// Hex escape.  Look for hex chars.
	case T_X:
		c = BufGetC (fb);  // get next char
		if (!isxdigit (c)) // if it's NOT hex
		{
			BufSeekBack (fb, 1); // put it back
			break;
		}
		esc = ascii2hex (c); // otherwise, save hex digit value
		c = BufGetC (fb);    // and get next char
		if (!isxdigit (c))   // if it's not a hex char
		{
			BufSeekBack (fb, 1); // just go back one so it'll come out next
			break;
		}
		esc <<= 4;	      // otherwise, shift previous char over by 4
		esc += ascii2hex (c); // and add this char's value
		break;

	// Might be an octal escape or a line-continue escape.
	default:
		if (isoctal (c)) // if we've got an octal char
		{
			esc = ascii2oct (c); // get its int value
			c = BufGetC (fb);    // look at next character
			if (!isoctal (c))    // if not octal
			{
				BufSeekBack (fb, 1); // put it back, dip out
				break;
			}
			esc <<= 3;	      // if it is octal, shift previous value over 3
			esc += ascii2oct (c); // and add it
			c = BufGetC (fb);     // look at third character
			if (!isoctal (c))     // and do the exact same thing
			{
				BufSeekBack (fb, 1);
				break;
			}
			esc <<= 3;
			esc += ascii2oct (c);
			esc &= 0xff;
		} // or, if we should parse for line-contine escape
		else if (eol_valid && (c == EOF || isspace (c)))
		{
			size_t n = 0;	 // how many chars we've gone past initial space
			int comment = 0; // whether we found a comment

			while (1)
			{
				if (c == T_CMT) // if we found a comment
					comment = 1;

				// if we're done or char is invalid
				if (c == T_EOL || c == EOF || (!comment && !isspace (c))) break;

				c = BufGetC (fb); // get next char
				++n;		  // we've gone one farther
			}
			if (c != T_EOL) // if we stopped because of a non-space character or eof
			{
				BufSeekBack (fb, n); // invalid, so go back however many chars we just went forward
				break;		     // dip out
			}
			esc = ' ';     // otherwise, it's valid, so replace it with a single space
			line_cont = 1; // set our return value to true
		}
		break;
	}

	// If we didn't get a valid sequence, we gotta put back the backslash.
	if (esc < 0)
	{
		esc = T_ESC;	     // set it
		BufSeekBack (fb, 1); // and go back so we haven't gotten any other chars after backslash
	}
	if (out)
	{ // and set *out if we can
		*out = esc;
	}

	return line_cont; // return whether it was a line continuation escape
}

/* Outputs a string, surrounding it in quotes if necessary, and escaping
 * everything that needs it as it goes.
 */
static int PutString (FILE * restrict f, const char * restrict str, int str_len, int is_key, int is_section)
{
	int quote = 0;	 // whether to quote the string
	int success = 1; // return value
	int first = 1;	 // whether we're processing the first character
	int advance;	 // how many bytes to advance
	int c;

	if (str_len > 0)
	{
		c = *(str + str_len - 1); // set c to last character in string
		if (*str == ' ' || c == ' ')
		{		   // if initial or trailing spaces (\t etc. are
			quote = 1; // always escaped, so we just care about ' ')
		}
	}

	if (quote && fputc (T_OQ, f) == EOF)
	{
		success = 0;
	}

	while (success && str_len > 0)
	{
		c = *str;
		advance = 1;

		if (quote)
		{
			// In quotes, we just need to escape \ and "
			if (c == T_ESC || c == T_CQ)
			{
				if (fputc (T_ESC, f) == EOF || fputc (c, f) == EOF)
				{
					success = 0;
				}
			}
			else
			{
				if (!(advance = PutUtf8Char (f, (const unsigned char *) str, str_len)))
				{
					success = 0;
				}
			}
		}
		else
		{
			// Outside of quotes, we need to escape a lot of things:
			// in keys: always:   \ ; =
			//         if first: " [
			// in section names: always:   \ ; ]
			//                  if first: " [
			// in values: always:   \ ;
			//           if first: "

			if (c == T_ESC || c == T_CMT || (first && c == T_OQ) || (is_key && (c == T_EQ || (first && c == T_OB))) ||
			    (is_section && (c == T_CB || (first && c == T_OB))))
			{
				if (fputc (T_ESC, f) == EOF || fputc (c, f) == EOF)
				{
					success = 0;
				}
			}
			else
			{
				if (!(advance = PutUtf8Char (f, (const unsigned char *) str, str_len)))
				{
					success = 0;
				}
			}
		}

		str += advance;
		str_len -= advance;
		first = 0;
	}

	if (success && quote && fputc (T_CQ, f) == EOF)
	{
		success = 0;
	}

	return success;
}

/* Outputs a single UTF-8 character from the string.  Escapes anything that's
 * invalid UTF-8.  Returns how many bytes made up the character.
 */
static int PutUtf8Char (FILE * restrict f, const unsigned char * restrict str, int str_len)
{
	// check for ASCII range
	if (str[0] < 0x80)
	{
		// escape what's polite
		if (str[0] < 0x20 || str[0] == 0x7f)
		{
			if (fputc (T_ESC, f) == EOF) return 0;

			// see if we can make a pretty, non-hex escape
			int c = 0;
			switch (str[0])
			{
			case '\a':
				c = 'a';
				break;
			case '\b':
				c = 'b';
				break;
			case '\f':
				c = 'f';
				break;
			case '\n':
				c = 'n';
				break;
			case '\r':
				c = 'r';
				break;
			case '\t':
				c = 't';
				break;
			case '\v':
				c = 'v';
				break;
			}

			if (c)
			{
				if (fputc (c, f) == EOF) return 0;
			}
			else
			{
				// gotta do it the hard way

				int hd1, hd2;
				hex2ascii1 (str[0], hd1);
				hex2ascii2 (str[0], hd2);

				if (fputc (T_X, f) == EOF || fputc (hd1, f) == EOF || fputc (hd2, f) == EOF) return 0;
			}
		}
		else // doesn't warrant escaping
		{
			if (fputc (str[0], f) == EOF) return 0;
		}

		return 1; // ASCII are one byte long
	}

	// This huge if statement for valid UTF-8 characters comes right out of The
	// Unicode Standard, Version 5.0 electronic edition, section 3.9, table 3-7,
	// page 104 <http://www.unicode.org/versions/Unicode5.0.0/ch03.pdf>.  It's
	// also described by RFC 3629 <http://www.ietf.org/rfc/rfc3629.txt>,
	// in particular the ABNF grammar in section 4.  This handles excluding
	// overlong sequences, the surrogates, and just plain bytes out of range.
	if ((str[0] >= 0xc2 && str[0] <= 0xdf && str_len >= 2 && str[1] >= 0x80 && str[1] <= 0xbf) ||
	    (str[0] == 0xe0 && str_len >= 3 && str[1] >= 0xa0 && str[1] <= 0xbf && str[2] >= 0x80 && str[2] <= 0xbf) ||
	    (str[0] >= 0xe1 && str[0] <= 0xec && str_len >= 3 && str[1] >= 0x80 && str[1] <= 0xbf && str[2] >= 0x80 && str[2] <= 0xbf) ||
	    (str[0] == 0xed && str_len >= 3 && str[1] >= 0x80 && str[1] <= 0x9f && str[2] >= 0x80 && str[2] <= 0xbf) ||
	    (str[0] >= 0xee && str[0] <= 0xef && str_len >= 3 && str[1] >= 0x80 && str[1] <= 0xbf && str[2] >= 0x80 && str[2] <= 0xbf) ||
	    (str[0] == 0xf0 && str_len >= 4 && str[1] >= 0x90 && str[1] <= 0xbf && str[2] >= 0x80 && str[2] <= 0xbf && str[3] >= 0x80 &&
	     str[3] <= 0xbf) ||
	    (str[0] >= 0xf1 && str[0] <= 0xf3 && str_len >= 4 && str[1] >= 0x80 && str[1] <= 0xbf && str[2] >= 0x80 && str[2] <= 0xbf &&
	     str[3] >= 0x80 && str[3] <= 0xbf) ||
	    (str[0] == 0xf4 && str_len >= 4 && str[1] >= 0x80 && str[1] <= 0x8f && str[2] >= 0x80 && str[2] <= 0xbf && str[3] >= 0x80 &&
	     str[3] <= 0xbf))
	{
		// we've got a valid UTF-8 sequence

		int char_len = (str[0] < 0xe0 ? 2 : (str[0] < 0xf0 ? 3 : 4));

		for (int i = 0; i < char_len; ++i)
		{
			if (fputc (str[i], f) == EOF) return 0;
		}

		return char_len; // let the caller know how many bytes we ate
	}

	// if we got here, it's not ASCII and not valid UTF-8, so just output the
	// byte escaped and call it a day

	int hd1, hd2;
	hex2ascii1 (str[0], hd1);
	hex2ascii2 (str[0], hd2);

	if (fputc (T_ESC, f) == EOF || fputc (T_X, f) == EOF || fputc (hd1, f) == EOF || fputc (hd2, f) == EOF) return 0;

	return 1; // we only processed one byte
}
