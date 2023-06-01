/******************************************************************************
 * Nickel - a library for hierarchical maps and .ini files
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: buf.c 332 2008-01-13 18:32:02Z chaz $
 ******************************************************************************/


#include "./internal.h"
#include <bohr/ni.h>

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// How big to make the internal buffer when it's created.
#define INITIAL_BUF_SIZE 4096


/* Initializes the file_buf.
 */
elektraNi_PRIVATE int InitFileBuf (file_buf * restrict b, FILE * restrict f)
{
	*b = (file_buf) FILE_BUF_INIT;
	b->stream = f;
	return Ds_InitVector_uc (&b->buffer, INITIAL_BUF_SIZE);
}

/* Frees memory associated with the file_buf.
 */
elektraNi_PRIVATE void FreeFileBuf (file_buf * restrict b)
{
	Ds_FreeVector_uc (&b->buffer);
	*b = (file_buf) FILE_BUF_INIT;
}

/* Basically like fgetc on our file_buf.  Translates newlines automatically for
 * us.  Returns EOF if error or the file is done.
 */
elektraNi_PRIVATE int BufGetC (file_buf * restrict b)
{
	int c;

	if (b->pos >= b->buffer.num && !feof (b->stream))
	{
		// We need more data and can pull it.

		if (b->buffer.num + 2 <= b->buffer.cap || Ds_ResizeVector_uc (&b->buffer, b->buffer.cap << 1))
		{
			// We now have space, possibly as a result of just expanding.

			// Get a char; if it's not eof, set next char in buffer to it.
			if ((c = fgetc (b->stream)) != EOF)
			{
				if ((b->buffer.buf[b->buffer.num++] = c) == '\r')
				{
					// Translate \r or \r\n to just \n.

					b->buffer.buf[b->buffer.num - 1] = '\n';

					// Get the next one and ignore it if it's \n.
					if ((c = fgetc (b->stream)) != '\n' && c != EOF) b->buffer.buf[b->buffer.num++] = c;
				}
			}
		}
	}

	// Return the current character and advance the position pointer.
	c = (b->pos < b->buffer.num ? b->buffer.buf[b->pos] : EOF);
	b->pos++;
	return c;
}

/* Basically like fseek(b, -n_back, SEEK_CUR).  It undoes n_back BufGetC()
 * calls, regardless of whether BufGetC() returned EOF.
 */
elektraNi_PRIVATE void BufSeekBack (file_buf * restrict b, size_t n_back)
{
	assert (b->pos >= n_back);

	b->pos -= n_back;
}

/* Flushes the internal buffer without affecting the stream position.  This
 * lets us keep the memory footprint down.
 */
elektraNi_PRIVATE void BufFlush (file_buf * restrict b)
{
	b->pos = 0;
	b->buffer.num = 0;
}
