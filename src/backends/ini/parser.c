/***************************************************************************
            parser.c  -  Parsing Functions for Configuration
                             -------------------
    begin                : 01.03.2005
    updated              : 06.10.2005
    copyright            : (C) 2005 by Markus Raab
    email                : debian@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#include <ini.h>


/**States of parsing the key*/
#define STATE_BEG 0
#define STATE_KEY 1
#define STATE_VALUE 2
#define STATE_COMMENT 4
#define STATE_END 8
#define STATE_SEC_BEG 16
#define STATE_SEC_END 32
#define STATE_EMPTY 64
#define STATE_CHAR 128

int state;

#define CHAR_OK 0
#define CHAR_ADD 1
#define CHAR_SEC 2
#define CHAR_NEWLINE 3
#define CHAR_NULL 4
#define CHAR_ERR 5

/**parses a char
 * Logic of states is here. If you want a user defined
 * configuration format, this is your function.
 *
 * The pointer points to an array of chars. It must be
 * null terminated, because the function might try to
 * read the next char, but it will only change the char
 * pointed to.
 *
 * The change will take place, when a mulitbyte sequence
 * should be decoded.
 * @see convert_engine
 * for more information about that.
 * 
 * @return CHAR_ERR on error
 * @return CHAR_OK if char was correctly handeled
 * @return CHAR_ADD if parsing function could not handle the char
 * @return CHAR_SEC if section was read in gracefully
 *  
 * @param: pointer to char, char may be converted if needed
 *  
 *  always check if success, otherwise the char belongs
 *  to key/name/comment*/
int parse_buffer (char * p)
{
	char c = *p;
#ifdef VERBOSE 
	fprintf (stderr, "Will parse %c with state %d\n", c, state);
#endif
	if (c == '\0')
	{ /**End found, report that*/
		return CHAR_NULL;
	} else if (c == '\n')
	{ /**Newline (end of key) found, report that*/
		return CHAR_NEWLINE;
	} else if (state & STATE_CHAR)
	{ /*Convert multichar*/
		if (c == '0') *p = '\0';
		else if (c=='n') *p = '\n';
		else if (c=='b') *p = '\\';
		else if (c=='w') *p = ' ';
		else if (c=='e') *p = '=';
		else if (c=='s') *p = ';';
		else if (c=='r') *p = '#';
		state &= ~STATE_CHAR; /**Remove char bit*/
		return CHAR_ADD;
	}
	else if (state == STATE_BEG) /**Check first char*/
	{
		if (c == '[') {
			state = STATE_SEC_BEG;
			return CHAR_OK;
		} else if (c == '#') {
			state = STATE_EMPTY;
			return CHAR_ERR;
		} else { /**char belongs to key*/
			state = STATE_KEY;
			return CHAR_ADD;
		}
	} 
	else if (state == STATE_KEY && c == '=')
	{	/* value follows*/
		state = STATE_VALUE;
		return CHAR_OK;
	}
	else if (state == STATE_VALUE && c == ';')
	{	/* comment follows*/
		state = STATE_COMMENT;
		return CHAR_OK;
	}
	else if (state == STATE_SEC_BEG && c == ']')
	{	/* section correctly ended*/
		state = STATE_SEC_END;
		return CHAR_OK;
	} else if (c == '\\')
	{ /**Mulitchar sequence appeared*/
		state |= STATE_CHAR;
		return CHAR_OK;
	}
	else if (state == STATE_KEY) 
	{ /**Stay in that state, and add char*/
		return CHAR_ADD;
	}
	else if (state == STATE_VALUE)
	{ /**Stay in that state, and add char*/
		return CHAR_ADD;
	}
	else if (state == STATE_COMMENT)
	{ /**Stay in that state, and add char*/
		return CHAR_ADD;
	}
	return CHAR_ERR; /* Should not be reached*/
}

/**Will decode a char into a char sequence safe for use
 * in configuration files.
 *
 * <0NULL> -> \0 ... 0 byte
 * <enter> -> \n ... newline
 *    \    -> \b ... backslash
 * <space> -> \w ... whitespace
 *    =    -> \e ... equals
 *    ;    -> \s ... semikolon
 *    #    -> \r ... rhombus
 *
 * It will use the next char in the array. The size of the
 * array needs to be 2. It is not needed to be NULL terminated.
 *
 * If it is not a multichar, the second byte is guaranteed to
 * be NULL ('\0'). Otherwise see above.
 * 
 * @return 0 on success
 */
int convert_engine (char * p)
{
	char c = *p;
	char * n = p+1;
	*n = '\0';
	if (c == '\0') *n = '0';
	else if (c=='\n') *n = 'n';
	else if (c=='\\') *n = 'b';
	else if (c==' ') *n = 'w';
	else if (c=='=') *n = 'e';
	else if (c==';') *n = 's';
	else if (c=='#') *n = 'r';
	else return 0;
	*p = '\\';
#ifdef VERBOSE
	fprintf (stderr, "Handle %c to %c%c\n", c, *p, *n);
#endif
	return 0;
}

/**Returns the Stringlen for the char, multichars are handeld
 * correct.
 *
 * @param size: You may find it funny to give a strlen function
 * a length. But thus null Char are allowed, this is needed.
 * In fact, the function will only iterate above the size
 * and will calculate how much more it will need after converting*/
int convert_strlen (char * p, int size)
{
	int i;
	char c[2];
	int s;
	for (i=0,s=0; i<size; i++,s++)
	{
		c[0] = p[i];
		convert_engine(c);
		if (c[1] != '\0') s++;
	}
	return s;
}

/**Outputs a buffer to a stream. It converts the multichar
 * sequences as
 * @see convert_engine
 * does.
 * Thus null bytes are allowed (will be handeld saftey)
 * the buffer is NOT null terminated, but minimum as long
 * as the
 * @param size
 * says.
 * @return 0 on success*/
int convert_stream (char * buffer, int size, FILE * stream)
{
	int i;
	char conv [2];
	for (i=0; i<size; i++)
	{
		conv[0] = buffer[i];
		convert_engine (conv);
		if (conv[1] == '\0') fprintf (stream, "%c", conv[0]);
		else fprintf (stream, "%c%c", conv[0], conv[1]);
	}
	return 0;
}


/**
 * Read one key out of a file.
 *
 * It does not check the Name of the Key.
 *
 * @param key: Will contain information of key
 * @param root: The prefix name of the key
 *
 * state logic is inside parse_buffer
 * 
 * @return -1 on failure (memory, EOF, comment line), 0 on sucess
 * 
 * @ingroup ini
 */
int read_key (Key * key, char * root)
{
	char * buffer = NULL;
	char * buffer_value = NULL;
	char * buffer_key = NULL;
	char * buffer_comment = NULL;
	int rc;
	
	int i;
	
	int string_length = BUFFER_SIZE;
	int value_length = BUFFER_SIZE;
	int key_length = BUFFER_SIZE;
	int comment_length = BUFFER_SIZE;
	
	int v=0;	/* position of value*/
	int k=0;	/* position of key*/
	int c=0;	/* position of comment*/
	
	state= STATE_BEG;
	if (fc == NULL)
	{
#ifdef DEBUG
		fprintf (stderr, "File not opend\n");
#endif
		return -1;
	}
	
	buffer = (char*) malloc (BUFFER_SIZE+1);
	if (buffer == NULL) goto memerror;
	if (fgets (buffer, BUFFER_SIZE,fc) == NULL) {
#ifdef DEBUG
		fprintf (stderr, "End of File\n");
#endif
		free (buffer);
		/*Could not receive key*/
		return -1;
	}
	
	buffer_value = (char*) malloc (BUFFER_SIZE+1);
	if (buffer_value == NULL) goto memerror;
	buffer_key = (char*) malloc (BUFFER_SIZE+1);
	if (buffer_key == NULL) goto memerror;
	buffer_comment = (char*) malloc (BUFFER_SIZE+1);
	if (buffer_comment == NULL) goto memerror;
	
	for (i=0; i < string_length; i++) {
#ifdef VERBOSE
		fprintf (stderr, "Processing |%c|%d|\n", buffer[i], buffer[i]);
#endif
		rc = parse_buffer (&buffer[i]);
		if (rc == CHAR_OK) continue;
		else if (rc == CHAR_SEC)
		{
			//TODO: VERBOSE
			fprintf (stderr, "Now in section %s\n", buffer[i]);
		}
		else if (rc == CHAR_ERR) {
#ifdef DEBUG
			fprintf (stderr, "Error reading char\n");
#endif
			return -1;
		}
		else if (rc == CHAR_NEWLINE) { /* end of line found*/
#ifdef VERBOSE
			fprintf (stderr, "Found end of key (\\n)\n");
#endif
			break;
		}
		else if (rc == CHAR_NULL ) {	/* anticipated end?*/
			if (i==string_length-1) { /* no its not*/
				string_length += BUFFER_SIZE;
				if (srealloc ((void**) & buffer, string_length) < 0)
					goto memerror;
				else fprintf (stderr, "Realloc ok buffer (%p, %d)\n", buffer, string_length);
				fgets (buffer+string_length-BUFFER_SIZE,
					BUFFER_SIZE,fc);
			} else {
#ifdef DEBUG
				fprintf (stderr, "No Enter found in this line?\n");
#endif
				return -1;
			}
		}
		/*if (rc == CHAR_ADD); Fallthrough states*/
		else if (state == STATE_KEY) {
			buffer_key [k++] = buffer[i];
			if (k == key_length-1)
			{
				key_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_key, key_length) < 0)
					goto memerror;
				else fprintf (stderr, "Realloc ok key\n");
			}
		}
		else if (state == STATE_VALUE) {
			buffer_value [v++] = buffer[i];
			if (v == value_length-1) 
			{
				value_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_value, value_length) < 0) 
					goto memerror;
				else fprintf (stderr, "Realloc ok value\n");
			}
		}
		else if (state == STATE_COMMENT) {
			buffer_comment [c++] = buffer[i];
			if (c == comment_length-1)
			{
				comment_length += BUFFER_SIZE;
				if (srealloc ((void **) & buffer_comment, comment_length) < 0)
					goto memerror;
				else fprintf (stderr, "Realloc ok comment\n");
			}
		}
	}

	buffer_value [v] = 0;
	buffer_key [k] = 0;
	buffer_comment [c] = 0;	/* key eingelesen*/

	if (make_key (key, root, buffer_key, buffer_value, buffer_comment) == -1)
		goto memerror;
	
	free (buffer);
	free (buffer_value);
	free (buffer_key);
	free (buffer_comment);
	
	return 0; /* success */

memerror:
#ifdef DEBUG
	fprintf (stderr, "Allocation error\n");
#endif
	free (buffer);
	free (buffer_key);
	free (buffer_value);
	free (buffer_comment);
	errno = KDB_RET_NOMEM;
	return -1;
}

/**This function makes a key out of the
 * @param root is the root (prefix) of the keyname
 * @param buffer_key is the postfix of the keyname
 * @param buffer_value is the value
 * @param buffer_comment contains the comment
 * @return -1 on allocation error, 0 else*/
int make_key (Key * key, char * root, char * buffer_key, char * buffer_value, char * buffer_comment)
{
	char * buffer_name = NULL;

	if ((buffer_name = malloc (strlen(buffer_key) + strlen(root) + 2)) == NULL)
		return -1;
	
	
	buffer_name[0] = '\0';	/* buffer_name is empty*/
	strcat (buffer_name, root);
	strcat (buffer_name, "/");
	strcat (buffer_name, buffer_key);
	if (keySetName (key, buffer_name) == 0)
#ifdef DEBUG
		fprintf (stderr, "Unable to set name\n");
	else	fprintf (stderr, "Name set to %s\n", buffer_name);
#endif
	; /**Semikolon in the beginning because of DEBUG not set*/
	/**Freeing buffer_name here twice (see bottom) causes
	 * a very fancy libc bug, it crashes at another free()..*/
	
	if (keySetString (key, buffer_value) == 0)
#ifdef DEBUG
		fprintf (stderr, "Unable to set value\n");
	else 	fprintf (stderr, "Value set to %s\n", buffer_value);
#endif
	
	;if (keySetComment (key, buffer_comment) == 0) /**Semikolon needed at begin*/
#ifdef DEBUG
		fprintf (stderr, "Unable to set comment\n");
	else 	fprintf (stderr, "Comment set to %s\n", buffer_comment);
#endif
	; /*WARNING semikolon needed*/
	
	key->flags &= ~KEY_SWITCH_NEEDSYNC; /* remove sync flag*/
	
	free (buffer_name);
	
	return 0;	
}


/**
 * Writes out a key into file on pos.
 * keySet is the key which should be written there
 *
 * @ret Returnes 0 on success.
 * 
 * @ingroup ini
 */
int write_key (Key * setKey, long oldpos)
{
	long newpos;
	long needed_size, sname, svalue, scomment; /**needed sizes*/
	
	char * name;
	char * value;
	char * comment;

#ifdef DEBUG
	fprintf (stderr, "write_key (Key, pos: %ld)\n", oldpos);
#endif
	/** use setkey to set the key to wished values*/
	newpos = ftell (fc);
	name = strrchr (keyStealName (setKey),'/')+1;
	value = keyStealValue (setKey);
	comment = keyStealComment (setKey);
	
	sname = convert_strlen (name, strlen (name));
	svalue = convert_strlen (value, keyGetDataSize (setKey));
	scomment = convert_strlen (comment, keyGetCommentSize (setKey));
	needed_size = sname + svalue + scomment + 1; /* +\n */
	
	if (newpos - oldpos > needed_size)
	{
		shrink_file (oldpos, newpos - oldpos -needed_size);
	} else if (newpos - oldpos < needed_size) {
		enlarge_file (newpos, needed_size - (newpos - oldpos));
	}
	
#ifdef DEBUG
	fprintf(stderr, "Writing key to disc (pos: %ld|%ld|%ld) ...\n",
		oldpos, newpos, needed_size);
#endif
	fseek (fc, oldpos, SEEK_SET);
	
	convert_stream (name, sname, fc);
	fwrite ("=", 1,1,fc);
	convert_stream (value, svalue, fc);
	fwrite (";", 1,1,fc);
	convert_stream (comment, scomment, fc);
	fwrite ("\n", 1,1,fc);

#ifdef DEBUG
	newpos = ftell (fc);
	fprintf (stderr, "Real endpos: %ld\n", newpos);
	fprintf (stderr, "key: %s, value: %s, comment: %s\n", 
		setKey->key, (char *) setKey->data, setKey->comment);
#endif

			
	return 0;
}

/**
 * Removes a key on pos.
 * keySet is the key which says the size it has.
 * So you must get the key first.
 *
 * @ret Returnes 0 on success.
 * 
 * @ingroup ini
 */
int remove_key (Key * setKey, long oldpos)
{
	long newpos;
	long delete_size, sname, svalue, scomment; /**needed sizes*/
	
	char * name;
	char * value;
	char * comment;
	name = strrchr (keyStealName (setKey),'/')+1;
	value = keyStealValue (setKey);
	comment = keyStealComment (setKey);
	
#ifdef DEBUG
	fprintf (stderr, "remove_key (Key, pos)\n");
#endif
	/** use setkey to set the key to wished values*/
	newpos = ftell (fc);
	
	sname = convert_strlen (name, strlen (name));
	svalue = convert_strlen (value, keyGetDataSize (setKey));
	scomment = convert_strlen (comment, keyGetCommentSize (setKey));
	delete_size = sname + svalue + scomment + 1; /* +\n */
	
	shrink_file (oldpos, newpos - oldpos - delete_size);
	
#ifdef DEBUG
	fprintf(stderr, "Deleting key on disc (pos: %ld|%ld|%ld) ...\n",
		oldpos, newpos, delete_size);
#endif
	return 0;
}

