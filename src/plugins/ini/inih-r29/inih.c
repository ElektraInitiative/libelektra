/* inih -- simple .INI file parser

   inih is released under the New BSD license (see LICENSE.txt). Go to the project
   home page for more info:

http://code.google.com/p/inih/

*/

#include "inih.h"
#include <ctype.h>
#include <kdbassert.h>
#include <kdblogger.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !INI_USE_STACK
#include <stdlib.h>
#endif

#define MAX_SECTION 65535
#define MAX_NAME 65535

/* Strip whitespace chars off end of given string, in place. Return s. */
static char * rstrip (char * s)
{
	char * p = s + strlen (s);
	while (p > s && isspace ((unsigned char) (*--p)))
	{
		*p = '\0';
	}
	return s;
}

/* Return pointer to first non-whitespace char in given string. */
static char * lskip (const char * s)
{
	while (*s && isspace ((unsigned char) (*s)))
	{
		s++;
	}
	return (char *) s;
}

/* Return pointer to first char c or ';' comment in given string, or pointer to
   null at end of string if neither found. ';' must be prefixed by a whitespace
   character to register as a comment. */
static char * find_char_or_comment (const char * s, char c)
{
	int was_whitespace = 0;
	while (*s && *s != c && !(was_whitespace && *s == ';'))
	{
		was_whitespace = isspace ((unsigned char) (*s));
		s++;
	}
	return (char *) s;
}

/* Version of strncpy that ensures dest (size bytes) is null-terminated. */
static char * strncpy0 (char * dest, const char * src, size_t size)
{
#if defined(__GNUC__) && __GNUC__ >= 8 && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-truncation"
#endif
	strncpy (dest, src, size);
#if defined(__GNUC__) && __GNUC__ >= 8 && !defined(__clang__)
#pragma GCC diagnostic pop
#endif
	dest[size - 1] = '\0';
	return dest;
}

static int isContinuation (const char * line, const struct IniConfig * config)
{
	if (!(*line)) return 0;
	return !strncmp (line, config->continuationString, strlen (config->continuationString));
}

static int isSection (const char * line)
{
	if (!(*line)) return 0;
	if (*line == '[') return 1;
	return 0;
}

static int isComment (const char * line)
{
	if (!(*line)) return 0;
	if (*line == '#' || *line == ';') return 1;
	return 0;
}

/* See documentation in header file. */
int ini_parse_file (FILE * file, const struct IniConfig * config, void * user)
{
	/* Uses a fair bit of stack (use heap instead if you need to) */
	char * line;

	char section[MAX_SECTION] = "";
	char prev_name[MAX_NAME] = "";

	char * start;
	char * end;
	char * name;
	char * value;
	char delim = config->delim;
	int lineno = 0;
	int error = 0;

	line = (char *) malloc (INI_MAX_LINE);

	ELEKTRA_LOG_DEBUG ("Allocated memory for line");

	if (!line)
	{
		return -2;
	}

	/* Scan through file line by line */
	while (fgets (line, INI_MAX_LINE, file) != NULL)
	{
		lineno++;
		ELEKTRA_LOG_DEBUG ("Read line %d with content “%s”", lineno, line);

		start = line;
#if INI_ALLOW_BOM
		if (lineno == 1 && (unsigned char) start[0] == 0xEF && (unsigned char) start[1] == 0xBB && (unsigned char) start[2] == 0xBF)
		{
			start += 3;
			config->bomHandler (user, 1);
		}
		else
		{
			config->bomHandler (user, 0);
		}
#endif
		if (*start == '\n')
		{
			if (!config->commentHandler (user, "") && !error) error = lineno;
			continue;
		}
		start = lskip (line);
		if (*start == '\0')
		{
			if (!config->commentHandler (user, "") && !error) error = lineno;
			continue;
		}
		if (isContinuation (line, config) && config->supportMultiline && *prev_name)
		{
			start = line + strlen (config->continuationString);
			if (*start == '"') ++start;
			end = line + (strlen (line) - 1);
			while ((*end != '"') && (!isprint (*end)) && (end > start))
			{
				if (*end == '\n') *end = '\0';
				--end;
			}
			if (*end == '"') *end = '\0';

			if (!config->keyHandler (user, section, prev_name, start, 1) && !error) error = lineno;
		}
		else if (isSection (line))
		{
			ELEKTRA_LOG_DEBUG ("Line contains a section");
			end = line + (strlen (line) - 1);
			while (end > start)
			{
				if (*end == ']') break;
				--end;
			}
			++start;
			if (*end == ']')
			{
				*end = '\0';
				strncpy0 (section, start, sizeof (section));
				*prev_name = '\0';
				ELEKTRA_LOG_DEBUG ("Found section “%s”", section);

				size_t numberBackslashes = 0;
				for (char * endSection = section + strlen (section) - 1; endSection >= section && *endSection == '\\';
				     endSection--)
				{
					numberBackslashes++;
				}
				if (numberBackslashes % 2 != 0)
				{
					ELEKTRA_LOG_WARNING ("Found uneven number of backlashes at end of section");
					error = lineno;
					break;
				}

				if (!config->sectionHandler (user, section) && !error) error = lineno;
			}
			else
			{
				end = line + (strlen (line) - 1);
				if (*end == '\n')
				{
					strncpy0 (section, start, sizeof (section));
					while (fgets (line, INI_MAX_LINE, file))
					{
						end = line + (strlen (line) - 1);
						while ((end > line) && *end != ']')
							--end;
						if (*end == ']')
						{
							*end = '\0';
							strncpy0 (section + strlen (section), line, sizeof (section) - strlen (section));
							*prev_name = '\0';
							if (!config->sectionHandler (user, section) && !error) error = lineno;
							break;
						}
						else
						{
							strncpy0 (section + strlen (section), line, sizeof (section) - strlen (section));
						}
					}
				}
				else
				{
					error = lineno;
				}
			}
		}
		else if (isComment (line))
		{
			start = line;
			end = line + (strlen (line) - 1);
			if (*end == '\n') *end = '\0';
			if (!config->commentHandler (user, start) && !error) error = lineno;
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("Line contains a key");

			char * ptr = start;
			unsigned int assign = 0;
			ELEKTRA_LOG_DEBUG ("Search for delimiter “%c”", delim);
			while (*ptr)
			{
				if (*ptr == delim)
				{
					++assign;
				}
				++ptr;
			}

			if (assign == 1)
			{
				ELEKTRA_LOG_DEBUG ("Found exactly one delimiter");
				name = start;
				end = strchr (start, delim);
				if (*name == '"')
				{
					ELEKTRA_LOG_DEBUG ("Name starts with double quote character");
					++name;
					if (*(end - 2) == '"')
					{
						*(end - 2) = '\0';
					}
					else if (*(end - 1) == '"')
					{
						*(end - 1) = '\0';
					}
					else
					{
						ELEKTRA_LOG_DEBUG ("Did not find closing double quote characters in current line");
						strncpy0 (prev_name, name, sizeof (prev_name));
						while (fgets (line, INI_MAX_LINE, file))
						{
							ELEKTRA_LOG_DEBUG ("Read continuation line with content “%s”", line);
							end = line + (strlen (line) - 1);
							while (end > line && *end != '"')
								--end;
							if (*end == '"')
							{
								ELEKTRA_LOG_DEBUG ("Found closing double quote character");
								*(end++) = '\0';
								strncpy0 (prev_name + strlen (prev_name), line,
									  sizeof (prev_name) - strlen (prev_name));
								break;
							}
							else
							{
								ELEKTRA_LOG_DEBUG ("Found name continuation");
								strncpy (prev_name + strlen (prev_name), line,
									 sizeof (prev_name) - strlen (prev_name));
							}
							ELEKTRA_LOG_DEBUG ("New extended name is “%s”", prev_name);
						}
						name = prev_name;
						ELEKTRA_LOG_DEBUG ("Name of key is “%s”", name);
					}
				}
				if (*end != delim)
				{
					ELEKTRA_LOG_DEBUG ("Search for delimiter in “%s”", end);
					ptr = lskip (end + 1);
					end = strchr (ptr, delim);
					if (end && *end == delim)
					{
						*end = '\0';
						ELEKTRA_LOG_DEBUG ("Found delimiter – New name is “%s”", end);
					}
					else
					{
						ELEKTRA_LOG_WARNING ("Unable to find delimiter");
						error = lineno;
						break;
					}
				}
				else
				{
					*end = '\0';
				}
				if (name != prev_name && end > line)
				{
					rstrip (end - 1);
				}
				value = lskip (end + 1);
				end = find_char_or_comment (value, '\0');
				if (*end == ';') *end = '\0';
				rstrip (value);
				if (*value == '"')
				{
					*(value++) = '\0';
					while ((*end != '"') && !isprint (*end) && end > value)
						--end;
					if (*end == '"') *end = '\0';
				}
				if (prev_name != name) strncpy0 (prev_name, name, sizeof (prev_name));
				if (!config->keyHandler (user, section, name, value, 0) && !error) error = lineno;
			}
			else if (assign == 0)
			{
				ELEKTRA_LOG_DEBUG ("Found no delimiter");
				if (*start == '"')
				{
					ELEKTRA_LOG_DEBUG ("Found initial double quote character");
					++start;
					end = line + (strlen (line) - 1);
					while (end > start && *end != '"')
						--end;
					if (*end == '"' && end != start)
					{
						*end = '\0';
						if (!config->keyHandler (user, section, start, NULL, 0) && !error) error = lineno;
					}
					else
					{
						ELEKTRA_LOG_DEBUG ("Did not find closing double quote character");
						strncpy0 (prev_name, start, sizeof (prev_name));
						while (fgets (line, INI_MAX_LINE, file))
						{
							end = line + (strlen (line) - 1);
							ELEKTRA_LOG_DEBUG ("Read continuation line with content “%s”", line);
							while (end > line && *end != '"')
								--end;
							if (*end == '"')
							{
								ELEKTRA_LOG_DEBUG ("Found closing double quote character");
								*end = '\0';
								strncpy0 (prev_name + strlen (prev_name), line,
									  sizeof (prev_name) - strlen (prev_name));
								break;
							}
							else
							{
								ELEKTRA_LOG_DEBUG ("Found name continuation");
								strncpy (prev_name + strlen (prev_name), line,
									 sizeof (prev_name) - strlen (prev_name));
							}
							ELEKTRA_LOG_DEBUG ("New extended name is “%s”", prev_name);
						}
						name = prev_name;
						ptr = end + 1;
						end = strchr (ptr, '=');
						if (!end) end = strchr (ptr, ':');
						if (!end)
						{
							if (!config->keyHandler (user, section, name, NULL, 0) && !error) error = lineno;
						}
						else
						{
							*end = '\0';
							value = lskip (end + 1);
							if (*value == '"') end = find_char_or_comment (value, '\0');
							if (*end == ';') *end = '\0';
							rstrip (value);
							if (*value == '"' || *(value + 1) == '"')
							{
								if (*value == '"')
									*(value++) = '\0';
								else if (*(value + 1) == '"')
								{
									*(value + 1) = '\0';
									value += 2;
								}
								while ((*end != '"') && !isprint (*end) && end > value)
									--end;
								if (*end == '"') *end = '\0';
							}
							if (prev_name != name) strncpy0 (prev_name, name, sizeof (prev_name));
							if (!config->keyHandler (user, section, name, value, 0) && !error) error = lineno;
						}
					}
				}
				else
				{
					name = rstrip (start);
					strncpy0 (prev_name, name, sizeof (prev_name));
					if (!config->keyHandler (user, section, name, NULL, 0) && !error) error = lineno;
				}
			}
			else
			{
				ELEKTRA_LOG_DEBUG ("Found multiple delimiters");

				end = start + 1;
				if (*start == '"')
				{
					/* Quoted Name:
						- The name has to end with a double quote
						- We do not allow any double quotes inside the name
					 */
					name = start + 1;
					while (*end && *end != '"')
					{
						end++;
					}
					if (!*end)
					{
						error = lineno;
						break;
					}
					*end = '\0';

					value = end + 1;
					value = lskip (value);
					if (!*value || *value != delim)
					{
						error = lineno;
						break;
					}
					value++;
				}
				else
				{
					/* Unquoted Name:
						- The name can not contain a delimiter unless it is the very first character
						- Trailing whitespace is removed from the name
					 */
					name = start;
					while (*end && *end != delim)
					{
						end++;
					}
					if (!end)
					{
						error = lineno;
						break;
					}
					*end = '\0';
					start = rstrip (start);
					value = end + 1;
				}
				value = lskip (value);

				end = find_char_or_comment (value, '\0');
				if (*end == ';') *end = '\0';
				rstrip (value);
				if (*value == '"' || *(value + 1) == '"')
				{
					if (*value == '"')
						*(value++) = '\0';
					else if (*(value + 1) == '"')
					{
						*(value + 1) = '\0';
						value += 2;
					}
					while ((*end != '"') && !isprint (*end) && end > value)
						--end;
					if (*end == '"') *end = '\0';
				}
				strncpy0 (prev_name, name, sizeof (prev_name));

				if (!config->keyHandler (user, section, name, value, 0) && !error) error = lineno;
			}
		}

#if INI_STOP_ON_FIRST_ERROR
		if (error) break;
#endif
	}

	free (line);
	return error;
}

/* See documentation in header file. */
int ini_parse (const char * filename, const struct IniConfig * config, void * user)
{
	FILE * file;
	int error;

	file = fopen (filename, "r");
	if (!file) return -1;
	error = ini_parse_file (file, config, user);
	fclose (file);
	return error;
}
