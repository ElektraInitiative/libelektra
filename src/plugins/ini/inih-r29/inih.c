/* inih -- simple .INI file parser

   inih is released under the New BSD license (see LICENSE.txt). Go to the project
   home page for more info:

http://code.google.com/p/inih/

*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <regex.h>
#include <string.h>
#include "inih.h"

#if !INI_USE_STACK
#include <stdlib.h>
#endif

#define MAX_SECTION 65535
#define MAX_NAME 65535

/* Strip whitespace chars off end of given string, in place. Return s. */
static char * rstrip (char * s)
{
	char * p = s + strlen (s);
	while (p > s && isspace ((unsigned char)(*--p)))
	{
		*p = '\0';
	}
	return s;
}

/* Return pointer to first non-whitespace char in given string. */
static char * lskip (const char * s)
{
	while (*s && isspace ((unsigned char)(*s)))
	{
		s++;
	}
	return (char *)s;
}

/* Return pointer to first char c or ';' comment in given string, or pointer to
   null at end of string if neither found. ';' must be prefixed by a whitespace
   character to register as a comment. */
static char * find_char_or_comment (const char * s, char c)
{
	int was_whitespace = 0;
	while (*s && *s != c && !(was_whitespace && *s == ';'))
	{
		was_whitespace = isspace ((unsigned char)(*s));
		s++;
	}
	return (char *)s;
}

/* Version of strncpy that ensures dest (size bytes) is null-terminated. */
static char * strncpy0 (char * dest, const char * src, size_t size)
{
	strncpy (dest, src, size);
	dest[size - 1] = '\0';
	return dest;
}

static char * strrstr (const char * haystack, const char * needle)
{
	char * prev = NULL;
	char * next = NULL;
	char * ptr = (char *)haystack;
	while ((next = strstr (ptr, needle)) != NULL)
	{
		prev = next;
		ptr = next + 1;
	}
	return prev;
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
	int lineno = 0;
	int error = 0;

	line = (char *)malloc (INI_MAX_LINE);

	if (!line)
	{
		return -2;
	}

	/* Scan through file line by line */
	while (fgets (line, INI_MAX_LINE, file) != NULL)
	{
		lineno++;

		start = line;
#if INI_ALLOW_BOM
		if (lineno == 1 && (unsigned char)start[0] == 0xEF && (unsigned char)start[1] == 0xBB && (unsigned char)start[2] == 0xBF)
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
			if (!config->commentHandler (user, " ") && !error) error = lineno;
			continue;
		}
		start = lskip (line);
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
							end = line + (strlen (line) - 1);
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
			start += 1;
			end = line + (strlen (line) - 1);
			if (*end == '\n') *end = '\0';
			if (!config->commentHandler (user, start) && !error) error = lineno;
		}
		else
		{
			// is a key

			char * ptr = start;
			unsigned int assign = 0;
			while (*ptr)
			{
				if (*ptr == '=' || *ptr == ':')
				{
					++assign;
				}
				++ptr;
			}

			if (assign == 1)
			{
				name = start;
				end = strchr (start, '=');
				if (!end) end = strchr (start, ':');
				if (*name == '"')
				{
					if (*(end - 2) == '"')
					{
						*(end - 2) = '\0';
						++name;
					}
					else if (*(end - 1) == '"')
					{
						*(end - 1) = '\0';
						++name;
					}
					else
					{
						++name;
						strncpy0 (prev_name, name, sizeof (prev_name));
						while (fgets (line, INI_MAX_LINE, file))
						{
							end = line + (strlen (line) - 1);
							while (end > line && *end != '"')
								--end;
							if (*end == '"')
							{
								*(end++) = '\0';
								strncpy0 (prev_name + strlen (prev_name), line,
									  sizeof (prev_name) - strlen (prev_name));
								break;
							}
							else
							{
								strncpy (prev_name + strlen (prev_name), line,
									 sizeof (prev_name) - strlen (prev_name));
							}
						}
						name = prev_name;
					}
				}
				if (*end != '=' && *end != ':')
				{
					ptr = lskip (end + 1);
					end = strchr (ptr, '=');
					if (!end) end = strchr (ptr, ':');
					if (*end == '=' || *end == ':') *end = '\0';
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
				if (*start == '"')
				{
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
						strncpy0 (prev_name, start, sizeof (prev_name));
						while (fgets (line, INI_MAX_LINE, file))
						{
							end = line + (strlen (line) - 1);
							while (end > line && *end != '"')
								--end;
							if (*end == '"')
							{
								*end = '\0';
								strncpy0 (prev_name + strlen (prev_name), line,
									  sizeof (prev_name) - strlen (prev_name));
								break;
							}
							else
							{
								strncpy (prev_name + strlen (prev_name), line,
									 sizeof (prev_name) - strlen (prev_name));
							}
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
					end = find_char_or_comment (start, '\0');
					name = rstrip (start);
					strncpy0 (prev_name, name, sizeof (prev_name));
					if (!config->keyHandler (user, section, name, NULL, 0) && !error) error = lineno;
				}
			}
			else
			{
				ptr = start + 1;
				while (*ptr)
				{
					if (*ptr == '=' || *ptr == ':')
					{
						if (*(ptr + 1) == '"' || *(ptr + 2) == '"' || *(ptr - 1) == '"' || *(ptr - 2) == '"') break;
					}
					++ptr;
				}
				if (*ptr)
				{
					end = strstr (ptr + 1, " = ");
					if (!end) end = strstr (ptr + 1, " : ");
					name = NULL;
					if (end)
					{
						// keyname == ":", "=", " : " or " = "
						if (*(ptr + 1) == '"')
						{
							*(ptr + 1) = '\0';
							end = (ptr + 2);
						}
						else if (*(ptr + 2) == '"')
						{
							*(ptr + 2) = '\0';
							end = (ptr + 3);
						}
						if (*(ptr - 1) == '"')
							*(ptr - 1) = '\0';
						else if (*(ptr - 2) == '"')
							*(ptr - 2) = '\0';
						name = ptr;
					}
					else if (*ptr == '=' || *ptr == ':')
					{
						*ptr = '\0';
						end = rstrip (start);
						if (*start == '"') ++start;
						if (*(ptr - 1) == '"')
							*(ptr - 1) = '\0';
						else if (*(ptr - 2) == '"')
							*(ptr - 2) = '\0';
						name = start;
					}
					else
					{
						if (!end) end = strrstr (start + 1, " = ");
						if (!end) end = strrstr (start + 1, " : ");
						*end = '\0';
						ptr = end + 2;
						end = rstrip (start);
						name = start;
					}
					value = ptr + 1;

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
				}
				else
				{
					rstrip (start);
					name = start;
					value = NULL;
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
