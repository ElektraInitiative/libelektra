/* inih -- simple .INI file parser

inih is released under the New BSD license (see LICENSE.txt). Go to the project
home page for more info:

http://code.google.com/p/inih/

*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "inih.h"

#if !INI_USE_STACK
#include <stdlib.h>
#endif

#define MAX_SECTION 512
#define MAX_NAME 512

/* Strip whitespace chars off end of given string, in place. Return s. */
static char* rstrip(char* s)
{
    char* p = s + strlen(s);
    while (p > s && isspace((unsigned char)(*--p)))
        *p = '\0';
    return s;
}

/* Return pointer to first non-whitespace char in given string. */
static char* lskip(const char* s)
{
    while (*s && isspace((unsigned char)(*s)))
        s++;
    return (char*)s;
}

/* Return pointer to first char c or ';' comment in given string, or pointer to
   null at end of string if neither found. ';' must be prefixed by a whitespace
   character to register as a comment. */
static char* find_char_or_comment(const char* s, char c)
{
    int was_whitespace = 0;
    while (*s && *s != c && !(was_whitespace && *s == ';')) {
        was_whitespace = isspace((unsigned char)(*s));
        s++;
    }
    return (char*)s;
}

/* Version of strncpy that ensures dest (size bytes) is null-terminated. */
static char* strncpy0(char* dest, const char* src, size_t size)
{
    strncpy(dest, src, size);
    dest[size - 1] = '\0';
    return dest;
}

/* See documentation in header file. */
int ini_parse_file(FILE* file,const struct IniConfig* config, void* user)
{
    /* Uses a fair bit of stack (use heap instead if you need to) */
#if INI_USE_STACK
    char line[INI_MAX_LINE];
#else
    char* line;
#endif
    char section[MAX_SECTION] = "";
    char prev_name[MAX_NAME] = "";

    char* start;
    char* end;
    char* name;
    char* value;
    int lineno = 0;
    int error = 0;
    int linecontinuation = 0;
#if !INI_USE_STACK
    line = (char*)malloc(INI_MAX_LINE);
    if (!line) {
        return -2;
    }
#endif

    /* Scan through file line by line */
    while (fgets(line, INI_MAX_LINE, file) != NULL) {
        lineno++;

        start = line;
#if INI_ALLOW_BOM
        if (lineno == 1 && (unsigned char)start[0] == 0xEF &&
                           (unsigned char)start[1] == 0xBB &&
                           (unsigned char)start[2] == 0xBF) {
            start += 3;
            config->bomHandler(user, 1);
        }
        else
        {
            config->bomHandler(user, 0);
        }
#endif
        if(*start == '\n')
        {
            if(!config->commentHandler(user, " ") && !error)
                error = lineno;
        }
        start = lskip(rstrip(start));

        if (*start == ';' || *start == '#') {
        	start += 1;
        	if (!config->commentHandler(user, start) && !error)
        		error = lineno;
            /* Per Python ConfigParser, allow '#' comments at start of line */
        }
        else if(config->supportMultiline && *prev_name && *start && !strncmp(line, config->continuationString, strlen(config->continuationString)))
        {
               /* Non-black line with leading whitespace, treat as continuation
               of previous name's value (as per Python ConfigParser). */
            start = line+strlen(config->continuationString);
            if(*start == '"')
                ++start;
            end =  line+(strlen(line)-1);
            while((*end != '"') && (!isprint(*end)))
    			--end;
	    	if((end > line) && (*end == '"'))
		   		*end = '\0';
            if (!config->keyHandler(user, section, prev_name, start, 1) && !error)
                error = lineno;
        }
		else if(*start == '=')
		{
			end = find_char_or_comment(start+1, '\0');
			value = lskip(start+1);
			rstrip(value);
            if (!config->keyHandler(user, NULL, NULL, value, 0) && !error)
                    error = lineno;
		}
        else if (*start == '[') {
            /* A "[section]" line */
            end = find_char_or_comment(start + 1, ']');
            if (*end == ']') {
                *end = '\0';
                strncpy0(section, start + 1, sizeof(section));
                *prev_name = '\0';
                if (!config->sectionHandler(user, section) && !error)
                	error = lineno;
            }
            else if (!error) {
                /* No ']' found on section line */
                error = lineno;
            }
        }
        else if (*start && *start != ';') {
            /* Not a comment, must be a name[=:]value pair */
            end = find_char_or_comment(start, '=');
            if (*end != '=') {
                end = find_char_or_comment(start, ':');
            }
            if (*end == '=' || *end == ':') {
                *end = '\0';
                name = rstrip(start);
                value = lskip(end + 1);
                end = find_char_or_comment(value, '\0');
                if (*end == ';')
                    *end = '\0';
                rstrip(value);
                if(*value == '"')
				{
    				while((*end != '"') && (!isprint(*end)))
	    				--end;
		    		if((end > value) && (*end == '"'))
			    		*end = '\0';
                    *(value++) = '\0';
				}
                /* Valid name[=:]value pair found, call handler */
                strncpy0(prev_name, name, sizeof(prev_name));
                if (!config->keyHandler(user, section, name, value, 0) && !error)
                    error = lineno;
            }
            else if (!error) {
				end = find_char_or_comment(start, '\0');
				name = rstrip(start);
				strncpy0(prev_name, name, sizeof(prev_name));
				if (!config->keyHandler(user, section, name, NULL, 0) && ! error)
					error = lineno;
            }
        }

#if INI_STOP_ON_FIRST_ERROR
        if (error)
            break;
#endif
    }

#if !INI_USE_STACK
    elektraFree (line);
#endif

    return error;
}

/* See documentation in header file. */
int ini_parse(const char* filename, const struct IniConfig* config, void* user)
{
    FILE* file;
    int error;

    file = fopen(filename, "r");
    if (!file)
        return -1;
    error = ini_parse_file(file, config, user);
    fclose(file);
    return error;
}
