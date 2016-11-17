/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "parser.hpp"

#include <fstream>
#include <iostream>

#ifndef _WIN32
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#endif

using namespace std;

ostream & operator<< (ostream & os, parse_t & p)
{
	os << "/*This is an auto-generated file generated by exporterrors. Do not modify it.*/" << endl
	   << endl
	   << "#ifndef KDBERRORS_H" << endl
	   << "#define KDBERRORS_H" << endl
	   << endl
	   << "#include <kdb.h>" << endl
	   << "#include <kdbhelper.h>" << endl
	   << "#include <kdblogger.h>" << endl
	   << "#include <string.h>" << endl
	   << endl
	   << "#ifndef STRINGIFY" << endl
	   << "#define STRINGIFY(x) STRINGIFY2 (x)" << endl
	   << "#define STRINGIFY2(x) #x" << endl
	   << "#endif" << endl
	   << endl
	   << "#define ELEKTRA_SET_ERROR(number, key, text) ELEKTRA_SET_ERROR_HELPER\\" << endl
	   << "	(number, key, text, __FILE__, __LINE__)" << endl
	   << endl
	   << "#define ELEKTRA_SET_ERROR_HELPER(number, key, text, file, line) ELEKTRA_SET_ERROR_HELPER_HELPER\\" << endl
	   << "	(number, key, text, file, line)" << endl
	   << endl
	   << "#define ELEKTRA_SET_ERROR_HELPER_HELPER(number, key, text, file, line) do {ELEKTRA_LOG (\"Add Error %d: %s\", "
	      "number, text); elektraSetError ## number\\"
	   << endl
	   << "	(key, text, file, #line); } while (0)" << endl
	   << endl
	   << endl
	   << "#define ELEKTRA_ADD_WARNING(number, key, text) ELEKTRA_ADD_WARNING_HELPER\\" << endl
	   << "	(number, key, text, __FILE__, __LINE__)" << endl
	   << "" << endl
	   << "#define ELEKTRA_ADD_WARNING_HELPER(number, key, text, file, line) ELEKTRA_ADD_WARNING_HELPER_HELPER\\" << endl
	   << "	(number, key, text, file, line)" << endl
	   << "" << endl
	   << "#define ELEKTRA_ADD_WARNING_HELPER_HELPER(number, key, text, file, line) do {ELEKTRA_LOG (\"Add Warning %d: %s\", "
	      "number, text);  elektraAddWarning ## number\\"
	   << endl
	   << "	(key, text, file, #line);} while (0)" << endl
	   << endl
	   << endl
	   << "#define ELEKTRA_SET_ERRORF(number, key, text, ...) ELEKTRA_SET_ERRORF_HELPER\\" << endl
	   << "	(number, key, text, __FILE__, __LINE__, __VA_ARGS__)" << endl
	   << endl
	   << "#define ELEKTRA_SET_ERRORF_HELPER(number, key, text, file, line, ...) ELEKTRA_SET_ERRORF_HELPER_HELPER\\" << endl
	   << "	(number, key, text, file, line, __VA_ARGS__)" << endl
	   << endl
	   << "#define ELEKTRA_SET_ERRORF_HELPER_HELPER(number, key, text, file, line, ...) do {ELEKTRA_LOG (\"Add Error \" "
	      "STRINGIFY(number) \" : \" text, __VA_ARGS__); elektraSetErrorf ## number\\"
	   << endl
	   << "	(key, text, file, #line,  __VA_ARGS__); } while (0)" << endl
	   << endl
	   << endl
	   << "#define ELEKTRA_ADD_WARNINGF(number, key, text, ...) ELEKTRA_ADD_WARNINGF_HELPER\\" << endl
	   << "	(number, key, text, __FILE__, __LINE__, __VA_ARGS__)" << endl
	   << "" << endl
	   << "#define ELEKTRA_ADD_WARNINGF_HELPER(number, key, text, file, line, ...) ELEKTRA_ADD_WARNINGF_HELPER_HELPER\\" << endl
	   << "	(number, key, text, file, line, __VA_ARGS__)" << endl
	   << "" << endl
	   << "#define ELEKTRA_ADD_WARNINGF_HELPER_HELPER(number, key, text, file, line, ...)  do {ELEKTRA_LOG (\"Add Warning \" "
	      "STRINGIFY(number) \" : \" text, __VA_ARGS__); elektraAddWarningf ## number\\"
	   << endl
	   << "	(key, text, file, #line, __VA_ARGS__); } while (0)" << endl
	   << endl
	   << endl;

	for (size_t i = 1; i < p.size (); ++i)
	{
		if (p[i]["unused"] == "yes")
		{
			continue;
		}

		if (p[i]["macro"].empty ())
		{
			continue;
		}

		os << "#define ELEKTRA_";
		if (p[i]["severity"] == "warning")
		{
			os << "WARNING_";
		}
		else
		{
			os << "ERROR_";
		}
		os << p[i]["macro"] << " " << i << endl;
	}

	os << endl << endl;

	for (size_t i = 1; i < p.size (); ++i)
	{
		if (p[i]["unused"] == "yes")
		{
			continue;
		}

		if (p[i]["severity"] == "warning")
		{
			for (int f = 0; f < 2; ++f)
			{
				if (f == 0)
				{
					os << "static inline void elektraAddWarningf" << i << "(Key *warningKey, const char *reason,"
					   << endl
					   << "	const char *file, const char *line, ...)  __attribute__ ((format (printf, 2, 5)));" << endl;
					os << "static inline void elektraAddWarningf" << i << "(Key *warningKey, const char *reason,"
					   << endl
					   << "	const char *file, const char *line, ...)" << endl;
				}
				else
				{
					os << "static inline void elektraAddWarning" << i << "(Key *warningKey, const char *reason," << endl
					   << "	const char *file, const char *line)" << endl;
				}
				os << "{" << endl
				   << "	if (!warningKey) return;" << endl
				   << "" << endl
				   << "	char buffer[] = \"warnings/#00\\0description\";" << endl
				   << "	const Key *meta = keyGetMeta(warningKey, \"warnings\");" << endl
				   << "	if (meta)" << endl
				   << "	{" << endl
				   << "		buffer[10] = keyString(meta)[0];" << endl
				   << "		buffer[11] = keyString(meta)[1];" << endl
				   << "		buffer[11]++;" << endl
				   << "		if (buffer[11] > '9')" << endl
				   << "		{" << endl
				   << "			buffer[11] = '0';" << endl
				   << "			buffer[10]++;" << endl
				   << "			if (buffer[10] > '9') buffer[10] = '0';" << endl
				   << "		}" << endl
				   << "		keySetMeta(warningKey, \"warnings\", &buffer[10]);" << endl
				   << "	} else  keySetMeta(warningKey, \"warnings\", \"00\");" << endl
				   << "" << endl
				   << "	keySetMeta(warningKey, buffer, \"number description ingroup module file line function reason\");"
				   << endl
				   << "	strcat(buffer, \"/number\" );" << endl
				   << "	keySetMeta(warningKey, buffer, \"" << i << "\");" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/description\");" << endl
				   << "	keySetMeta(warningKey, buffer, \"" << p[i]["description"] << "\");" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/ingroup\");" << endl
				   << "	keySetMeta(warningKey, buffer, \"" << p[i]["ingroup"] << "\");" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/module\");" << endl
				   << "	keySetMeta(warningKey, buffer, \"" << p[i]["module"] << "\");" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/file\");" << endl // should be called sourcefile
				   << "	keySetMeta(warningKey, buffer, file);" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/line\");" << endl
				   << "	keySetMeta(warningKey, buffer, line);" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/mountpoint\");" << endl
				   << "	keySetMeta(warningKey, buffer, keyName(warningKey));" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/configfile\");" << endl
				   << "	keySetMeta(warningKey, buffer, keyString(warningKey));" << endl
				   << "	buffer[12] = '\\0'; strcat(buffer, \"/reason\");" << endl;
				if (f == 0)
				{
					os << "	va_list arg;" << endl
					   << "	va_start(arg, line);" << endl
					   << "	char * r = elektraVFormat(reason, arg);" << endl
					   << "	keySetMeta(warningKey, buffer, r);" << endl
					   << "	elektraFree(r);" << endl
					   << "	va_end(arg);" << endl;
				}
				else
				{
					os << "	keySetMeta(warningKey, buffer, reason);" << endl;
				}
				os << "}" << endl << endl;
			}
		}
		else
		{
			for (int f = 0; f < 2; ++f)
			{
				if (f == 0)
				{
					os << "static inline void elektraSetErrorf" << i << "(Key *errorKey, const char *reason," << endl
					   << "	const char *file, const char *line, ...)  __attribute__ ((format (printf, 2, 5)));" << endl
					   << "static inline void elektraSetErrorf" << i << "(Key *errorKey, const char *reason," << endl
					   << "	const char *file, const char *line, ...)" << endl;
				}
				else
				{
					os << "static inline void elektraSetError" << i << "(Key *errorKey, const char *reason," << endl
					   << "	const char *file, const char *line)" << endl;
				}
				os << "{" << endl
				   << "	if (!errorKey) return;" << endl
				   << "	char buffer[] = \"warnings/#00\\0description\";" << endl
				   << " 	const Key *meta = keyGetMeta(errorKey, \"error\");" << endl
				   << "	if (meta)" << endl
				   << "	{" << endl
				   << "		const Key *warningMeta = keyGetMeta(errorKey, \"warnings\");" << endl
				   << "		if (warningMeta)" << endl
				   << "		{" << endl
				   << "			buffer[10] = keyString(warningMeta)[0];" << endl
				   << "			buffer[11] = keyString(warningMeta)[1];" << endl
				   << "			buffer[11]++;" << endl
				   << "			if (buffer[11] > '9')" << endl
				   << "			{" << endl
				   << "				buffer[11] = '0';" << endl
				   << "				buffer[10]++;" << endl
				   << "				if (buffer[10] > '9') buffer[10] = '0';" << endl
				   << "			}" << endl
				   << "			keySetMeta(errorKey, \"warnings\", &buffer[10]);" << endl
				   << "		} else	keySetMeta(errorKey, \"warnings\", \"00\");" << endl
				   << "		keySetMeta(errorKey, buffer, \"number description ingroup module file line function "
				      "reason\");"
				   << endl
				   << "		strcat(buffer, \"/number\" );" << endl
				   << "		keySetMeta(errorKey, buffer, \"" << i << "\");" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/description\");" << endl
				   << "		keySetMeta(errorKey, buffer, \"" << p[i]["description"] << "\");" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/ingroup\");" << endl
				   << "		keySetMeta(errorKey, buffer, \"" << p[i]["ingroup"] << "\");" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/module\");" << endl
				   << "		keySetMeta(errorKey, buffer, \"" << p[i]["module"] << "\");" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/file\");" << endl // should be called sourcefile
				   << "		keySetMeta(errorKey, buffer, file);" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/line\");" << endl
				   << "		keySetMeta(errorKey, buffer, line);" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/mountpoint\");" << endl
				   << "		keySetMeta(errorKey, buffer, keyName(errorKey));" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/configfile\");" << endl
				   << "		keySetMeta(errorKey, buffer, keyString(errorKey));" << endl
				   << "		buffer[12] = '\\0'; strcat(buffer, \"/reason\");" << endl
				   << "	}" << endl
				   << " 	else" << endl
				   << " 	{" << endl
				   << "		keySetMeta(errorKey, \"error\", \""
				   << "number description ingroup module file line function reason"
				   << "\");" << endl
				   << "		keySetMeta(errorKey, \"error/number\", \"" << i << "\");" << endl
				   << "		keySetMeta(errorKey, \"error/description\", \"" << p[i]["description"] << "\");" << endl
				   << "		keySetMeta(errorKey, \"error/ingroup\", \"" << p[i]["ingroup"] << "\");" << endl
				   << "		keySetMeta(errorKey, \"error/module\", \"" << p[i]["module"] << "\");" << endl
				   << "		keySetMeta(errorKey, \"error/file\", "
				   << "file"
				   << ");" << endl
				   << "		keySetMeta(errorKey, \"error/line\", "
				   << "line"
				   << ");" << endl
				   << "		keySetMeta(errorKey, \"error/mountpoint\", "
				   << "keyName(errorKey)"
				   << ");" << endl
				   << "		keySetMeta(errorKey, \"error/configfile\", "
				   << "keyString(errorKey)"
				   << ");" << endl
				   << " 	}" << endl;
				if (f == 0)
				{
					os << "	va_list arg;" << endl
					   << "	va_start(arg, line);" << endl
					   << "	char * r = elektraVFormat(reason, arg);" << endl
					   << " 	if(meta)" << endl
					   << "			keySetMeta(errorKey, buffer, r);" << endl
					   << " 	else" << endl
					   << "			keySetMeta(errorKey, \"error/reason\", "
					   << "r"
					   << ");" << endl
					   << "	elektraFree(r);" << endl
					   << "	va_end(arg);" << endl;
				}
				else
				{
					os << " 	if(meta)" << endl
					   << "			keySetMeta(errorKey, buffer, reason);" << endl
					   << " 	else" << endl
					   << "			keySetMeta(errorKey, \"error/reason\", reason);" << endl;
				}
				os << "}" << endl << endl;
			}
		}
	}

	os << "static inline KeySet *elektraErrorSpecification (void)" << endl
	   << "{" << endl
	   << "	return ksNew (30," << endl
	   << "		keyNew (\"system/elektra/modules/error/specification\"," << endl
	   << "			KEY_VALUE, \"the specification of all error codes\", KEY_END)," << endl;
	for (size_t i = 1; i < p.size (); ++i)
	{
		if (p[i]["unused"] == "yes")
		{
			continue;
		}

		os << "		keyNew (\"system/elektra/modules/error/specification/" << i << "\"," << endl
		   << "			KEY_END)," << endl
		   << "		keyNew (\"system/elektra/modules/error/specification/" << i << "/description\"," << endl
		   << "			KEY_VALUE, \"" << p[i]["description"] << "\", KEY_END)," << endl
		   << "		keyNew (\"system/elektra/modules/error/specification/" << i << "/ingroup\"," << endl
		   << "			KEY_VALUE, \"" << p[i]["ingroup"] << "\", KEY_END)," << endl
		   << "		keyNew (\"system/elektra/modules/error/specification/" << i << "/severity\"," << endl
		   << "			KEY_VALUE, \"" << p[i]["severity"] << "\", KEY_END)," << endl
		   << "		keyNew (\"system/elektra/modules/error/specification/" << i << "/module\"," << endl
		   << "			KEY_VALUE, \"" << p[i]["module"] << "\", KEY_END)," << endl;
	}
	os << "		KS_END);" << endl << "}" << endl;

	os << "static inline void elektraTriggerWarnings (int nr, Key *parentKey, const char *message)" << endl
	   << "{" << endl
	   << "	switch (nr)" << endl
	   << "	{" << endl;
	for (size_t i = 1; i < p.size (); ++i)
	{
		if (p[i]["unused"] == "yes")
		{
			continue;
		}

		if (p[i]["severity"] != "warning") continue;
		os << "		case " << i << ": ELEKTRA_ADD_WARNING (" << i << ", parentKey, message);" << endl
		   << "			break;" << endl;
	}
	os << "		default: ELEKTRA_ADD_WARNING (45, parentKey, \"in default branch\");" << endl
	   << "			 break;" << endl
	   << "	}" << endl
	   << "}" << endl
	   << "" << endl;
	os << "static inline void elektraTriggerError (int nr, Key *parentKey, const char *message)" << endl
	   << "{" << endl
	   << "	switch (nr)" << endl
	   << "	{" << endl;
	for (size_t i = 1; i < p.size (); ++i)
	{
		if (p[i]["unused"] == "yes")
		{
			continue;
		}

		if (p[i]["severity"] == "warning") continue;
		os << "		case " << i << ": ELEKTRA_SET_ERROR (" << i << ", parentKey, message);" << endl
		   << "			break;" << endl;
	}
	os << "		default: ELEKTRA_SET_ERROR (44, parentKey, \"in default branch\");" << endl
	   << "			 break;" << endl
	   << "	}" << endl
	   << "}" << endl;


	os << "#endif" << endl;
	return os;
}

int main (int argc, char ** argv) try
{
	if (argc == 1 || argc > 3)
	{
		cerr << "Usage " << argv[0] << " infile [outfile]" << endl;
		return 1;
	}

	string infile = argv[1];

	parse_t result = parse (infile);

	if (argc == 3)
	{
		std::string tmpfile = argv[2];
#ifndef _WIN32
		tmpfile += ".tmp";
		tmpfile += to_string (getpid ());
#endif
		{
			ofstream fout (tmpfile);
			if (!fout.is_open ())
			{
				cerr << "Could not open output file " << argv[2] << endl;
				return 1;
			}
			fout << result;
		}

#ifndef _WIN32
		int fd = open (tmpfile.c_str (), O_RDWR);
		if (fd == -1)
		{
			cerr << "Could not reopen file " << argv[2] << endl;
			return 2;
		}
		if (fsync (fd) == -1)
		{
			cerr << "Could not fsync config file " << argv[2] << " because ", strerror (errno);
			close (fd);
			return 3;
		}
		close (fd);

		if (rename (tmpfile.c_str (), argv[2]) == -1)
		{
			cerr << "Could not rename file " << tmpfile << " to " << argv[2] << endl;
			return 4;
		}
#endif
	}
	else
	{
		cout << result;
	}
}
catch (parse_error const & e)
{
	cerr << "The line " << e.linenr << " caused following parse error: " << e.info << endl;
	return 2;
}
