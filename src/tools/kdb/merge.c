/**
 * @file
 *
 * @brief Implementation of kdb cmerge command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <merge.h>

#include <kdb.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#include <cpp-main.h>

#define COMMAND_NAME "merge"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMergeSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Three-way merge of Key sets.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/ourpath", KEY_META, "description", "Path to the keyset to serve as our.",
			     KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/theirpath", KEY_META, "description", "Path to the keyset to serve as their.",
			     KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/basepath", KEY_META, "description", "Path to the base keyset.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/resultpath", KEY_META, "description",
				   "Path without keys where the merged keyset will be saved.", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "3", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/strategy", KEY_META, "description",
			     "strategy  to use in case of a conflict. Options: ours,theirs,abort(default)", KEY_META, "opt", "s", KEY_META,
			     "opt/arg/help", "STRATEGY", KEY_META, "opt/long", "strategy", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Overwrite existing keys in  result.",
				   KEY_META, "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));


	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppMerge (int argc, char** argv)
{ // for `merge` use `cmerge` in cpp

	char * backup = argv[1];
	argv[1] = strdup ("cmerge");

	int ret = cpp_main (argc, argv);

	argv[1] = backup;

	return ret;
}
