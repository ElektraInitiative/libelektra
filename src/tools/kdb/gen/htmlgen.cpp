/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "htmlgen.hpp"

kainjow::mustache::data HtmlGenTemplate::getTemplateData ()
{
	using namespace kainjow::mustache;

	/*
	[/our_editor/quit]
	type=string
	default="CTRL+Q"
	fallback/#0=/kde/kate/ActionProp/Default/file_quit
	fallback/#1=/vim/map/:qa<CR>
	fallback/#2=/emacs/keyboard-escape-quit

	[/myapp/shortcut/quit_myapp]
	type=string
	default="CTRL+Q"
	override/#0=/kate/quit
	override/#0/transform/cpp=std::transform(value.begin(), value.end(),
			value.begin(), ::toupper);
			return value
	 */
	return list{
		object{
			{ "name", "/our_editor/quit" },
			{ "type", "string" },
			{ "default", "CTRL+0" },
			{ "has_fallback", true },
			{
				"fallback",
				list{ "/kde/kate/ActionProp/Default/file_quit", "/vim/map/:qa<CR>", "/emacs/keyboard-escape-quit" },
			},
		},
		object{
			{ "name", "/myapp/shortcut/quit_myapp" },
			{ "type", "string" },
			{ "default", "CTRL+0" },
			{ "has_override", true },
			{
				"override",
				list{ "/kate/quit" },
			},
		},
	};
}