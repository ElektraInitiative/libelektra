/**
 * @file
 *
 * @brief This file contains a basic PEGTL YAML grammar.
 *
 * The grammar tries to stay as close as possible to the one of the
 * [specification](https://yaml.org/spec/1.2/spec.html).
 *
 * Since PEGTL’s grammar rules do not support nested function calls with custom
 * parameters directly, we use stacks stored in a custom state to emulate the
 * function based matching rules of the YAML spec. One of those stacks stores
 * the current indentation levels, while the other one stores the contexts (
 * e.g. `flow-in`, `flow-out`, etc.).
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAYPEG_GRAMMAR_HPP
#define ELEKTRA_PLUGIN_YAYPEG_GRAMMAR_HPP

// -- Macros -------------------------------------------------------------------

#define TAO_PEGTL_NAMESPACE yaypeg

// -- Imports ------------------------------------------------------------------

#include <functional>
#include <iostream>

#include <tao/pegtl.hpp>
#include <tao/pegtl/analyze.hpp>
#include <tao/pegtl/contrib/parse_tree.hpp>

#include <kdb.hpp>
#include <kdblogger.h>
#include <kdbtypes.h>

#include "state.hpp"

// -- Functions ----------------------------------------------------------------

namespace
{

/**
 * @brief This function returns the last matched character as UTF-32 code point.
 *
 * @pre The given input has to store at least one matched character:
 *      `(input.current() != input.begin())`.
 *
 * @note The function returns `'\0'` if either the last matched character was
 *       `'\0'` or if the input did not contain a valid UTF-8 sequence.
 *
 * @param input This variable stores the current state of the parser input.
 *
 * @return The last matched character as UTF-32 code point
 */
template <typename Input>
std::uint32_t lastMatchedUtf32 (Input & input)
{
	// We assume UTF-8 as encoding!
	auto last = input.current () - 1;
	std::uint32_t character = 0;

	// One byte: 0xxxxxxx
	if (static_cast<std::uint8_t> (*last) <= 0x7f)
	{
		character = *last;
	}
	// Two bytes: 110xxxxx  10xxxxxx
	else if (last - 1 != input.begin () && static_cast<std::uint8_t> (*(last - 1)) >> 5 == 0x6)
	{
		character = *last & 0x3F;
		character |= (*(last - 1) & 0x1f) << 6;
	} // Three bytes: 1110xxxx  10xxxxxx  10xxxxxx
	else if (last - 2 != input.begin () && static_cast<std::uint8_t> (*(last - 2)) >> 4 == 0xE)
	{
		character = *last & 0x3F;
		character |= (*(last - 1) & 0x3F) << 6;
		character |= (*(last - 2) & 0xF) << 12;
	} // Four bytes: 11110xxx  10xxxxxx  10xxxxxx  10xxxxxx
	else if (last - 3 != input.begin () && static_cast<std::uint8_t> (*(last - 3)) >> 3 == 0x1E)
	{
		character = *last & 0x3F;
		character |= (*(last - 1) & 0x3F) << 6;
		character |= (*(last - 2) & 0x3F) << 12;
		character |= (*(last - 3) & 0x7) << 18;
	}

	ELEKTRA_LOG_DEBUG ("Last code point (UTF-32): " ELEKTRA_UNSIGNED_LONG_F, static_cast<kdb_unsigned_long_t> (character));
	return character;
}

/**
 * @brief This function returns a Clang-like error message for a given error.
 *
 * @param input This parameter stores the input at the time an error occurred.
 * @param prefix This variable stores as prefix that this function prepends
 *               to every line of the visualized error message.
 *
 * @return A string representation of the error
 */
template <typename Input>
std::string visualizeError (Input const & input, std::string const & prefix)
{
	std::string::size_type start = 0;
	std::string::size_type end = 0;
	auto text = std::string{ input.begin (), input.end () };
	for (size_t currentLine = 1; currentLine <= input.position ().line; currentLine++)
	{
		size_t offset = (end == 0 ? 0 : 1);
		start = end + offset;
		end = text.find ("\n", end + offset);
	}

	std::string errorLine = text.substr (start, end - start);

	errorLine = prefix + errorLine + "\n" + prefix + std::string (input.position ().byte_in_line, ' ') + "^";

	return errorLine;
}

} // namespace

// -- Rules & Actions ----------------------------------------------------------

namespace yaypeg
{

using tao::TAO_PEGTL_NAMESPACE::any;
using tao::TAO_PEGTL_NAMESPACE::at;
using tao::TAO_PEGTL_NAMESPACE::blank;
using tao::TAO_PEGTL_NAMESPACE::bol;
using tao::TAO_PEGTL_NAMESPACE::digit;
using tao::TAO_PEGTL_NAMESPACE::eof;
using tao::TAO_PEGTL_NAMESPACE::eolf;
using tao::TAO_PEGTL_NAMESPACE::failure;
using tao::TAO_PEGTL_NAMESPACE::identifier;
using tao::TAO_PEGTL_NAMESPACE::if_must;
using tao::TAO_PEGTL_NAMESPACE::must;
using tao::TAO_PEGTL_NAMESPACE::not_at;
using tao::TAO_PEGTL_NAMESPACE::nothing;
using tao::TAO_PEGTL_NAMESPACE::opt;
using tao::TAO_PEGTL_NAMESPACE::plus;
using tao::TAO_PEGTL_NAMESPACE::rep;
using tao::TAO_PEGTL_NAMESPACE::seq;
using tao::TAO_PEGTL_NAMESPACE::sor;
using tao::TAO_PEGTL_NAMESPACE::star;
using tao::TAO_PEGTL_NAMESPACE::success;
using tao::TAO_PEGTL_NAMESPACE::until;
using tao::TAO_PEGTL_NAMESPACE::xdigit;
using tao::TAO_PEGTL_NAMESPACE::utf8::one;
using tao::TAO_PEGTL_NAMESPACE::utf8::range;
using tao::TAO_PEGTL_NAMESPACE::utf8::ranges;

// ==========================
// = Parser Context Updates =
// ==========================

/**
 * @brief The default action for matched rules does not execute any code.
 */
template <typename Rule>
struct action : nothing<Rule>
{
};

/**
 * @brief This grammar rule pushes the current indentation onto the stack.
 *
 * This piece of code takes a similar role of what the YAML spec describes as:
 *
 * > For some fixed auto-detected m > 0
 *
 * . Here `m` is a certain number of whitespace character. The YAML spec
 * contains rules such as
 *
 * > rule-name(n+m)
 *
 * where it uses the sum of the autodetected (additional) indentation + the
 * old indentation (of the parent of the current node). While we could detect
 * `m` in the rule `push_indent`, we decided that the rule should detect `n+m`
 * directly and and then put this value on the stack.
 */
struct push_indent
{
	using analyze_t = tao::TAO_PEGTL_NAMESPACE::analysis::generic<tao::TAO_PEGTL_NAMESPACE::analysis::rule_type::ANY>;

	template <tao::TAO_PEGTL_NAMESPACE::apply_mode, tao::TAO_PEGTL_NAMESPACE::rewind_mode, template <typename...> class,
		  template <typename...> class, typename Input, typename... States>
	static bool match (Input & input, State & state, States &&...)
	{
		long long indent = 0;
		while (!input.empty () && input.peek_char (indent) == ' ')
		{
			++indent;
		}
		state.indentation.push_back (indent);
		return true;
	}
};

/**
 * @brief This grammar rule increases the indentation by 1 and stores this value
 *        on the stack.
 */
struct push_indent_plus_one : success
{
};
template <>
struct action<push_indent_plus_one>
{
	template <typename Input>
	static void apply (const Input &, State & state)
	{
		state.indentation.push_back (state.indentation.back () + 1);
	}
};

/**
 * @brief This grammar rule stores adds the two top values on the stack and stores this sum at the top of the stack.
 */
struct push_indent_plus_last_two : success
{
};
template <>
struct action<push_indent_plus_last_two>
{
	template <typename Input>
	static void apply (const Input &, State & state)
	{
		auto levels = state.indentation.size ();
		state.indentation.push_back (state.indentation[levels - 1] + state.indentation[levels - 2]);
	}
};

/**
 * @brief This rule pops the top value of the indentation stack.
 */
struct pop_indent : success
{
};
template <>
struct action<pop_indent>
{
	template <typename Input>
	static void apply (const Input &, State & state)
	{
		state.indentation.pop_back ();
	}
};

/**
 * @brief This rule pushes the context provided as first template parameter onto
 *        the stack.
 */
template <State::Context Context>
struct push_context
{
	using analyze_t = tao::TAO_PEGTL_NAMESPACE::analysis::generic<tao::TAO_PEGTL_NAMESPACE::analysis::rule_type::ANY>;

	template <tao::TAO_PEGTL_NAMESPACE::apply_mode, tao::TAO_PEGTL_NAMESPACE::rewind_mode, template <typename...> class,
		  template <typename...> class, typename Input, typename... States>
	static bool match (Input &, State & state, States &&...)
	{
		state.context.push (Context);
		return true;
	}
};

/**
 * @brief This rule pops the top value of the context stack.
 */
struct pop_context : success
{
};
template <>
struct action<pop_context>
{
	template <typename Input>
	static void apply (const Input &, State & state)
	{
		state.context.pop ();
	}
};

/**
 * @brief This meta rule temporarily updates the state using the given rules.
 *
 * The rule uses `UpdateStateRule` to change the state, then applies all rules
 * stored in the template parameter pack `Rules` and undoes all state changes
 * using `RevertStateRule` afterwards. The rule `RevertStateRule` is called
 * regardless of the success or failure of `Rules`.
 */
template <typename UpdateStateRule, typename RevertStateRule, typename... Rules>
struct with_updated_state : seq<UpdateStateRule, sor<seq<Rules...>, seq<RevertStateRule, failure>>, RevertStateRule>
{
};

/**
 * @brief This rule parses `Rules` with the indentation detected by
 *        `push_indent`.
 */
template <typename... Rules>
struct with_updated_indent : with_updated_state<push_indent, pop_indent, Rules...>
{
};

/**
 * @brief This rule parses `Rules` with the last indentation increased by one.
 */
template <typename... Rules>
struct with_updated_indent_plus_one : with_updated_state<push_indent_plus_one, pop_indent, Rules...>
{
};

/**
 * @brief This rule parses `Rules` with a new indentation that is the sum of the last two indentation values.
 */
template <typename... Rules>
struct with_updated_indent_plus_last_two : with_updated_state<push_indent_plus_last_two, pop_indent, Rules...>
{
};

/**
 * @brief This rule parses `Rules` with the context updated to `Context`.
 */
template <State::Context Context, typename... Rules>
struct with_updated_context : with_updated_state<push_context<Context>, pop_context, Rules...>
{
};

// =========================
// = Parser Context Checks =
// =========================

/**
 * @brief This rule compares the current indentation and the indentation level
 *        before that using the given comparator.
 */
template <typename Comparator>
struct indent
{
	using analyze_t = tao::TAO_PEGTL_NAMESPACE::analysis::generic<tao::TAO_PEGTL_NAMESPACE::analysis::rule_type::ANY>;

	template <tao::TAO_PEGTL_NAMESPACE::apply_mode, tao::TAO_PEGTL_NAMESPACE::rewind_mode, template <typename...> class,
		  template <typename...> class, typename Input, typename... States>
	static bool match (Input &, State & state, States &&...)
	{
		auto levels = state.indentation.size ();
		return Comparator{}(state.indentation[levels - 1], state.indentation[levels - 2]);
	}
};

/**
 * @brief This rule succeeds if the current indentation level is smaller than
 *        the one before.
 */
struct less_indent : indent<std::less<long long>>
{
};
/**
 * @brief This rule succeeds if the current indentation level is greater than
 *        the one before.
 */
struct more_indent : indent<std::greater<long long>>
{
};

/**
 * @brief This rule changes the matching function based on the current context.
 *
 * The rule uses `RuleTrue` to match the current input, if the current context
 * is either `Context1` or `Context2`. If that is not the case, then the rule
 * matches the input with the matching function of `RuleFalse` instead.
 */
template <State::Context Context1, State::Context Context2, typename RuleTrue, typename RuleFalse>
struct if_context_else
{
	using analyze_t = tao::TAO_PEGTL_NAMESPACE::analysis::generic<tao::TAO_PEGTL_NAMESPACE::analysis::rule_type::ANY>;

	template <tao::TAO_PEGTL_NAMESPACE::apply_mode ApplyMode, tao::TAO_PEGTL_NAMESPACE::rewind_mode RewindMode,
		  template <typename...> class Action, template <typename...> class Control, typename Input, typename... States>
	static bool match (Input & input, State & state, States &&... states)
	{
		if (state.context.top () == Context1 || state.context.top () == Context2)
		{
			return RuleTrue::template match<ApplyMode, RewindMode, Action, Control> (input, state, states...);
		}
		return RuleFalse::template match<ApplyMode, RewindMode, Action, Control> (input, state, states...);
		;
	}
};

// ===========
// = Grammar =
// ===========

// ======================
// = 5.1. Character Set =
// ======================

// [1]
struct c_printable : sor<one<'\t', '\n', '\r', 0x85>, ranges<' ', 0x7E, 0xA0, 0xD7FF, 0xE000, 0xFFFD, 0x10000, 0x10FFFF>>
{
};
// [2]
struct nb_json : sor<one<0x9>, range<0x20, 0x10FFFF>>
{
};

// ============================
// = 5.2. Character Encodings =
// ============================

// [3]
struct c_byte_order_mark : one<0xFEFF>
{
};

// =============================
// = 5.3. Indicator Characters =
// =============================

// [12]
struct c_comment : one<'#'>
{
};

// [22]
struct c_indicator : one<'-', '?', ':', ',', '[', ']', '{', '}', '#', '&', '*', '!', ',', '>', '\'', '"', '%', '@', '`'>
{
};
// [23]
struct c_flow_indicator : one<',', '[', ']', '{', '}'>
{
};

// ==============================
// = 5.4. Line Break Characters =
// ==============================

// [24]
struct b_line_feed : one<'\n'>
{
};
// [25]
struct b_carriage_return : one<'\r'>
{
};
// [26]
struct b_char : sor<b_line_feed, b_carriage_return>
{
};
// [27]
struct nb_char : seq<not_at<sor<b_char, c_byte_order_mark>>, c_printable>
{
};
// [28]
struct b_break : sor<seq<b_carriage_return, b_line_feed>, b_carriage_return, b_line_feed>
{
};

// [29]
struct b_as_line_feed : b_break
{
};
// [30]
struct b_non_content : b_break
{
};

// ===============================
// = 5.5. White Space Characters =
// ===============================

// [31]
struct s_space : one<' '>
{
};
// [32]
struct s_tab : one<'\t'>
{
};
// [33]
struct s_white : sor<s_space, s_tab>
{
};
// [34]
struct ns_char : seq<not_at<s_white>, nb_char>
{
};

// =================================
// = 5.6. Miscellaneous Characters =
// =================================

// [35]
struct ns_dec_digit : digit
{
};
// [36]
struct ns_hex_digit : xdigit
{
};

// ===========================
// = 5.7. Escaped Characters =
// ===========================

// [42]
struct ns_esc_null : one<'0'>
{
};
// [43]
struct ns_esc_bell : one<'a'>
{
};
// [44]
struct ns_esc_backspace : one<'b'>
{
};
// [45]
struct ns_esc_horizontal_tab : one<'t', 0x9>
{
};
// [46]
struct ns_esc_line_feed : one<'n'>
{
};
// [47]
struct ns_esc_vertical_tab : one<'v'>
{
};
// [48]
struct ns_esc_form_feed : one<'f'>
{
};
// [49]
struct ns_esc_carriage_return : one<'r'>
{
};
// [50]
struct ns_esc_escape : one<'e'>
{
};
// [51]
struct ns_esc_space : one<0x20>
{
};
// [52]
struct ns_esc_double_quote : one<'"'>
{
};
// [53]
struct ns_esc_slash : one<'/'>
{
};
// [54]
struct ns_esc_backslash : one<'\\'>
{
};
// [55]
struct ns_esc_next_line : one<'N'>
{
};
// [56]
struct ns_esc_non_breaking_space : one<'_'>
{
};
// [57]
struct ns_esc_line_separator : one<'L'>
{
};
// [58]
struct ns_esc_paragraph_separator : one<'P'>
{
};
// [59]
struct ns_esc_8_bit : seq<one<'x'>, rep<2, ns_hex_digit>>
{
};
// [60]
struct ns_esc_16_bit : seq<one<'u'>, rep<4, ns_hex_digit>>
{
};
// [61]
struct ns_esc_32_bit : seq<one<'U'>, rep<8, ns_hex_digit>>
{
};

// [62] (Modified)
struct escaped_choices
: sor<ns_esc_null, ns_esc_bell, ns_esc_backspace, ns_esc_horizontal_tab, ns_esc_line_feed, ns_esc_vertical_tab, ns_esc_form_feed,
      ns_esc_carriage_return, ns_esc_escape, ns_esc_space, ns_esc_double_quote, ns_esc_slash, ns_esc_backslash, ns_esc_next_line,
      ns_esc_non_breaking_space, ns_esc_line_separator, ns_esc_paragraph_separator, ns_esc_8_bit, ns_esc_16_bit, ns_esc_32_bit>
{
};

struct c_ns_esc_char : seq<if_must<one<'\\'>, escaped_choices>>
{
};

// ===========================
// = 6.1. Indentation Spaces =
// ===========================

// [63]
struct s_indent
{
	using analyze_t = tao::TAO_PEGTL_NAMESPACE::analysis::generic<tao::TAO_PEGTL_NAMESPACE::analysis::rule_type::ANY>;

	template <tao::TAO_PEGTL_NAMESPACE::apply_mode, tao::TAO_PEGTL_NAMESPACE::rewind_mode, template <typename...> class,
		  template <typename...> class, typename Input, typename... States>
	static bool match (Input & input, State & state, States &&...)
	{
		auto indent = state.indentation.back ();
		decltype (indent) spaces = 0;
		while (!input.empty () && input.peek_char (spaces) == ' ' && spaces < indent)
		{
			spaces++;
		}
		if (spaces < indent)
		{
			return false;
		}
		input.bump (state.indentation.back ());
		return true;
	}
};

// [64]
struct s_indent_smaller_n : with_updated_indent<less_indent, s_indent>
{
};

// ==========================
// = 6.2. Separation Spaces =
// ==========================

// [66]
struct s_separate_in_line : sor<plus<s_white>, bol>
{
};

// ======================
// = 6.3. Line Prefixes =
// ======================

// [67]
struct s_block_line_prefix;
struct s_flow_line_prefix;
struct s_line_prefix : if_context_else<State::Context::BLOCK_OUT, State::Context::BLOCK_IN, s_block_line_prefix, s_flow_line_prefix>
{
};
// [68]
struct s_block_line_prefix : s_indent
{
};
// [69]
struct s_flow_line_prefix : seq<s_indent, opt<s_separate_in_line>>
{
};

// ====================
// = 6.4. Empty Lines =
// ====================

// [70]
struct l_empty : seq<sor<s_line_prefix, s_indent_smaller_n>, b_as_line_feed>
{
};

// =====================
// = 6.5. Line Folding =
// =====================

// [71]
struct b_l_trimmed : seq<b_non_content, plus<l_empty>>
{
};
// [72]
struct b_as_space : b_break
{
};
// [73]
struct b_l_folded : sor<b_l_trimmed, b_as_space>
{
};
// [74]
struct s_flow_folded : seq<opt<s_separate_in_line>, with_updated_context<State::Context::FLOW_IN, b_l_folded>, s_flow_line_prefix>
{
};

// ========================
// = 6.7 Separation Lines =
// ========================

// [80]
struct s_separate_lines;
struct s_separate : if_context_else<State::Context::BLOCK_KEY, State::Context::FLOW_KEY, s_separate_in_line, s_separate_lines>
{
};
// [81]
struct s_l_comments;
struct s_separate_lines : sor<seq<s_l_comments, s_flow_line_prefix>, s_separate_in_line>
{
};

// =================
// = 6.6. Comments =
// =================

// [75]
struct c_nb_comment_text : seq<one<'#'>, star<nb_char>>
{
};
// [76]
struct b_comment : sor<b_non_content, eof>
{
};
// [77]
struct s_b_comment : seq<opt<s_separate_in_line, opt<c_nb_comment_text>>, b_comment>
{
};
// [78]
struct l_comment : seq<s_separate_in_line, opt<c_nb_comment_text>, sor<b_non_content, seq<eof, failure>>>
{
};
// [79]
struct s_l_comments : seq<sor<s_b_comment, bol>, star<l_comment>>
{
};

// ====================
// = 7.2. Empty Nodes =
// ====================

// [105]
struct e_scalar : success
{
};
// [106]
struct e_node : e_scalar
{
};

// ===========================
// = 7.3. Flow Scalar Styles =
// ===========================

// ==============================
// = 7.3.1. Double-Quoted Style =
// ==============================

// [107]
struct nb_double_char : sor<c_ns_esc_char, seq<not_at<one<'\\', '"'>>, nb_json>>
{
};
// [108]
struct ns_double_char : seq<not_at<s_white>, nb_double_char>
{
};
// [109] (Modified)
struct nb_double_text;
struct closing_double_quote : one<'"'>
{
};
struct c_double_quoted : seq<one<'"'>, nb_double_text, must<closing_double_quote>>
{
};
// [110]
struct nb_double_multi_line;
struct nb_double_one_line;
struct nb_double_text : if_context_else<State::Context::FLOW_OUT, State::Context::FLOW_IN, nb_double_multi_line, nb_double_one_line>
{
};
// [111]
struct nb_double_one_line : star<nb_double_char>
{
};
// [112]
struct s_double_escaped
: seq<star<s_white>, one<'\\'>, b_non_content, with_updated_context<State::Context::FLOW_IN, star<l_empty>>, s_flow_line_prefix>
{
};
// [113]
struct s_double_break : sor<s_double_escaped, s_flow_folded>
{
};
// [114]
struct nb_ns_double_in_line : star<star<s_white>, ns_double_char>
{
};
// [115]
struct s_double_next_line : seq<s_double_break, opt<ns_double_char, nb_ns_double_in_line, sor<s_double_next_line, star<s_white>>>>
{
};
// [116]
struct nb_double_multi_line : seq<nb_ns_double_in_line, sor<s_double_next_line, star<s_white>>>
{
};

// ==============================
// = 7.3.2. Single-Quoted Style =
// ==============================

// [117]
struct c_quoted_quote : rep<2, one<'\''>>
{
};
// [118]
struct nb_single_char : sor<c_quoted_quote, seq<not_at<one<'\''>>, nb_json>>
{
};
// [119]
struct ns_single_char : seq<not_at<s_white>, nb_single_char>
{
};
// [120] (Modified)
struct nb_single_text;
struct closing_single_quote : one<'\''>
{
};
struct c_single_quoted : seq<one<'\''>, nb_single_text, must<closing_single_quote>>
{
};
// [121]
struct nb_single_multi_line;
struct nb_single_one_line;
struct nb_single_text : if_context_else<State::Context::FLOW_OUT, State::Context::FLOW_IN, nb_single_multi_line, nb_single_one_line>
{
};
// [122]
struct nb_single_one_line : star<nb_single_char>
{
};
// [123]
struct nb_ns_single_in_line : star<star<s_white>, ns_single_char>
{
};
// [124]
struct s_single_next_line : seq<s_flow_folded, opt<ns_single_char, nb_ns_single_in_line, sor<s_single_next_line, star<s_white>>>>
{
};
// [125]
struct nb_single_multi_line : seq<nb_ns_single_in_line, sor<s_single_next_line, star<s_white>>>
{
};

// ======================
// = 7.3.3. Plain Style =
// ======================

// [126]
struct ns_plain_safe;
struct ns_plain_first : sor<seq<not_at<c_indicator>, ns_char>, seq<one<'?', ':', '-'>, at<ns_plain_safe>>>
{
};

// [127]
struct ns_plain_safe_out;
struct ns_plain_safe_in;
struct ns_plain_safe : if_context_else<State::Context::FLOW_OUT, State::Context::BLOCK_KEY, ns_plain_safe_out, ns_plain_safe_in>
{
};
// [128]
struct ns_plain_safe_out : ns_char
{
};
// [129]
struct ns_plain_safe_in : seq<not_at<c_flow_indicator>, ns_char>
{
};
// [130]
struct ns_char_preceding
{
	using analyze_t = tao::TAO_PEGTL_NAMESPACE::analysis::generic<tao::TAO_PEGTL_NAMESPACE::analysis::rule_type::ANY>;

	template <tao::TAO_PEGTL_NAMESPACE::apply_mode, tao::TAO_PEGTL_NAMESPACE::rewind_mode, template <typename...> class,
		  template <typename...> class, typename Input, typename... States>
	static bool match (Input & input, State &, States &&...)
	{
		if (input.current () == input.begin ())
		{
			return true;
		}
		auto last = lastMatchedUtf32 (input);

		if (last == '\n' || last == 0xFEFF || last == ' ' || last == '\t')
		{
			return false;
		}
		return last == 0x85 || (last > ' ' && last <= 0x7E) || (last >= 0xA0 && last <= 0xD7FF) ||
		       (last >= 0xE000 && last <= 0xFFFD) || (last >= 0x10000 && last <= 0x10FFFF);
	}
};
struct ns_plain_char : sor<seq<not_at<one<':', '#'>>, ns_plain_safe>, seq<ns_char_preceding, one<'#'>>, seq<one<':'>, at<ns_plain_safe>>>
{
};
// [131]
struct ns_plain_multi_line;
struct ns_plain_one_line;
struct ns_plain : if_context_else<State::Context::FLOW_OUT, State::Context::FLOW_IN, ns_plain_multi_line, ns_plain_one_line>
{
};
// [132]
struct nb_ns_plain_in_line : star<seq<star<s_white>>, ns_plain_char>
{
};
// [133]
struct ns_plain_one_line : seq<ns_plain_first, nb_ns_plain_in_line>
{
};
// [134]
struct s_ns_plain_next_line : seq<s_flow_folded, ns_plain_char, nb_ns_plain_in_line>
{
};
// [135]
struct ns_plain_multi_line : seq<ns_plain_one_line, star<s_ns_plain_next_line>>
{
};

// ========================
// = 7.4.2. Flow Mappings =
// ========================

// [154]
struct ns_flow_yaml_node;
struct ns_s_implicit_yaml_key : seq<ns_flow_yaml_node, opt<s_separate_in_line>>
{
};
template <>
struct action<ns_s_implicit_yaml_key>
{
	template <typename Input>
	static bool apply (const Input & input, State &)
	{
		return input.string ().size () <= 1024;
	}
};
// [155]
struct c_flow_json_node;
struct c_s_implicit_json_key : seq<c_flow_json_node, opt<s_separate_in_line>>
{
};
template <>
struct action<c_s_implicit_json_key>
{
	template <typename Input>
	static bool apply (const Input & input, State &)
	{
		return input.string ().size () <= 1024;
	}
};

// ===================
// = 7.5. Flow Nodes =
// ===================

// [156]
struct ns_flow_yaml_content : ns_plain
{
};
// [157] (Incomplete)
struct c_flow_json_content : sor<c_single_quoted, c_double_quoted>
{
};
// [158]
struct ns_flow_content : sor<ns_flow_yaml_content, c_flow_json_content>
{
};
// [159] (Incomplete)
struct ns_flow_yaml_node : ns_flow_yaml_content
{
};
// [160] (Incomplete)
struct c_flow_json_node : c_flow_json_content
{
};
// [161] (Incomplete)
struct ns_flow_node : ns_flow_content
{
};

// ================================
// = 8.2. Block Collection Styles =
// ================================

// ==========================
// = 8.2.1. Block Sequences =
// ==========================

// [183]
struct c_l_block_seq_entry;
struct l_plus_block_sequence : with_updated_indent<more_indent, plus<s_indent, c_l_block_seq_entry>>
{
};
// [184]
struct s_l_plus_block_indented;
struct c_l_block_seq_entry : seq<one<'-'>, not_at<ns_char>, with_updated_context<State::Context::BLOCK_IN, s_l_plus_block_indented>>
{
};
// [185]
struct s_l_plus_block_node;
struct ns_l_compact_sequence;
struct ns_l_compact_mapping;
struct s_l_plus_block_indented
: sor<seq<with_updated_indent<
	      s_indent, with_updated_indent_plus_last_two<with_updated_indent_plus_one<sor<ns_l_compact_sequence, ns_l_compact_mapping>>>>>,
      s_l_plus_block_node, seq<e_node, s_l_comments>>
{
};
// [186]
struct ns_l_compact_sequence : seq<c_l_block_seq_entry, star<s_indent, c_l_block_seq_entry>>
{
};

// =========================
// = 8.2.2. Block Mappings =
// =========================

// [187]
struct ns_l_block_map_entry;
struct l_plus_block_mapping : with_updated_indent<more_indent, plus<s_indent, ns_l_block_map_entry>>
{
};

// [188] (Incomplete)
struct ns_l_block_map_implicit_entry;
struct ns_l_block_map_entry : sor<ns_l_block_map_implicit_entry>
{
};
// [192]
struct ns_s_block_map_implicit_key;
struct c_l_block_map_implicit_value;
struct ns_l_block_map_implicit_entry : seq<sor<ns_s_block_map_implicit_key, e_node>, c_l_block_map_implicit_value>
{
};
// [193]
struct ns_s_block_map_implicit_key : with_updated_context<State::Context::BLOCK_KEY, sor<c_s_implicit_json_key, ns_s_implicit_yaml_key>>
{
};
// [194]
struct c_l_block_map_implicit_value
: seq<one<':'>, sor<with_updated_context<State::Context::BLOCK_OUT, s_l_plus_block_node>, seq<e_node, s_l_comments>>>
{
};
// [195]
struct ns_l_compact_mapping : seq<ns_l_block_map_entry, star<s_indent, ns_l_block_map_entry>>
{
};

// ======================
// = 8.2.3. Block Nodes =
// ======================

// [196]
struct s_l_plus_block_in_block;
struct s_l_plus_flow_in_block;
struct s_l_plus_block_node : sor<s_l_plus_block_in_block, s_l_plus_flow_in_block>
{
};
// [197]
struct s_l_plus_flow_in_block
: seq<with_updated_indent_plus_one<with_updated_context<State::Context::FLOW_OUT, seq<s_separate, ns_flow_node>>>, s_l_comments>
{
};

// [198] (Incomplete)
struct s_l_plus_block_collection;
struct s_l_plus_block_in_block : sor<s_l_plus_block_collection>
{
};
// [200] (Incomplete)
template <typename... Rules>
struct seq_spaces;
struct s_l_plus_block_collection : seq<s_l_comments, sor<seq_spaces<l_plus_block_sequence>, l_plus_block_mapping>>
{
};

// [201]
struct push_indent_sequence : success
{
};
template <>
struct action<push_indent_sequence>
{
	template <typename Input>
	static void apply (const Input &, State & state)
	{
		auto indent = state.indentation.back ();
		state.indentation.push_back ((state.context.top () == State::Context::BLOCK_OUT) ? indent - 1 : indent);
	}
};
template <typename... Rules>
struct seq_spaces : with_updated_state<push_indent_sequence, pop_indent, Rules...>
{
};

// ==================
// = 9.1. Documents =
// ==================

// ==========================
// = 9.1.1. Document Prefix =
// ==========================

// [202]
struct l_document_prefix : seq<opt<c_byte_order_mark>, star<l_comment>>
{
};

// ===========================
// = 9.1.2. Document Markers =
// ===========================

// [203]
struct c_directives_end : rep<3, one<'-'>>
{
};
// [204]
struct c_document_end : rep<3, one<'.'>>
{
};
// [205]
struct l_document_suffix : seq<c_document_end, s_l_comments>
{
};
// [206]
struct c_forbidden : seq<bol, sor<c_directives_end, c_document_end>, sor<b_char, s_white, eof>>
{
};

// =========================
// = 9.1.3. Bare Documents =
// =========================

// [207]
struct l_bare_document : with_updated_context<State::Context::BLOCK_IN, not_at<until<c_forbidden, any>>, s_l_plus_block_node>
{
};

// ================
// = 9.2. Streams =
// ================

// [210] (Incomplete)
struct l_any_document : sor<l_bare_document>
{
};

// [211] (Incomplete, Modified)
struct l_yaml_stream : seq<opt<l_document_prefix>, opt<l_any_document>>
{
};

struct yaml : if_must<l_yaml_stream, eof>
{
};

// -- Debug Actions ------------------------------------------------------------

template <>
struct action<c_flow_json_node>
{
	template <typename Input>
	static void apply (const Input & input __attribute__ ((unused)), State &)
	{
		ELEKTRA_LOG_DEBUG ("`c_flow_json_node`: “%s”", input.string ().c_str ());
	}
};

template <>
struct action<ns_flow_node>
{
	template <typename Input>
	static void apply (const Input & input __attribute__ ((unused)), State &)
	{
		ELEKTRA_LOG_DEBUG ("`ns_flow_node`: “%s”", input.string ().c_str ());
	}
};

// -- Error Handling -----------------------------------------------------------

/**
 * @brief This templated control struct specifies an error messages for a `must` grammar rule with name `Rule`.
 */
template <typename Rule>
/* For detailed debugging information, please use the control class `tracer` instead of `normal`. */
struct errors : public tao::TAO_PEGTL_NAMESPACE::normal<Rule>
{
	static char const * const errorMessage;

	/**
	 * @brief The parser calls this method if the grammar rule `must<Rule>` failed.
	 *
	 * @param input This variable stores the state of the parser input, at the time parsing failed.
	 */
	template <typename Input, typename... States>
	static void raise (const Input & input, States &&...)
	{
		tao::TAO_PEGTL_NAMESPACE::position pos = input.position ();
		std::string location = pos.source + ":" + std::to_string (pos.line) + ":" + std::to_string (pos.byte_in_line) + ": ";

		std::string message = "\n" + location + errorMessage;
		message += "\n" + visualizeError (input, std::string (location.length (), ' '));

		throw std::runtime_error (message);
	}
};

/* Define an error message for the only `must` grammar rule: `if_must<l_yaml_stream, eof>` */
template <>
char const * const errors<tao::TAO_PEGTL_NAMESPACE::eof>::errorMessage = "Incomplete document, expected “end of file”";
template <>
char const * const errors<escaped_choices>::errorMessage = "Unexpected escape character";
template <>
char const * const errors<closing_double_quote>::errorMessage = "Missing closing double quote or incorrect value inside flow scalar";
template <>
char const * const errors<closing_single_quote>::errorMessage = "Missing closing single quote or incorrect value inside flow scalar";

// -- Parse Tree Selector ------------------------------------------------------

/**
 * @brief This selector removes all nodes for grammar rules from the parse tree
 *        except for the ones for the grammar rules specified below.
 */
// clang-format off
template <typename Rule>
using selector = tao::TAO_PEGTL_NAMESPACE::parse_tree::selector<
	Rule, tao::TAO_PEGTL_NAMESPACE::parse_tree::@STORE_CONTENT@<c_flow_json_node, ns_flow_yaml_node, ns_flow_node, e_node>,
	tao::TAO_PEGTL_NAMESPACE::parse_tree::@REMOVE_CONTENT@<ns_l_block_map_implicit_entry, ns_s_block_map_implicit_key,
								c_l_block_map_implicit_value, l_plus_block_sequence, c_l_block_seq_entry>>;
// clang-format on
} // namespace yaypeg

#endif // ELEKTRA_PLUGIN_YAYPEG_GRAMMAR_HPP
