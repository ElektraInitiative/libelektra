/**
 * @file
 *
 * @brief A listener reacting to matches for the grammar rules defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <stack>

#include <kdb.hpp>

#include "YAMLBaseListener.h"

// -- Class --------------------------------------------------------------------

namespace yanlr
{

/**
 * @brief This class creates a key set by listening to matches of grammar rules
 *        specified via YAML.g4.
 */
class KeyListener : public YAMLBaseListener
{
	/** This variable stores a key set representing the textual input. */
	kdb::KeySet keys;

	/**
	 * This stack stores a key for each level of the current key name below
	 * parent.
	 */
	std::stack<kdb::Key> parents;

	/**
	 * This stack stores indices for the next array elements.
	 */
	std::stack<uintmax_t> indices;

public:
	/**
	 * @brief This constructor creates a new empty key storage using the given
	 *        parent key.
	 *
	 * @param parent This key specifies the parent of all keys stored in the
	 *               object.
	 */
	KeyListener (kdb::Key parent);

	/**
	 * @brief This function returns the data read by the parser.
	 *
	 * @return The key set representing the data from the textual input
	 */
	kdb::KeySet keySet ();

	/**
	 * @brief This function will be called when the listener enters an empty file (that might contain comments).
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	void enterEmpty (YAML::EmptyContext * context) override;

	/**
	 * @brief This function will be called after the parser exits a value.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	void exitValue (YAML::ValueContext * context) override;

	/**
	 * @brief This function will be called after the parser enters a key-value
	 *        pair.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void enterPair (YAML::PairContext * context) override;

	/**
	 * @brief This function will be called after the parser exits a key-value
	 *        pair.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitPair (YAML::PairContext * context) override;

	/**
	 * @brief This function will be called after the parser enters a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void enterSequence (YAML::SequenceContext * context) override;

	/**
	 * @brief This function will be called after the parser exits a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitSequence (YAML::SequenceContext * context) override;

	/**
	 * @brief This function will be called after the parser recognizes an element
	 *        of a sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void enterElement (YAML::ElementContext * context) override;

	/**
	 * @brief This function will be called after the parser read an element of a
	 *        sequence.
	 *
	 * @param context The context specifies data matched by the rule.
	 */
	virtual void exitElement (YAML::ElementContext * context) override;
};
}
