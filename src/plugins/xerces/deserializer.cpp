/**
 * @file
 *
 * @brief deserialization implementation for xerces plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "deserializer.hpp"
#include "util.hpp"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <algorithm>
#include <iostream>
#include <locale>
#include <map>

#include <elektra/kdbease.h>
#include <kdblogger.h>
#include <key.hpp>

XERCES_CPP_NAMESPACE_USE
using namespace std;
using namespace kdb;
using namespace xerces;

namespace
{

XercesPtr<DOMDocument> doc2dom (std::string const & src)
{
	XercesDOMParser parser;
	parser.setValidationScheme (XercesDOMParser::Val_Auto);

	parser.parse (asXMLCh (src));
	return XercesPtr<DOMDocument> (parser.adoptDocument ());
}

string trim (string const & str)
{
	stringstream ss (str);
	string to;
	string trimmed;

	while (getline (ss, to, '\n'))
	{
		// Remove whitespace lines, most likely caused by pretty printing
		if (!all_of (to.begin (), to.end (), [] (char c) { return isspace (c, locale ()); })) trimmed += to;
	}

	return trimmed;
}

string getElementText (DOMNode const * parent)
{
	string str;

	for (auto child = parent->getFirstChild (); child != NULL; child = child->getNextSibling ())
	{
		if (DOMNode::NodeType::TEXT_NODE == child->getNodeType () || DOMNode::NodeType::CDATA_SECTION_NODE == child->getNodeType ())
		{
			DOMText * data = dynamic_cast<DOMText *> (child);
			if (!data->getIsElementContentWhitespace ()) str += toStr (data->getData ());
		}
	}

	// Trim whitespace that is most likely due to pretty printing
	return trim (str);
}

Key newNodeKey (Key const & parent, DOMNode const * node)
{
	Key childKey (parent.getName (), KEY_END);
	const string keyName = toStr (node->getNodeName ());
	childKey.addBaseName (keyName);
	return childKey;
}

void node2key (DOMNode const * n, Key const & parent, KeySet const & ks, Key & current)
{
	const string keyName = toStr (n->getNodeName ());
	ELEKTRA_LOG_DEBUG ("Encountered Element: %s with parent %s", keyName.c_str (), current.getName ().c_str ());

	if (!ks.size ())
	{ // we map the parent key to the xml root element
		// preserve the original name if it is different
		auto parentName = parent.rbegin ();
		if (parentName != parent.rend () && (*parentName) != keyName)
		{
			ELEKTRA_LOG_DEBUG ("parent name %s differs from root element name %s", (*parentName).c_str (), keyName.c_str ());
			current.setMeta (ELEKTRA_XERCES_ORIGINAL_ROOT_NAME, keyName);
		}
	}
	else
		current.addBaseName (keyName);

	const string text = getElementText (n);
	current.set<string> (text);

	if (!current.isValid ()) throw XercesPluginException ("Given keyset contains invalid keys to serialize");

	ELEKTRA_LOG_DEBUG ("new parent is %s with value %s", current.getName ().c_str (), current.get<string> ().c_str ());

	if (n->hasAttributes ())
	{
		// get all the attributes of the node
		DOMNamedNodeMap * pAttributes = n->getAttributes ();
		const XMLSize_t nSize = pAttributes->getLength ();
		ELEKTRA_LOG_DEBUG ("\tAttributes");
		for (XMLSize_t i = 0; i < nSize; ++i)
		{
			DOMAttr * pAttributeNode = dynamic_cast<DOMAttr *> (pAttributes->item (i));
			ELEKTRA_LOG_DEBUG ("\t%s=%s", asCStr (pAttributeNode->getName ()), asCStr (pAttributeNode->getValue ()));
			current.setMeta (toStr (pAttributeNode->getName ()), toStr (pAttributeNode->getValue ()));
		}
	}
}

void analyzeMultipleElements (DOMNode const * n, Key const & current, map<Key, bool> & arrays)
{
	for (auto child = n->getFirstChild (); child != 0; child = child->getNextSibling ())
	{
		Key childKey = newNodeKey (current, child);

		auto it = arrays.find (childKey);
		if (it != arrays.end ())
		{
			if (!it->second)
			{
				ELEKTRA_LOG_DEBUG ("There are multiple elements of %s, mapping this as an array",
						   childKey.getName ().c_str ());
				arrays[childKey] = true;
			}
		}
		else
			arrays[childKey] = false;
	}
}

Key newArrayKey (Key const & arrayKey, KeySet & ks)
{
	KeySet result (elektraArrayGet (arrayKey.getKey (), ks.getKeySet ()));
	if (!result.size ())
	{
		Key arrayBaseKey = arrayKey.dup ();

		Key parentArrayKey = Key (arrayKey.getName (), KEY_END);
		parentArrayKey.setMeta ("array", "empty");
		ks.append (parentArrayKey);

		arrayBaseKey.addBaseName ("#");
		result.append (arrayBaseKey);
	}
	return elektraArrayGetNextKey (result.getKeySet ());
}

void dom2keyset (DOMNode const * n, Key const & parent, KeySet & ks, map<Key, bool> & arrays)
{
	if (n)
	{
		Key current (parent.getName (), KEY_END);

		if (n->getNodeType () == DOMNode::ELEMENT_NODE)
		{
			node2key (n, parent, ks, current);

			auto it = arrays.find (current);
			const bool array = it != arrays.end () && it->second;
			string parentArrayName = current.getName ();
			// Multiple elements with that name, map as an array
			if (array) current.addBaseName (newArrayKey (current, ks).getBaseName ());

			// Only add keys with a value, attributes or leafs or the root to preserve the original name or array keys
			if (n->hasAttributes () || !current.getString ().empty () || !n->getFirstChild () || !ks.size () || array)
			{
				ELEKTRA_LOG_DEBUG ("adding %s", current.getName ().c_str ());
				if (array)
				{
					Key parentArrayKey = ks.lookup (parentArrayName);
					parentArrayKey.setMeta ("array", current.getBaseName ());
				}
				ks.append (current);
			}
			else
			{
				ELEKTRA_LOG_DEBUG ("skipping %s", current.getName ().c_str ());
			}
		}
		// the first level cannot have more children so its enough to check that here
		analyzeMultipleElements (n, current, arrays);
		for (auto child = n->getFirstChild (); child != 0; child = child->getNextSibling ())
			dom2keyset (child, current, ks, arrays);
	}
}

} // namespace

void xerces::deserialize (Key const & parentKey, KeySet & ks)
{
	if (!parentKey.isValid ()) throw XercesPluginException ("Parent key is invalid");
	if (parentKey.get<string> ().empty ()) throw XercesPluginException ("No source file specified as key value");

	ELEKTRA_LOG_DEBUG ("deserializing relative to %s from file %s", parentKey.getName ().c_str (), parentKey.get<string> ().c_str ());
	auto document = doc2dom (parentKey.get<string> ());
	map<Key, bool> arrays;
	if (document) dom2keyset (document->getDocumentElement (), parentKey, ks, arrays);
}
