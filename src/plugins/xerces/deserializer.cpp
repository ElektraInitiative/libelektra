/**
 * @file
 *
 * @brief deserialization implementation for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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
#include <kdblogger.h>
#include <key.hpp>
#include <locale>

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
		if (!all_of (to.begin (), to.end (), [](char c) { return isspace (c, locale ()); }))
		{
			trimmed += to;
		}
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

void node2key (DOMNode const * n, Key const & parent, KeySet const & ks, Key & current)
{
	const string keyName = toStr (n->getNodeName ());
	ELEKTRA_LOG_DEBUG ("Encountered Element: %s", keyName.c_str ());

	if (!ks.size ())
	{ // we map the parent key to the xml root element
		// preserve the original name if its different
		auto parentName = parent.rbegin ();
		if (parentName != parent.rend () && (*parentName) != keyName)
		{
			ELEKTRA_LOG_DEBUG ("parent name %s differs from root element name %s", (*parentName).c_str (), keyName.c_str ());
			current.setMeta (ELEKTRA_XERCES_ORIGINAL_ROOT_NAME, keyName);
		}
	}
	else
	{
		current.addBaseName (keyName);
	}

	const string text = getElementText (n);
	current.set<string> (text);

	if (!current.isValid ())
	{
		throw XercesPluginException ("Given keyset contains invalid keys to serialize");
	}

	ELEKTRA_LOG_DEBUG ("new parent is %s with value %s", current.getFullName ().c_str (), current.get<string> ().c_str ());

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

void dom2keyset (DOMNode const * n, Key const & parent, KeySet & ks)
{
	if (n)
	{
		Key current (parent.getFullName (), KEY_END);
		if (n->getNodeType () == DOMNode::ELEMENT_NODE)
		{
			node2key (n, parent, ks, current);
			// Only add keys with a value, attributes or leafs or the root to preserve the original name
			if (n->hasAttributes () || !current.getString ().empty () || !n->getFirstChild () || !ks.size ())
			{
				ELEKTRA_LOG_DEBUG ("adding %s", current.getFullName ().c_str ());
				ks.append (current);
			}
		}
		for (auto child = n->getFirstChild (); child != 0; child = child->getNextSibling ())
			dom2keyset (child, current, ks);
	}
}

} // namespace

void xerces::deserialize (Key const & parentKey, KeySet & ks)
{
	if (!parentKey.isValid ()) throw XercesPluginException ("Parent key is invalid");
	if (parentKey.get<string> ().empty ()) throw XercesPluginException ("No source file specified as key value");

	ELEKTRA_LOG_DEBUG ("deserializing relative to %s from file %s", parentKey.getFullName ().c_str (),
			   parentKey.get<string> ().c_str ());
	auto document = doc2dom (parentKey.get<string> ());
	if (document) dom2keyset (document->getDocumentElement (), parentKey, ks);
}
