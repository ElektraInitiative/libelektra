/**
 * @file
 *
 * @brief deserialization implementation for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "deserializer.hpp"
#include "util.hpp"

#include <fstream>
#include <functional>
#include <iostream>
#include <memory>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMError.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <key.hpp>

XERCES_CPP_NAMESPACE_USE
using namespace std;
using namespace kdb;

/*
 * Actual Xerces logic
 */

static xerces_unique_ptr<DOMDocument> doc2dom (std::string const & src)
{
	XercesDOMParser parser;
	parser.setValidationScheme (XercesDOMParser::Val_Auto);
	parser.setDoNamespaces (false);
	parser.setDoSchema (false);
	parser.setCreateEntityReferenceNodes (false);

	parser.parse (asXMLCh (src));
	return xerces_unique_ptr<DOMDocument> (parser.adoptDocument ());
}

static string getElementText (DOMNode const * parent)
{
	string str;

	for (auto child = parent->getFirstChild (); child != NULL; child = child->getNextSibling ())
	{
		if (DOMNode::TEXT_NODE == child->getNodeType ())
		{
			DOMText * data = dynamic_cast<DOMText *> (child);
			if (!data->getIsElementContentWhitespace ()) str += toStr (data->getData ());
		}
	}
	// trim whitespace
	const char * ws = " \t\n\r\f\v";
	str.erase (str.find_last_not_of (ws) + 1);
	str.erase (0, str.find_first_not_of (ws));
	return str;
}

static void dom2keyset (DOMNode const * n, KeySet & ks, Key const & parent)
{
	if (n)
	{
		Key current = parent.dup ();
		if (n->getNodeType () == DOMNode::ELEMENT_NODE)
		{
			string keyName = toStr (n->getNodeName ());
			cout << "Encountered Element : " << keyName << endl;
			current.addBaseName (keyName);

			string text = getElementText (n);
			current.set<string> (text);

			if (!current.isValid ())
			{
				// TODO we've encountered an invalid namespace or keyname, ignore, fail, ?
			}

			cout << "new parent is " << current.getFullName () << " with value " << current.get<string> () << endl;

			if (n->hasAttributes ())
			{
				// get all the attributes of the node
				DOMNamedNodeMap * pAttributes = n->getAttributes ();
				const XMLSize_t nSize = pAttributes->getLength ();
				cout << "\tAttributes" << endl;
				for (XMLSize_t i = 0; i < nSize; ++i)
				{
					DOMAttr * pAttributeNode = dynamic_cast<DOMAttr *> (pAttributes->item (i));
					cout << "\t" << toStr (pAttributeNode->getName ()) << "=";
					cout << toStr (pAttributeNode->getValue ()) << endl;
					current.setMeta (toStr (pAttributeNode->getName ()), toStr (pAttributeNode->getValue ()));
				}
			}
			// Only add keys with a value, attributes or leafs
			if (n->hasAttributes () || !text.empty () || !n->getFirstChild ()) ks.append (current);
		}
		for (auto child = n->getFirstChild (); child != 0; child = child->getNextSibling ())
			dom2keyset (child, ks, current);
	}
}

void dom2keyset (DOMDocument const & doc, KeySet & ks)
{
	// root namespace element
	DOMElement * root = doc.getDocumentElement ();

	if (root)
	{
		// the namespace elements
		for (auto ns = root->getFirstChild (); ns != nullptr; ns = ns->getNextSibling ())
		{
			string keyName = toStr (ns->getNodeName ());
			if (keyName == "cascading") keyName = "/";
			Key parent (keyName);
			// the actual content nodes
			for (auto child = ns->getFirstChild (); child != nullptr; child = child->getNextSibling ())
			{
				dom2keyset (child, ks, parent);
			}
		}
	}
}

void deserialize (string const & src, KeySet & ks)
{
	auto document = doc2dom (src);
	if (document) dom2keyset (*document, ks);
}