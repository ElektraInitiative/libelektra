/**
 * @file
 *
 * @brief deseialization implementation for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "deserializer.hpp"

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
 + Smart pointer helper functions to ease the pain of dealing between XMLCh and std::string
 */

// no need to expand this, just keep it in one line
// TODO move to external file if also used for serializer
// clang-format off
struct XmlChDeleter { void operator() (XMLCh * ptr) { XMLString::release (&ptr); } };

struct StringDeleter {
	char * cStr; // Workaround as ptr->c_str gives as a const * which we cant free
	void operator() (string * ptr) { XMLString::release (&cStr); }
};

// clang-format on

inline static unique_ptr<XMLCh, XmlChDeleter> fromStr (const std::string str)
{
	return unique_ptr<XMLCh, XmlChDeleter> (XMLString::transcode (str.c_str ()));
}

inline static unique_ptr<std::string, StringDeleter> toStr (XMLCh const * xmlCh)
{
	char * cStr = XMLString::transcode (xmlCh);
	return unique_ptr<std::string, StringDeleter> (new string (cStr), StringDeleter{ cStr });
}

/*
 * Actual Xerces logic
 */

static unique_ptr<XercesDOMParser> doc2dom (const std::string src)
{
	unique_ptr<XercesDOMParser> parser (new XercesDOMParser);
	parser->setValidationScheme (XercesDOMParser::Val_Auto);
	parser->setDoNamespaces (false);
	parser->setDoSchema (false);
	parser->setCreateEntityReferenceNodes (false);

	try
	{
		parser->parse (fromStr (src).get ());
	}
	catch (...)
	{
		// TODO better error handling
		std::cerr << "An exception parsing ";
		std::cerr << src << std::endl;
	}

	return parser;
}

static string getElementText (DOMNode const * parent)
{
	string strVal;

	for (auto child = parent->getFirstChild (); child != NULL; child = child->getNextSibling ())
	{
		if (DOMNode::TEXT_NODE == child->getNodeType ())
		{
			DOMText * data = dynamic_cast<DOMText *> (child);
			strVal += *toStr (data->getWholeText ());
		}
	}
	return strVal;
}

static void dom2keyset (DOMNode const * n, KeySet & ks)
{
	if (n)
	{
		if (n->getNodeType () == DOMNode::ELEMENT_NODE)
		{
			cout << "Encountered Element : " << *toStr (n->getNodeName ());

			// Clang doesn't allow POD variadic types so we have to use c_str instead
			Key key (Key (toStr (n->getNodeName ())->c_str (), KEY_VALUE, getElementText (n).c_str (), KEY_END));

			if (n->hasAttributes ())
			{
				// get all the attributes of the node
				DOMNamedNodeMap * pAttributes = n->getAttributes ();
				const XMLSize_t nSize = pAttributes->getLength ();
				cout << "\tAttributes" << endl;
				for (XMLSize_t i = 0; i < nSize; ++i)
				{
					DOMAttr * pAttributeNode = dynamic_cast<DOMAttr *> (pAttributes->item (i));
					cout << "\t" << *toStr (pAttributeNode->getName ()) << "=";
					cout << *toStr (pAttributeNode->getValue ()) << endl;
					key.setMeta (*toStr (pAttributeNode->getName ()), *toStr (pAttributeNode->getValue ()));
				}
			}

			ks.append (key);
		}
		for (auto child = n->getFirstChild (); child != 0; child = child->getNextSibling ())
			dom2keyset (child, ks);
	}
}

// TODO: pass errorKey and set it to something in case we have an error?
void deserialize (const string src, KeySet & ks)
{
	auto parser = doc2dom (src);
	if (!parser->getErrorCount ()) dom2keyset (parser->getDocument (), ks);
}