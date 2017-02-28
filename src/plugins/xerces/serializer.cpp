/**
 * @file
 *
 * @brief serialization implementation for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "serializer.hpp"
#include "util.hpp"

#include <fstream>
#include <functional>
#include <iostream>
#include <memory>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#include <key.hpp>

XERCES_CPP_NAMESPACE_USE
using namespace std;
using namespace kdb;

static DOMElement * findChildWithName (DOMElement const & elem, std::string const & name)
{
	for (auto child = elem.getFirstChild (); child != NULL; child = elem.getNextSibling ())
	{
		if (DOMNode::ELEMENT_NODE == child->getNodeType ())
		{
			DOMElement * childElem = dynamic_cast<DOMElement *> (child);
			if (name == toStr (childElem->getNodeName ())) return childElem;
		}
	}
	return nullptr;
}

static void appendKey (DOMDocument & doc, Key const & key)
{
	DOMElement * current = doc.getDocumentElement ();

	// Find the key's insertion point, creating the path if non existent
	const auto end = key.begin () != key.end () ? --key.end () : key.end ();
	auto name = key.begin ();
	for (; name != end; name++)
	{
		string actualName = *name;
		if (name == key.begin () && actualName.empty ()) actualName = "cascading";

		DOMElement * child = findChildWithName (*current, actualName);
		if (!child)
		{
			child = doc.createElement (asXMLCh (actualName));
			current->appendChild (child);
		}
		current = child;
	}

	// Now we are at the key's insertion point and the last key name part
	DOMElement * elem = doc.createElement (asXMLCh (*name));
	// key value = element value
	if (!key.get<string> ().empty ())
	{
		elem->appendChild (doc.createTextNode (asXMLCh (key.get<string> ())));
	}

	// meta keys = attributes
	Key itKey = key.dup (); // We can't use nextMeta on const key
	itKey.rewindMeta ();
	while (Key const & meta = itKey.nextMeta ())
	{
		cout << "setting element " << meta.getName () << " to " << meta.get<string> () << flush << endl;
		elem->setAttribute (asXMLCh (meta.getName ()), asXMLCh (meta.get<string> ()));
	}

	current->appendChild (elem);
}

static void ks2dom (DOMDocument & doc, kdb::KeySet const & ks)
{
	for (auto const & k : ks)
	{
		appendKey (doc, k);
	}
}

void serialize (std::string const & src, kdb::KeySet const & ks)
{
	DOMImplementation * impl = DOMImplementationRegistry::getDOMImplementation (asXMLCh ("Core"));
	if (impl != NULL)
	{
		xerces_unique_ptr<DOMDocument> document (impl->createDocument (0, asXMLCh ("namespace"), 0));
		ks2dom (*document, ks);

		DOMImplementationLS * implLS = dynamic_cast<DOMImplementationLS *> (impl->getImplementation ());

		xerces_unique_ptr<DOMLSSerializer> serializer (implLS->createLSSerializer ());
		DOMConfiguration * serializerConfig = serializer->getDomConfig ();
		if (serializerConfig->canSetParameter (XMLUni::fgDOMWRTFormatPrettyPrint, true))
			serializerConfig->setParameter (XMLUni::fgDOMWRTFormatPrettyPrint, true);

		LocalFileFormatTarget targetFile (asXMLCh (src));
		xerces_unique_ptr<DOMLSOutput> output (implLS->createLSOutput ());
		output->setByteStream (&targetFile);

		serializer->write (document.get (), output.get ());
	}
	else
		throw XercesPluginException ("DOMImplementation not available");
}