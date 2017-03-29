/**
 * @file
 *
 * @brief serialization implementation for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "serializer.hpp"
#include "util.hpp"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <kdblogger.h>
#include <key.hpp>

XERCES_CPP_NAMESPACE_USE
using namespace std;
using namespace kdb;
using namespace xerces;

namespace
{

DOMElement * findChildWithName (DOMNode const & elem, string const & name)
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

void key2xml (DOMDocument & doc, DOMElement & elem, string name, Key const & key)
{
	ELEKTRA_LOG_DEBUG ("updating element %s", name.c_str ());

	// key value = element value
	if (!key.get<string> ().empty ())
	{
		ELEKTRA_LOG_DEBUG ("creating text for element %s: %s", name.c_str (), key.get<string> ().c_str ());
		elem.appendChild (doc.createTextNode (asXMLCh (key.get<string> ())));
	}

	// meta keys = attributes
	Key itKey = key.dup (); // We can't use nextMeta on const key
	itKey.rewindMeta ();
	while (Key const & meta = itKey.nextMeta ())
	{
		if (meta.getName () != ELEKTRA_XERCES_ORIGINAL_ROOT_NAME)
		{
			ELEKTRA_LOG_DEBUG ("creating attribute %s for element %s: %s", meta.getName ().c_str (), name.c_str (),
					   meta.get<string> ().c_str ());
			elem.setAttribute (asXMLCh (meta.getName ()), asXMLCh (meta.get<string> ()));
		}
	}
}


void appendKey (DOMDocument & doc, Key const & parentKey, string const & originalRootName, Key const & key)
{
	DOMNode * current = &doc;

	// Find the key's insertion point, creating the path if non existent

	// Strip the parentKey, as we use relative paths
	auto parentName = parentKey.begin ();
	auto name = key.begin ();
	while (parentName != --parentKey.end () && name != key.end ())
	{
		parentName++;
		name++;
	}

	if (name == key.end ())
	{
		throw new XercesPluginException ("Key " + key.getFullName () + " is not under " + parentKey.getFullName ());
	}

	// restore original root element name if present
	auto rootPos = name;

	// Now create the path
	for (; name != --key.end (); name++)
	{
		const string actualName = !originalRootName.empty () && name == rootPos ? originalRootName : (*name);
		DOMElement * child = findChildWithName (*current, actualName);
		if (!child)
		{
			ELEKTRA_LOG_DEBUG ("creating path element %s", actualName.c_str ());
			child = doc.createElement (asXMLCh (actualName));
			current->appendChild (child);
		}
		current = child;
	}

	// Now we are at the key's insertion point and the last key name part
	const string actualName = !originalRootName.empty () && name == rootPos ? originalRootName : (*name);
	DOMElement * child = findChildWithName (*current, actualName);
	DOMElement * elem = child ? child : doc.createElement (asXMLCh (actualName));
	key2xml (doc, *elem, actualName, key);
	if (!child) current->appendChild (elem);
}

void ks2dom (DOMDocument & doc, Key const & parentKey, KeySet const & ks)
{
	Key root = ks.lookup (parentKey);
	const string originalRootName =
		root.hasMeta (ELEKTRA_XERCES_ORIGINAL_ROOT_NAME) ? root.getMeta<string> (ELEKTRA_XERCES_ORIGINAL_ROOT_NAME) : "";
	for (auto const & k : ks)
	{
		appendKey (doc, parentKey, originalRootName, k);
	}
}

} // namespace

void xerces::serialize (Key const & parentKey, KeySet const & ks)
{
	if (!parentKey.isValid ()) throw XercesPluginException ("Parent key is invalid");
	if (parentKey.get<string> ().empty ()) throw XercesPluginException ("No destination file specified as key value");

	ELEKTRA_LOG_DEBUG ("serializing relative to %s to file %s", parentKey.getFullName ().c_str (), parentKey.get<string> ().c_str ());
	DOMImplementation * impl = DOMImplementationRegistry::getDOMImplementation (asXMLCh ("Core"));
	if (impl != NULL)
	{
		XercesPtr<DOMDocument> doc (impl->createDocument ());
		ks2dom (*doc, parentKey, ks);

		DOMImplementationLS * implLS = dynamic_cast<DOMImplementationLS *> (impl->getImplementation ());

		XercesPtr<DOMLSSerializer> serializer (implLS->createLSSerializer ());
		DOMConfiguration * serializerConfig = serializer->getDomConfig ();
		if (serializerConfig->canSetParameter (XMLUni::fgDOMWRTFormatPrettyPrint, true))
			serializerConfig->setParameter (XMLUni::fgDOMWRTFormatPrettyPrint, true);

		LocalFileFormatTarget targetFile (asXMLCh (parentKey.get<string> ()));
		XercesPtr<DOMLSOutput> output (implLS->createLSOutput ());
		output->setByteStream (&targetFile);

		serializer->write (doc.get (), output.get ());
	}
	else
		throw XercesPluginException ("DOMImplementation not available");
}
