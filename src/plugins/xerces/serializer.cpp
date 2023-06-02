/**
 * @file
 *
 * @brief serialization implementation for xerces plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./serializer.hpp"
#include "./util.hpp"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include <map>

#include <elektra/ease/array.h>
#include <internal/macros/attributes.h>
#include <internal/utility/logger.h>
#include <key.hpp>

XERCES_CPP_NAMESPACE_USE
using namespace std;
using namespace kdb;
using namespace xerces;

namespace
{

DOMElement * findChildWithName (DOMNode const & elem, string const & name)
{
	for (auto child = elem.getFirstChild (); child != NULL; child = child->getNextSibling ())
	{
		if (DOMNode::ELEMENT_NODE == child->getNodeType ())
		{
			DOMElement * childElem = dynamic_cast<DOMElement *> (child);
			if (name == toStr (childElem->getNodeName ())) return childElem;
		}
	}
	return nullptr;
}

// the name parameter is only used in debug mode for logging, not in production, so we suppress the warning
void key2xml (DOMDocument & doc, DOMElement & elem, string const & name ELEKTRA_UNUSED, Key const & key)
{
	ELEKTRA_LOG_DEBUG ("updating element %s", name.c_str ());

	// key value = element value
	if (!key.get<string> ().empty ())
	{
		ELEKTRA_LOG_DEBUG ("creating text for element %s: %s", name.c_str (), key.get<string> ().c_str ());
		elem.appendChild (doc.createTextNode (asXMLCh (key.get<string> ())));
	}

	// meta keys = attributes
	ckdb::KeySet * metaKeys = ckdb::keyMeta (key.getKey ());

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
	{
		const Key curMeta = ckdb::ksAtCursor (metaKeys, it);
		auto metaName = curMeta.getName ().substr (sizeof ("meta:/") - 1);
		if (metaName != ELEKTRA_XERCES_ORIGINAL_ROOT_NAME && metaName != "array")
		{
			ELEKTRA_LOG_DEBUG ("creating attribute %s for element %s: %s", metaName.c_str (), name.c_str (),
					   curMeta.get<string> ().c_str ());
			elem.setAttribute (asXMLCh (metaName), asXMLCh (curMeta.get<string> ()));
		}
	}
}

DOMElement * prepareArrayNodes (DOMDocument & doc, KeySet const & ks, Key & currentPathKey, bool rootPos, string const & name,
				string const & actualName, DOMNode * current, map<Key, DOMElement *> & arrays)
{
	if (rootPos) return nullptr;

	currentPathKey.addBaseName (name);
	auto it = arrays.find (currentPathKey);
	Key arrayKey = currentPathKey.dup ();
	arrayKey.addBaseName ("#");

	// now check if its scanned already, if not, scan it and create and map the node elements
	if (arrays.find (arrayKey) == arrays.end () && it == arrays.end ())
	{
		arrays[arrayKey] = nullptr; // used as a marker not mapped to the DOM for now
		KeySet arrayKeys = ckdb::elektraArrayGet (currentPathKey.getKey (), ks.getKeySet ());
		for (auto ak : arrayKeys)
		{
			ELEKTRA_LOG_DEBUG ("Precreating array node %s", ak->getName ().c_str ());
			DOMElement * arrayNode = doc.createElement (asXMLCh (actualName));
			arrays[ak] = arrayNode;
			current->appendChild (arrayNode);
		}
	}
	return it != arrays.end () ? it->second : nullptr;
}

void appendKey (DOMDocument & doc, KeySet const & ks, Key const & parentKey, string const & originalRootName, Key const & key,
		map<Key, DOMElement *> & arrays)
{
	DOMNode * current = &doc;

	// Find the key's insertion point, creating the path if non existent
	ELEKTRA_LOG_DEBUG ("serializing key %s", key.getName ().c_str ());

	// Strip the parentKey, as we use relative paths
	auto parentName = parentKey.begin ();
	auto name = key.begin ();
	while (parentName != --parentKey.end () && name != key.end ())
	{
		parentName++;
		name++;
	}

	if (name == key.end ()) throw XercesPluginException ("Key " + key.getName () + " is not under " + parentKey.getName ());

	// restore original root element name if present
	const auto rootPos = name;

	// Now create the path
	Key currentPathKey = parentKey.dup ();
	string actualName;
	DOMElement * child = nullptr;
	for (; name != key.end (); name++)
	{
		actualName = !originalRootName.empty () && name == rootPos ? originalRootName : (*name);
		// If we are not at the root element we scan for array keys as those need special treatment and use their mapped node
		DOMElement * arrayChild = prepareArrayNodes (doc, ks, currentPathKey, name == rootPos, *name, actualName, current, arrays);
		// skip the array part of the path, in xml we can have multiple elements with the same name directly
		child = arrayChild ? arrayChild : findChildWithName (*current, actualName);

		if (!child)
		{
			ELEKTRA_LOG_DEBUG ("creating path element %s", actualName.c_str ());
			child = doc.createElement (asXMLCh (actualName));
			current->appendChild (child);
		}
		current = child;
	}

	// Now we are at the key's insertion point and the last key name part, the loop has already set all our elements
	if (child) key2xml (doc, *child, actualName, key);
}

void ks2dom (DOMDocument & doc, Key const & parentKey, KeySet const & ks)
{
	Key root = ks.lookup (parentKey);
	const string originalRootName =
		root.hasMeta (ELEKTRA_XERCES_ORIGINAL_ROOT_NAME) ? root.getMeta<string> (ELEKTRA_XERCES_ORIGINAL_ROOT_NAME) : "";
	map<Key, DOMElement *> arrays;
	for (auto const & k : ks)
		appendKey (doc, ks, parentKey, originalRootName, k, arrays);
}

} // namespace

void xerces::serialize (Key const & parentKey, KeySet const & ks)
{
	if (!parentKey.isValid ()) throw XercesPluginException ("Parent key is invalid");
	if (parentKey.get<string> ().empty ()) throw XercesPluginException ("No destination file specified as key value");

	ELEKTRA_LOG_DEBUG ("serializing relative to %s to file %s", parentKey.getName ().c_str (), parentKey.get<string> ().c_str ());
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
