#ifndef ELEKTRA_KCONFIG_SERIALIZER_HPP
#define ELEKTRA_KCONFIG_SERIALIZER_HPP

#include <elektra/core/errors.h>
#include <kdbplugin.hpp>
#include <memory>

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

class KConfigSerializer
{
private:
	/* This output stream points to where we want to save the serialized keys */
	std::unique_ptr<std::ostream> o;
	/* This CppKeySet contains all the keys that we want to serialize*/
	CppKeySet & keySet;
	/* This std::size_t stores the size of the full path of the parent key*/
	std::size_t parentKeyNameSize;
	/* This string is used to store the last group that we saved in the output stream */
	std::string lastPrintedGroup;
	/* This boolean is used to track if we've saved anything in the output stream or not */
	bool isFirstKey;


	/**
	 * @brief This method is used to put strings into the output stream while escaping some of the characters.
	 * This will escape newlines, carriage return, tabs and backslashes. If this is a group key, then there is some additional logic to
	 * handle the forward slashes.
	 *
	 * @param val This string contains the characters the we might escape
	 * @param isGroupKey This boolean tells us if the parameter `val` is a group key name
	 */
	void saveAndEscapeString (const std::string & val, bool isGroupKey);

	/**
	 * @brief This method is used to serialize a key as in the group format.
	 * @param k This CppKey contains the group name including its' metadata
	 */
	void saveGroupKey (CppKey const & k);

	/**
	 * @brief This method is used to serialize a group key while ignoring its' metadata.
	 * @param group This string contains the group name
	 * @param newline This bool is set to true if a newline must be saved after the group key
	 */
	void saveGroupKeyWithoutMeta (std::string const & group, bool newline = true);

	/**
	 * @brief This method serializes a key, and if it's group is not serialized yet, it does that first
	 * @param k This CppKey contains the key, metadata, group and value that we want to serialize
	 */
	void saveLeafKeyWithGroupCandidate (CppKey const & k);

	/**
	 * @brief This method serializes a leaf key into the output stream
	 * @param key This CppKey contains the key, metadata and value that we want to serialize
	 */
	void saveLeafKey (CppKey const & key);

	/**
	 * @brief This method finds the position where the last slash is located. Escaped slashes (sequence `\/`) are ignored.
	 *
	 * WARNING: The value 0 will be return if the last slash is in the 0 position, but also if there was no slash at all.
	 *
	 * @param s This string will be inspected
	 * @return This std::size_t contains the position of the last slash, or 0 if none
	 */
	static std::size_t findLastSlash (std::string const & s);

	/**
	 * @brief This method is used to extract a group name of a full path.
	 * E.g. given `system:/group/subgroup/key.name` it returns `system:/group/subgroup`
	 * @param leafKeyName This string contains the full path
	 * @return This string contains the group name
	 */
	static std::string groupNameFromLeaf (std::string const & leafKeyName);

	class KeyNameComparator;

public:
	/**
	 * @brief This constructor is used to abstract logic of serializing a KeySet to a file. Notice that the file is not saved by just
	 * creating an instance of this class, the method `save()` has to be called afterwards
	 * @param keySetParam This KeySet will be serialized
	 * @param parentParam This Key will be used to get the filename and prefix of the keys that we want to serialize
	 */
	KConfigSerializer (CppKeySet & keySetParam, CppKey & parentParam, std::unique_ptr<std::ostream> oParam);

	/**
	 * This method is used to print the data to the output stream.
	 */
	void save ();
};

class KConfigSerializer::KeyNameComparator
{
private:
	/* This std::size_t contains the number of keynames that the parent key has */
	std::size_t parentKeyCount;

	/**
	 * @brief This method will skip the common prefix that a key has with the parent key.
	 * If the given iterator points to a key name `system:/group/subgroup/key.name` and the parentKeyCount is 2, after this method is
	 * executed the iterator will point to e key name `subgroup/key.name`
	 *
	 * @param it This CppKey::iterator contains the key that we are skipping
	 */
	void skipParent (CppKey::iterator & it);

public:
	/**
	 * @brief This constructor is used to create an object which can compare two values of type CppKey to each other. It uses a CppKey
	 * which is a prefix to both keys so that it doesn't have to compare some parts that are always the same.
	 * @param parentParam This CppKey is the prefix that all keys that we want to compare have in common.
	 */
	explicit KeyNameComparator (CppKey const & parentParam);

	/**
	 * @brief This method compares two keys to each other.
	 *
	 * Analogy:
	 * Consider the KeySet being a tree. If we traverse that tree breadth-first while prioritising leaves over tree nodes, all the Keys
	 * would be sorted with the same logic that this method uses.
	 *
	 * @param keyA This CppKey contains one of the keys that we want to compare
	 * @param keyB This CppKey contains the other key that we want to compare
	 * @retval true if keyA is considered smaller than keyB
	 * @retval false otherwise
	 */
	bool operator() (CppKey const & keyA, CppKey const & keyB);
};

#endif // ELEKTRA_KCONFIG_SERIALIZER_HPP
