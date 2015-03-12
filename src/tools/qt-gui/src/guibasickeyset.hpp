#ifndef GUIBASICKEYSET_HPP
#define GUIBASICKEYSET_HPP

#include <kdb.hpp>

/**
 * @brief The GUIBasicKeySet class. It encapsules the current basic kdb::KeySet needed for threewaymerging.
 */
class GUIBasicKeySet
{
public:
	/**
	 * @brief basic Gets the current basic kdb::KeySet
	 * @return The current basic kdb::KeySet
	 */
	static kdb::KeySet basic();

	/**
	 * @brief setBasic Sets the current basic kdb::KeySet
	 * @param basic The current basic kdb::KeySet
	 */
	static void setBasic(const kdb::KeySet &basic);

private:
	static kdb::KeySet m_basic;
};

#endif // GUIBASICKEYSET_HPP
