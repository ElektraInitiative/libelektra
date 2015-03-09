#ifndef GUIBASICKEYSET_HPP
#define GUIBASICKEYSET_HPP

#include <kdb.hpp>

/**
 * @brief The GUIBasicKeySet class
 */
class GUIBasicKeySet
{
public:
	/**
	 * @brief basic
	 * @return
	 */
	static kdb::KeySet basic();

	/**
	 * @brief setBasic
	 * @param basic
	 */
	static void setBasic(const kdb::KeySet &basic);

private:
	static kdb::KeySet m_basic;
};

#endif // GUIBASICKEYSET_HPP
