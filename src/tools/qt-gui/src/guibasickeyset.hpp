#ifndef GUIBASICKEYSET_HPP
#define GUIBASICKEYSET_HPP

#include <kdb.hpp>

class GUIBasicKeySet
{
public:

	static kdb::KeySet basic();
	static void setBasic(const kdb::KeySet &basic);

private:
	static kdb::KeySet m_basic;
};

#endif // GUIBASICKEYSET_HPP
