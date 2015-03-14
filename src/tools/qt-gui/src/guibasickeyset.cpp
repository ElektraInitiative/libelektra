#include "guibasickeyset.hpp"

kdb::KeySet GUIBasicKeySet::m_basic;

kdb::KeySet GUIBasicKeySet::basic()
{
	return m_basic;
}

void GUIBasicKeySet::setBasic(const kdb::KeySet &basic)
{
	kdb::KeySet set;
	basic.rewind();

	while(basic.next())
		set.append(basic.current().dup());

	m_basic = set;
}
