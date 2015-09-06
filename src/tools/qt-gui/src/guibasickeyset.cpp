#include "guibasickeyset.hpp"

kdb::KeySet GUIBasicKeySet::m_basic;

kdb::KeySet GUIBasicKeySet::basic()
{
	return m_basic;
}

void GUIBasicKeySet::setBasic(const kdb::KeySet &basic)
{
	m_basic = basic;
}
