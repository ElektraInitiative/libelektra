#include "./kconfig_serializer.hpp"
#include "./base.hpp"
#include <algorithm>
#include <vector>

KConfigSerializer::KConfigSerializer (CppKeySet & keySetParam, CppKey & parentParam, std::unique_ptr<std::ostream> oParam)
: o{ std::move (oParam) }, keySet{ keySetParam }, parentKeyNameSize{ parentParam.getName ().size () + 1 }, lastPrintedGroup{ "" },
  isFirstKey{ true }
{
	std::string parentKeyName = parentParam.getName ();
	if (parentKeyName == "/")
	{
		parentKeyNameSize = 1;
	}
	else
	{
		parentKeyNameSize = parentKeyName.size () + 1;
	}
}

void KConfigSerializer::save ()
{
	std::vector<CppKey> keys{ keySet.begin (), keySet.end () };
	//	std::sort (keys.begin (), keys.end (), KConfigSerializer::KeyNameComparator{ parent });

	const CppKey * groupCandidate{ nullptr };

	for (const auto & k : keys)
	{
		if (groupCandidate != nullptr)
		{
			if (k.getName ().rfind (groupCandidate->getName (), 0) == 0)
			{
				saveGroupKey (*groupCandidate);
				lastPrintedGroup = groupCandidate->getName ();
			}
			else
			{
				saveLeafKeyWithGroupCandidate (*groupCandidate);
			}
			groupCandidate = nullptr;
		}

		if (!k.getString ().empty ())
		{
			saveLeafKeyWithGroupCandidate (k);
		}
		else
		{
			groupCandidate = &k;
		}
	}
}

void KConfigSerializer::saveAndEscapeString (const std::string & val, bool isGroupKey)
{
	std::ostream & out = *o;

	char currentChar;
	for (std::size_t i{ 0 }; i < val.size (); ++i)
	{
		currentChar = val[i];
		switch (currentChar)
		{
		case '\\':
			if (isGroupKey && i < val.size () && val[i + 1] == '/')
			{
				// Forward slash is parsed as "\/"
				out << '/';
				++i;
			}
			else
			{
				out << "\\\\";
			}
			break;
		case '/':
			if (isGroupKey)
			{
				out << "][";
			}
			else
			{
				out << '/';
			}
			break;
		case '\n':
			out << "\\n";
			break;
		case '\r':
			out << "\\r";
			break;
		case '\t':
			out << "\\t";
			break;
		default:
			out << currentChar;
		}
	}
}

void KConfigSerializer::saveGroupKey (CppKey const & k)
{
	saveGroupKeyWithoutMeta (k.getName (), false);

	std::string metadata{ k.getMeta<std::string> (KCONFIG_METADATA_KEY) };

	std::ostream & out = *o;
	if (!metadata.empty ())
	{

		out << character_open_bracket;
		out << character_dollar_sign;
		out << metadata;
		out << character_close_bracket;
	}
	out << character_newline;
}

void KConfigSerializer::saveGroupKeyWithoutMeta (std::string const & group, bool newline)
{
	std::ostream & out = *o;
	std::size_t skipChars = parentKeyNameSize;

	if (group.size () > skipChars)
	{
		std::string outstr{ group.substr (skipChars) };

		if (!isFirstKey)
		{
			out << character_newline;
		}
		else
		{
			isFirstKey = false;
		}

		out << character_open_bracket;
		saveAndEscapeString (outstr, true);
		out << character_close_bracket;
		if (newline)
		{
			out << character_newline;
		}
	}
}

void KConfigSerializer::saveLeafKeyWithGroupCandidate (CppKey const & k)
{
	std::string currentGroupName{ groupNameFromLeaf (k.getName ()) };
	if (lastPrintedGroup != currentGroupName)
	{
		saveGroupKeyWithoutMeta (currentGroupName);
		lastPrintedGroup = currentGroupName;
	}
	saveLeafKey (k);
}

void KConfigSerializer::saveLeafKey (CppKey const & key)
{
	std::ostream & out = *o;
	isFirstKey = false;
	saveAndEscapeString (key.getBaseName (), false);

	std::string meta = key.getMeta<std::string> (KCONFIG_METADATA_KEY);
	for (char c : meta)
	{
		out << character_open_bracket;
		out << character_dollar_sign;
		out << c;
		out << character_close_bracket;
	}

	out << character_equals_sign;
	saveAndEscapeString (key.getString (), false);
	out << character_newline;
}

std::size_t KConfigSerializer::findLastSlash (std::string const & s)
{
	std::size_t count{ 0 };

	for (std::size_t i{ 0 }; i < s.size (); i++)
	{
		switch (s[i])
		{
		case '/':
			count = i;
			break;
		case '\\':
			++i;
		}
	}

	return count;
}

std::string KConfigSerializer::groupNameFromLeaf (std::string const & leafKeyName)
{
	return leafKeyName.substr (0, findLastSlash (leafKeyName));
}

KConfigSerializer::KeyNameComparator::KeyNameComparator (CppKey const & parent)
{
	auto iter = parent.begin ();
	parentKeyCount = 0;

	while (iter != parent.end ())
	{
		iter++;
		parentKeyCount++;
	}
}

bool KConfigSerializer::KeyNameComparator::operator() (CppKey const & keyA, CppKey const & keyB)
{
	auto itA = keyA.begin ();
	auto itB = keyB.begin ();

	// Don't compare the parent
	this->skipParent (itA);
	this->skipParent (itB);

	auto endA = keyA.end ();
	auto endB = keyB.end ();

	while (true)
	{
		// pointer to where the current keys are stored
		const char * strPosA = itA.pos ();
		const char * strPosB = itB.pos ();

		++itA;
		++itB;

		// Check if they're group keys, or leaf keys
		bool endsA{ itA == endA };
		bool endsB{ itB == endB };

		if (endsA || endsB)
		{
			// If one of them is a leaf key, we don't need to iterate further
			if (endsA && endsB)
			{
				// If both are leaf keys, return the compared value
				return strcmp (strPosA, strPosB) < 0;
			}
			return endsA;
		}
		// Compare the current branch key of a to that of b
		int compareAtoB{ strcmp (strPosA, strPosB) };

		if (compareAtoB != 0)
		{
			// If the names don't match, don't continue iterating and return
			return compareAtoB < 0;
		}
	}
}

void KConfigSerializer::KeyNameComparator::skipParent (CppKey::iterator & it)
{
	for (std::size_t i{ 0 }; i < parentKeyCount; ++i)
	{
		it++;
	}
}
