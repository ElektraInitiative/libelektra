#include <key.hpp>
#include <keyset.hpp>

#include <sstream>
#include <iostream>
#include <iomanip>

using namespace kdb;

void printError(Key error)
{
	if (!error.getMeta<const Key>("error"))
	{
		// no error available
		return;
	}

	try{
		error.getMeta<std::string>("error");
		std::cerr << "Error (#" << error.getMeta<std::string>("error/number") << ") occurred!" << std::endl;
		std::cerr << "Description: " << error.getMeta<std::string>("error/description") << std::endl;
		std::cerr << "Ingroup: " << error.getMeta<std::string>("error/ingroup") << std::endl;
		std::cerr << "Module: " << error.getMeta<std::string>("error/module") << std::endl;
		std::cerr << "At: " << error.getMeta<std::string>("error/file") << ":" << error.getMeta<std::string>("error/line") << std::endl;
		std::cerr << "Reason: " << error.getMeta<std::string>("error/reason") << std::endl;
	} catch (KeyMetaException const& e)
	{
		std::cerr << "Error meta data is not set correctly by a plugin" << std::endl;
	}
}

void printWarnings(Key error)
{
	if (!error.getMeta<const Key>("warnings"))
	{
		// no warnings were issued
		return;
	}

	try{
		int nr = error.getMeta<int>("warnings");
		if (!nr)
		{
			std::cerr << "1 Warning was issued:" << std::endl;
		}
		else
		{
			std::cerr << nr+1 << " Warnings were issued:" << std::endl;
		}

		for (int i=0; i<=nr; i++)
		{
			std::ostringstream name;
			name << "warnings/#" << std::setfill('0') << std::setw(2) << i;
			// std::cerr << "\t" << name.str() << ": " << error.getMeta<std::string>(name.str()) << std::endl;
			std::cerr << "\tWarning number: " << error.getMeta<std::string>(name.str() + "/number") << std::endl;
			std::cerr << "\tDescription: " << error.getMeta<std::string>(name.str() + "/description") << std::endl;
			std::cerr << "\tIngroup: " << error.getMeta<std::string>(name.str() + "/ingroup") << std::endl;
			std::cerr << "\tModule: " << error.getMeta<std::string>(name.str() + "/module") << std::endl;
			std::cerr << "\tAt: " << error.getMeta<std::string>(name.str() + "/file") << ":"
				  << error.getMeta<std::string>(name.str() + "/line") << std::endl;
			std::cerr << "\tReason: " << error.getMeta<std::string>(name.str() + "/reason") << std::endl;
		}

	} catch (KeyMetaException const& e)
	{
		std::cerr << "Warnings meta data not set correctly by a plugin" << std::endl;
	}
}

std::ostream & operator << (std::ostream & os, const kdb::Key &k)
{
	os << k.getName();

	return os;
}

std::ostream & operator << (std::ostream & os, const kdb::KeySet &cks)
{
	kdb::KeySet & ks = const_cast<kdb::KeySet &>(cks);
	cursor_t c = ks.getCursor();
	ks.rewind();
	Key k;
	while (k=ks.next())
	{
		os << k << std::endl;
	}
	ks.setCursor(c);

	return os;
}
