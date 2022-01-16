
#include "iostream"
#include "errors/error.hpp"


namespace kdb
{
namespace tools
{
namespace errors
{


/* copy warning to make it independent from source object *
 * if the same warning gets added to two different errors, they can be changed independently */
void Error::addWarning (Warning & warning)
{
	/* TODO: Decide if we should create copies or store the original warnings */
	warnings.push_back (warning.clone());
}

/* getters */
kdb::long_t Error::warningCount ()
{
	return warnings.size();
}

Warning& Error::operator[](int index)
{
	if(index >= warningCount())
	{
		throw std::out_of_range ("The warning with index " + std::to_string (index) + " was accessed, but there are only "
					 + std::to_string(warningCount()) + " warnings stored in the Error-object!");
	}
	else
	{
		return (*(warnings[index]));
	}

}


bool Error::compare(const BaseNotification& other) const
{
	/* comparison of data fields is done by operator== in BaseNotification class */
	const Error* pOtherError = dynamic_cast<const Error*> (&other);
	if(!pOtherError || warnings.size() != pOtherError->warnings.size())
	{
		return false;
	}
	else
	{
		/* compare warnings */
		for (Warning *w : warnings)
		{
			/*TODO: Decide if ordering of warnings should be considered for equality.
			 * Currently two errors are equal if they contain the same warnings (compared by member values),
			 * even if they have different orders in the internal vector.*/

			/*TODO: Currently copies of warnings are stored by the addWarning(Warning&) method.
			 * If we decide to store the original warnings, then we have to decide if
			 * two different Warnings (not the same address in mem) are considered equal
			 * if the member values are equal.*/
			bool equalWarningFound = false;
			for (Warning *ow : pOtherError->warnings)
			{
				if (*w == *ow)
				{
					equalWarningFound = true;
					break;
				}
			}

			if (!equalWarningFound)
			{
				return false;
			}
		}
		return true;
	}

}
Error::~Error ()
{
	for (Warning *w : warnings)
		delete w;

}

} // namespace errors
} // namespace tools
} // namespace kdb