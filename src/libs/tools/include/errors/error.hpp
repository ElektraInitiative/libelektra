#ifndef ELEKTRA_ERROR_HPP
#define ELEKTRA_ERROR_HPP

#include <vector>
#include "errors/warning.hpp"

namespace kdb
{
namespace tools
{

namespace errors
{

class Error : public BaseNotification
{
public:

	/* inherit constructors */
	using BaseNotification::BaseNotification;
	virtual ~Error();

	/**
	 * @brief Add a warning to an error
	 *
	 * The warning is copied to make it independent from the source object.
	 * This way the same warning added to two different errors can be
	 * changed independently.
	 *
	 * An object of type `Error` can contain 0 to n warnings,
	 * like Keys in the C-API can contain one error,
	 * but multiple warnings.
	 *
	 * @param warning the warning to add
	 */
	void addWarning (Warning & warning);

	/* getters */
	kdb::long_t warningCount ();

	/* iterator functionality */
	std::vector<Warning*>::iterator begin() { return warnings.begin(); }
	std::vector<Warning*>::iterator end() { return warnings.end(); }
	std::vector<Warning*>::const_iterator begin() const { return warnings.begin(); }
	std::vector<Warning*>::const_iterator end() const { return warnings.begin(); }
	std::vector<Warning*>::const_iterator cbegin() const { return warnings.cbegin(); }
	std::vector<Warning*>::const_iterator cend() const { return warnings.cend(); }

	/* get warning by index */
	Warning& operator[](int index);

private:
	std::vector<Warning*> warnings;

protected:

	/**
	 * @brief Compare errors
	 *
	 * The comparison of data fields is done by operator== in the BaseNotification class.
	 * This function compares an errors warnings in addition to the notification fields.
	 *
	 * @param other the notification to compare to
	 *
	 * @return true if objects are equal
	 */
	bool compare(const BaseNotification& other) const override;
};
} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERROR_HPP
