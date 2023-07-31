
#ifndef ELEKTRA_ERRORTYPES_HPP
#define ELEKTRA_ERRORTYPES_HPP
#include "error.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

/* Not an Error by itself, but a container for multiple Warnings,
 * like keys in the C-API have at most one error, but 0 to n warnings.
 * This way the content of such a key can be stored in a single Error object,
 * even if the key doesn't contain an actual error. */
class PureWarningError : public Error
{
public:
	PureWarningError () : Error{ "Warnings", "No error, only warnings.", "", "", "", "", 0 }
	{
	}

	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};


/* Concrete classes for the different Errors, based on the constants defined in /src/include/kdberrors.h */
class ResourceError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class OutOfMemoryError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class InstallationError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};


class InternalError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class InterfaceError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class PluginMisbehaviorError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class ConflictingStateError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class ValidationSyntacticError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class ValidationSemanticError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

class CliError : public Error
{
public:
	using Error::Error;
	std::string code () const override;

private:
	bool compare (const BaseNotification & other) const final;
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERRORTYPES_HPP
