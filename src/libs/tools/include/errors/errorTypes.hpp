
#ifndef ELEKTRA_ERRORTYPES_HPP
#define ELEKTRA_ERRORTYPES_HPP

#include "error.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

/* This file contains all concrete classes for the different Errors, based on the constants defined in /src/include/kdberrors.h */

class ResourceError : public Error
{
public:
	ResourceError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
		: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class OutOfMemoryError : public Error
{
public:
	OutOfMemoryError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

		std::string code() const override;
	std::string description() const override;
};

class InstallationError : public Error
{
public:
	InstallationError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};


class InternalError: public Error
{
public:
	InternalError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class InterfaceError : public Error
{
public:
	InterfaceError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class PluginMisbehaviorError : public Error
{
public:
	PluginMisbehaviorError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class ConflictingStateError : public Error
{
public:
	ConflictingStateError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class ValidationSyntacticError : public Error
{
public:
	ValidationSyntacticError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class ValidationSemanticError : public Error
{
public:
	ValidationSemanticError (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Error { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERRORTYPES_HPP
