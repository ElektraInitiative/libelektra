
#ifndef ELEKTRA_WARNINGTYPES_HPP
#define ELEKTRA_WARNINGTYPES_HPP

#include "warning.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

/* This file contains all concrete classes for the different Warnings, based on the constants defined in /src/include/kdberrors.h */

class ResourceWarning : public Warning
{
public:
	ResourceWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
		: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class OutOfMemoryWarning : public Warning
{
public:
	OutOfMemoryWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class InstallationWarning : public Warning
{
public:
	InstallationWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class InternalWarning : public Warning
{
public:
	InternalWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class InterfaceWarning : public Warning
{
public:
	InterfaceWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class PluginMisbehaviorWarning : public Warning
{
public:
	PluginMisbehaviorWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class ConflictingStateWarning : public Warning
{
public:
	ConflictingStateWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class ValidationSyntacticWarning : public Warning
{
public:
	ValidationSyntacticWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

class ValidationSemanticWarning : public Warning
{
public:
	ValidationSemanticWarning (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
	: Warning { reason, module, file, line} {}

	std::string code() const override;
	std::string description() const override;
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_WARNINGTYPES_HPP
