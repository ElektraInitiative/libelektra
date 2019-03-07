/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_TEMPLATE_HPP
#define ELEKTRA_TEMPLATE_HPP

#include "mustache.hpp"

#include <algorithm>
#include <iostream>
#include <memory>
#include <unordered_set>

#include <kdb.hpp>
#include <kdbconfig.h>

class EmptyGenTemplate;

/**
 * A template for the gen command.
 */
class GenTemplate
{
protected:
	/**
	 * Construct a new template for the gen command.
	 *
	 * A template may consist of multiple parts.
	 * All mustache files for this template must start with @p templateBaseName,
	 * and end with ".mustache". The part in between is determined by an entry in @p parts.
	 *
	 * @param templateBaseName The basename for all mustache files associated with this template.
	 * @param parts	           The suffixes of the different mustache files associated with this template.
	 *                         Pass `{ "" }` if this template only uses a single file, identified by the @p templateBaseName.
	 * @param parameters       The list of parameters this template uses. The keys are the names, while the value
	 *                         determines whether the parameter is required or not.
	 */
	GenTemplate (std::string templateBaseName, std::vector<std::string> parts, std::vector<std::string> partials,
		     const std::unordered_map<std::string, bool> & parameters);

	/**
	 * Construct the mustache template data from the given snapshot of the KDB.
	 *
	 * @param outputName the basename of all output files. The output files are expected to use the same names
	 *                   as the template files, except that `templateBaseName` is replaced by @p outputName.
	 * @param ks         A KeySet containing the data for this template.
	 * @param parentKey  The parent key below which the data for this template resides.
	 *
	 * @return The mustache data needed to render this template.
	 */
	virtual kainjow::mustache::data getTemplateData (const std::string & outputName, const kdb::KeySet & ks,
							 const std::string & parentKey) const = 0;

	/**
	 * Get the value of a parameter.
	 *
	 * @param name         The parameter name
	 * @param defaultValue The default value
	 *
	 * @return the value of the parameter or @p defaultValue, if it wasn't set or is unknown
	 */
	std::string getParameter (const std::string & name, const std::string & defaultValue = "") const;

public:
	/**
	 * @retval true if this is the empty template
	 * @retval false otherwise
	 */
	virtual explicit operator bool () const
	{
		return false;
	}

	/**
	 * Set the value of a parameter.
	 *
	 * @param name The parameter name
	 * @param value The new parameter value
	 */
	void setParameter (const std::string & name, const std::string & value);

	/**
	 * Clears the parameters values set on this template.
	 */
	void clearParameters ();

	/**
	 * @return A list of parts of which this template consists.
	 */
	std::vector<std::string> getParts () const;

	/**
	 * Render a part of this template using the given snapshot of the KDB into the given stream.
	 *
	 * @param output     The stream to which the render result shall be written.
	 * @param outputName the basename of all output files. The output files are expected to use the same names
	 *                   as the template files, except that `templateBaseName` is replaced by @p outputName.
	 * @param part       The name of the part to be rendered.
	 *                   No output will be generated, if this isn't an element of getParts().
	 * @param ks         A KeySet containing the data for this template. Probably resulting from a call to KDB#get.
	 * @param parentKey  The parent key below which the data for this template resides.
	 */
	void render (std::ostream & output, const std::string & outputName, const std::string & part, const kdb::KeySet & ks,
		     const std::string & parentKey) const;

	virtual ~GenTemplate () = default;

private:
	std::string _templateBaseName;
	std::vector<std::string> _parts;
	std::vector<std::string> _partials;
	std::unordered_map<std::string, std::string> _parameters;
	std::unordered_set<std::string> _requiredParameters;

	static std::string escapeFunction (const std::string & str);

	std::unordered_map<std::string, kainjow::mustache::partial> getPartials () const;
};

/**
 * Singleton class containing the list of known templates for the gen command.
 */
class GenTemplateList
{
public:
	/**
	 * @return the singleton instance of GenTemplateList
	 */
	static GenTemplateList & getInstance ()
	{
		static GenTemplateList instance; // Guaranteed to be destroyed. Instantiated on first use.
		return instance;
	}

	/**
	 * Find a template with a given name.
	 *
	 * @param name       the unique name of the template
	 * @param parameters the map of parameters passed to the template
	 *
	 * @return a pointer to the template with the given name initialized with the given parameters
	 * or GenTemplate::empty, if the template wasn't found
	 */
	const GenTemplate * getTemplate (const std::string & name, const std::unordered_map<std::string, std::string> & parameters) const;

private:
	GenTemplateList ();

	template <class genClass>
	void addTemplate (const std::string & name);
	std::unordered_map<std::string, std::unique_ptr<GenTemplate>> _templates;

public:
	// public for better error messages
	GenTemplateList (GenTemplateList const &) = delete;
	void operator= (GenTemplateList const &) = delete;
};

class EmptyGenTemplate : public GenTemplate
{
public:
	/**
	 * @return the singleton instance of EmptyGenTemplate
	 */
	static EmptyGenTemplate & getInstance ()
	{
		static EmptyGenTemplate instance; // Guaranteed to be destroyed. Instantiated on first use.
		return instance;
	}

	explicit operator bool () const override
	{
		return true;
	}

	kainjow::mustache::data getTemplateData (const std::string & outputName ELEKTRA_UNUSED, const kdb::KeySet & ks ELEKTRA_UNUSED,
						 const std::string & parentKey ELEKTRA_UNUSED) const override
	{
		return {};
	}

private:
	EmptyGenTemplate ()
	: GenTemplate ("", std::vector<std::string> (), std::vector<std::string> (), std::unordered_map<std::string, bool> ())
	{
	}

public:
	// public for better error messages
	EmptyGenTemplate (EmptyGenTemplate const &) = delete;
	void operator= (EmptyGenTemplate const &) = delete;
};

#endif // ELEKTRA_TEMPLATE_HPP
