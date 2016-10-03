#include <algorithm>
#include <iostream>

#include "service.hpp"

namespace kdbrest
{

namespace service
{

/**
         * @brief Can be used to filter an entry vector based on a name prefix.
         * All entries that do not start with the given name prefix will be
         * removed from the result.
         *
         * @param entries A vector containing entries to be filtered
         * @param startsWith The string containing the required entry name prefix
         */
void SearchEngine::filterConfigurationsByName (std::vector<kdbrest::model::Entry> & entries, const std::string & startsWith)
{

	entries.erase (std::remove_if (entries.begin (), entries.end (),
				       [startsWith](kdbrest::model::Entry & elem) -> bool {
					       if (elem.getPublicName ().compare (0, startsWith.size (), startsWith))
					       {
						       return true;
					       }
					       return false;
				       }),
		       entries.end ());
}

/**
         * @brief Can be used to filter an entry vector based on a `filter` (search string).
		 *
		 * Whether an entry remains in the list depends on the used `filter`. The searched
		 * field(s) can be specified with the `filterby` parameter, the search value itself
		 * in the `filter` value.
		 *
		 * Possible values for `filterby` are:
		 * - all
		 * - key
		 * - title
		 * - description
		 * - author
		 * - tags
         *
         * Entries that do not have the `filter` value in the `filterby` field(s) will be removed.
         *
         * @param entries The vector containing snippet entries to be filtered
         * @param filter The string to be searched for
		 * @param filterby The field(s) to search in
         */
void SearchEngine::findConfigurationsByFilter (std::vector<kdbrest::model::Entry> & entries, const std::string & filter,
					       const std::string filterby)
{
	entries.erase (std::remove_if (entries.begin (), entries.end (),
				       [filter, filterby](kdbrest::model::Entry & elem) -> bool {
					       if (filterby == "key")
					       {
						       if (elem.getPublicName ().find (filter) == std::string::npos)
						       {
							       return true; // remove from result set
						       }
					       }
					       else if (filterby == "title")
					       {
						       if (elem.getTitle ().find (filter) == std::string::npos)
						       {
							       return true; // remove from result set
						       }
					       }
					       else if (filterby == "description")
					       {
						       if (elem.getDescription ().find (filter) == std::string::npos)
						       {
							       return true; // remove from result set
						       }
					       }
					       else if (filterby == "author")
					       {
						       if (elem.getAuthor ().find (filter) == std::string::npos)
						       {
							       return true; // remove from result set
						       }
					       }
					       else if (filterby == "tags")
					       {
						       std::vector<std::string> tags = elem.getTags ();
						       return std::find (tags.begin (), tags.end (), filter) == tags.end ();
					       }
					       else // filterby "all"
					       {
						       std::vector<std::string> tags = elem.getTags ();
						       if (elem.getPublicName ().find (filter) == std::string::npos &&
							   elem.getTitle ().find (filter) == std::string::npos &&
							   elem.getDescription ().find (filter) == std::string::npos &&
							   elem.getAuthor ().find (filter) == std::string::npos &&
							   std::find (tags.begin (), tags.end (), filter) == tags.end ())
						       {
							       return true; // remove from result set
						       }
					       }

					       return false; // seems search was successful, keep the element
				       }),
		       entries.end ());
}

/**
         * @brief Can be used to filter a user vector based on a `filter` (search string).
		 *
		 * Whether an entry remains in the list depends on the used `filter`. The searched
		 * field(s) can be specified with the `filterby` parameter, the search value itself
		 * in the `filter` value.
		 *
		 * Possible values for `filterby` are:
		 * - all
		 * - username
		 * - email
         *
         * Entries that do not have the `filter` value in the `filterby` field(s) will be removed.
         *
         * @param users The vector containing user entries to be filtered
         * @param filter The string to be searched for
		 * @param filterby The field(s) to search in
         */
void SearchEngine::findUsersByFilter (std::vector<kdbrest::model::User> & users, const std::string & filter, const std::string filterby)
{
	users.erase (std::remove_if (users.begin (), users.end (),
				     [filter, filterby](kdbrest::model::User & elem) -> bool {
					     if (filterby == "username")
					     {
						     if (elem.getUsername ().find (filter) == std::string::npos)
						     {
							     return true; // remove from result set
						     }
					     }
					     else if (filterby == "email")
					     {
						     if (elem.getEmail ().find (filter) == std::string::npos)
						     {
							     return true; // remove from result set
						     }
					     }
					     else // filterby "all"
					     {
						     if (elem.getUsername ().find (filter) == std::string::npos &&
							 elem.getEmail ().find (filter) == std::string::npos)
						     {
							     return true; // remove from result set
						     }
					     }

					     return false; // seems search was successful, keep the element
				     }),
		     users.end ());
}

} // namespace service

} // namespace kdbrest
