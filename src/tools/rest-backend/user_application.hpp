/**
 * @file
 *
 * @brief header for cppcms controller managing user resources
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_USERAPP_HPP
#define ELEKTRA_REST_USERAPP_HPP

#include <map>
#include <string>
#include <vector>

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>

#include <model_user.hpp>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

// STATIC CONSTANTS

// parameter that can be used to operate on the currently authenticated user
static std::string PARAM_CURRENT = "current";

// index to be used for username in requests
static std::string INDEX_USERNAME = "username";
// index to be used for password in requests
static std::string INDEX_PASSWORD = "password";
// index to be used for email in requests
static std::string INDEX_EMAIL = "email";
// index to be used for rank in requests
static std::string INDEX_RANK = "rank";

// regex to be used to validate usernames
static std::string REGEX_USERNAME = "[a-zA-Z0-9\\-\\.]{3,20}";
// regex to be used to validate passwords
static std::string REGEX_PASSWORD = "(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9]).{8,}";

// map from sort type to function
static std::map<std::string, bool (*) (model::User &, model::User &)> SORT_USER_MAP = { { "username", &model::User::less_than_username },
											{ "email", &model::User::less_than_email },
											{ "rank", &model::User::less_than_rank },
											{ "created_at",
											  &model::User::less_than_created_at } };

/**
 * @brief User endpoint class, serves endpoint for user management
 */
class UserApp : public cppcms::application
{

public:
	UserApp (cppcms::service & srv);

	virtual void handle (std::string username);

private:
	void handleGet (cppcms::http::request & request, cppcms::http::response & response) const;
	void handleGetUnique (cppcms::http::response & response, const std::string username = std::string ()) const;
	void handleInsert (cppcms::http::request & request, cppcms::http::response & response) const;
	void handleUpdate (cppcms::http::request & request, cppcms::http::response & response, const std::string username = std::string (),
			   const bool canSetRank = false) const;
	void handleDelete (cppcms::http::response & response, const std::string username = std::string ()) const;

	inline void processFiltering (cppcms::http::request & request, std::vector<model::User> & users) const;
	inline void processSorting (cppcms::http::request & request, std::vector<model::User> & users) const;
	inline int getMaxrows (cppcms::http::request & request) const;
	inline int getOffset (cppcms::http::request & request) const;

	void generateAndSendUserList (cppcms::http::request & request, cppcms::http::response & response,
				      const std::vector<model::User> & users) const;

	bool isValidEmail (const std::string & email) const;
};

} // namespace kdbrest

#endif
