#ifndef ELEKTRA_REST_USERAPP_HEADER_GUARD
#define ELEKTRA_REST_USERAPP_HEADER_GUARD

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <string>
#include <vector>

#include <model_user.hpp>

/**
 * @brief This is the main namespace for all classes belonging
 * to the kdb rest service
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

/**
     * @brief User endpoint class, serves endpoint for user management
     */
class UserApp : public cppcms::application
{

public:
	UserApp (cppcms::service & srv);

	virtual void handle (std::string username);

private:
	void handleGet (cppcms::http::request & request, cppcms::http::response & response);
	void handleGetUnique (cppcms::http::response & response, std::string username = std::string ());
	void handleInsert (cppcms::http::request & request, cppcms::http::response & response);
	void handleUpdate (cppcms::http::request & request, cppcms::http::response & response, std::string username = std::string (),
			   bool canSetRank = false);
	void handleDelete (cppcms::http::response & response, std::string username = std::string ());

	inline void produceOutput (cppcms::http::request & request, cppcms::http::response & response, std::vector<model::User> & users);
	inline void processFiltering (cppcms::http::request & request, std::vector<model::User> & users);
	inline void processSorting (cppcms::http::request & request, std::vector<model::User> & users);
	inline int getMaxrows (cppcms::http::request & request);
	inline int getOffset (cppcms::http::request & request);

	bool isValidEmail (std::string & email);
};

} // namespace kdbrest

#endif
