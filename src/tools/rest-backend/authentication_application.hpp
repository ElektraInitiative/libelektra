#ifndef ELEKTRA_REST_AUTHENTICATIONAPP_HEADER_GUARD
#define ELEKTRA_REST_AUTHENTICATIONAPP_HEADER_GUARD

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <string>

#include <model_user.hpp>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

// STATIC CONSTANTS

// parameter for a session token as alternative to the authorization header
static const std::string PARAM_TOKEN = "token";
// prefix for the value of the authorization header, e.g.: Authorization: Bearer <token>
static const std::string AUTH_HEADER_PREFIX = "Bearer ";

/**
 * @brief serves endpoint for authentication management
 */
class AuthenticationApp : public cppcms::application
{

public:
	AuthenticationApp (cppcms::service & srv);

	virtual void authenticate ();

	static bool validateAuthentication (cppcms::http::request & request, cppcms::http::response & response, const int rank = 0,
					    const std::string orUser = std::string ());

	static model::User getCurrentUser (cppcms::http::request & request);

private:
};

} // namespace kdbrest

#endif
