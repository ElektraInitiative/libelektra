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

#include "model_user.hpp"


/**
 * @brief This is the main namespace for all classes belonging
 * to the kdb rest service
 */
namespace kdbrest
{
static std::string INDEX_TOKEN = "token";

static std::string AUTH_HEADER_PREFIX = "Bearer ";

/**
     * @brief Authentication class, serves endpoint for authentication
     * management
     */
class AuthenticationApp : public cppcms::application
{

public:
	AuthenticationApp (cppcms::service & srv);

	virtual void authenticate ();

	static bool validateAuthentication (cppcms::http::request & request, cppcms::http::response & response, int rank = 0,
					    std::string orUser = std::string ());

	static model::User getCurrentUser (cppcms::http::request & request);

private:
};

} // namespace kdbrest

#endif
