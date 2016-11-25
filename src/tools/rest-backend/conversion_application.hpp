#ifndef ELEKTRA_REST_CONVERSIONAPP_HEADER_GUARD
#define ELEKTRA_REST_CONVERSIONAPP_HEADER_GUARD

#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/http_request.h>
#include <cppcms/http_response.h>
#include <cppcms/service.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <string>
#include <vector>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

// STATIC CONSTANTS

// index for the input format in requests
static std::string INDEX_INPUT_FORMAT = "input.format";
// index for the input value (snippet) in requests
static std::string INDEX_INPUT_VALUE = "input.snippet";
// index for the output format in requests
static std::string INDEX_OUTPUT_FORMAT = "output.format";

/**
 * @brief serves endpoint for snippet conversion
 */
class ConversionApp : public cppcms::application
{

public:
	ConversionApp (cppcms::service & srv);

	virtual void convert ();
	virtual void formats ();

private:
};

} // namespace kdbrest

#endif
