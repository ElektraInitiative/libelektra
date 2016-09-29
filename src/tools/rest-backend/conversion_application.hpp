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
 * @brief This is the main namespace for all classes belonging
 * to the kdb rest service
 */
namespace kdbrest
{

// STATIC CONSTANTS
static std::string INDEX_INPUT_FORMAT = "input.format";
static std::string INDEX_INPUT_VALUE = "input.snippet";
static std::string INDEX_OUTPUT_FORMAT = "output.format";

/**
     * @brief Conversion endpoint class, serves endpoint for snippet conversion
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
