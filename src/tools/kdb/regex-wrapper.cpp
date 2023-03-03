#include <memory>

// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=105562
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

#include <regex>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif

#include <unordered_map>

// Map to hold the regex objects. The key will be a handle (a unique identifier).
std::unordered_map<int, std::shared_ptr<std::regex>> regex_map;
int next_handle = 1;

extern "C" int regcompWrapper (int * preg, const char * pattern)
{
	try
	{
		// Use make_shared to allocate the std::regex. Ignore cflags for simplicity.
		std::shared_ptr<std::regex> re = std::make_shared<std::regex> (pattern);

		// Assign a handle (next_handle), then increment next_handle for next time.
		int handle = next_handle++;
		regex_map[handle] = re;

		// Assign the handle to *preg, so the caller can use it later.
		*preg = handle;

		// Return 0 for success.
		return 0;
	}
	catch (const std::regex_error & e)
	{
		// Return a non-zero error code if regex compilation failed.
		return 1;
	}
}

extern "C" int regexecWrapper (int handle, const char * str)
{
	// Find the std::regex object in the map.
	auto iter = regex_map.find (handle);
	if (iter == regex_map.end ())
	{
		// If the handle isn't in the map, return an error code.
		return 1;
	}

	std::shared_ptr<std::regex> re = iter->second;

	// Use std::regex_match to perform the matching.
	if (std::regex_search (str, *re))
	{
		return 0; // Match
	}
	else
	{
		return 1; // No match
	}
}

extern "C" void regfreeWrapper (int preg)
{
	// Remove the std::regex object from the map.
	regex_map.erase (preg);
}