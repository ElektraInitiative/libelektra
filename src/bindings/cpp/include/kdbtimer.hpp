/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>
#include <chrono>

#ifdef __GNUC__
#define TIMER_NOINLINE __attribute__ ((noinline))
#elif _MSC_VER
#define TIMER_NOINLINE __declspec (noinline)
#endif

class Timer
{
public:
	// options what to do at cleanup
	enum option_t
	{
		raw_data_cerr, ///< print name,data\n (default!)
		median_cerr,   ///< print name,median\n
		quiet,	 ///< print nothing
	};

	// functions
	TIMER_NOINLINE void start ()
	{
		begin = std::chrono::system_clock::now();
	}
	TIMER_NOINLINE void stop ()
	{
		end = std::chrono::system_clock::now();

		const std::chrono::duration<double> result = end - begin;
		results.push_back (result);
	}
	std::string getMedian () const;
	Timer (std::string name, option_t option = raw_data_cerr);
	~Timer (); // print csv table at end

	// data
	std::chrono::system_clock::time_point begin;
	std::chrono::system_clock::time_point end;
	typedef std::vector<std::chrono::duration<double>> results_t;
	results_t results;
	std::string name;
	option_t option;
};

inline Timer::Timer (std::string name_, option_t option_) : begin (), end (), results (), name (std::move (name_)), option (option_)
{
}

inline Timer::~Timer ()
{
	switch (option)
	{
	case raw_data_cerr:
		for (auto result : results)
		{
			// clang-format off
			std::cerr << name << ","
				 << std::setprecision(6)
				 << result.count()
				 << std::endl;
			// clang-format on
		}
		break;
	case median_cerr:
		std::cerr << name << "," << getMedian () << std::endl;
		break;
	case quiet:
		break;
	}
}

std::string Timer::getMedian () const
{
	Timer::results_t md = results;
	nth_element (md.begin (), md.begin () + md.size () / 2, md.end ());
	auto r = *(md.begin () + md.size () / 2);
	std::ostringstream os;
	os << std::setprecision (6) << r.count() ;
	return os.str ();
}


inline std::ostream & operator<< (std::ostream & os, Timer const & t)
{
	// clang-format off
	auto r = t.results.back();
	os.width(30);
	os.fill(' ');
	os << t.name << "\t";
	os  << std::setprecision(6)
		<< r.count()
		<< " sec";
	r = std::accumulate(t.results.begin(), t.results.end(), std::chrono::duration<double>(0));
	os  << "\t Sum: "
		<< std::setprecision(6)
		<< r.count()
		<< " sec";
	r = r / t.results.size();
	os  << "\tAvg: "
		<< std::setprecision(6)
		<< r.count()
		<< " sec";
	r = *std::min_element(t.results.begin(), t.results.end());
	os  << "\tMin: "
		<< std::setprecision(6)
		<< r.count()
		<< " sec";
	r = *std::max_element(t.results.begin(), t.results.end());
	os  << "\tMax: "
		<< std::setprecision(6)
		<< r.count()
		<< " sec";
	os  << "\tMedian: "
		<< t.getMedian()
		<< " sec"
		<< std::endl;
	// clang-format on
	return os;
}
