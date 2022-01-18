/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDBTIMER_HPP
#define ELEKTRA_KDBTIMER_HPP

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <sys/time.h>
#include <vector>

#ifdef __GNUC__
#define TIMER_NOINLINE __attribute__ ((noinline))
#endif

class Timer
{
public:
	// options what to do at cleanup
	enum option_t
	{
		raw_data_cerr, ///< print name,data\n (default!)
		median_cerr,   ///< print name,median\n
		quiet,	       ///< print nothing
	};

	// functions
	TIMER_NOINLINE void start ()
	{
		gettimeofday (&begin, nullptr);
		// clock_gettime(CLOCK_MONOTONIC, &begin);
	}
	TIMER_NOINLINE void stop ()
	{
		gettimeofday (&end, nullptr);
		// clock_gettime(CLOCK_MONOTONIC, &end);

		// force calculation in long long:
		// could use timersub here
		timer_t result = end.tv_sec - begin.tv_sec;
		result *= usec_factor;
		result += end.tv_usec - begin.tv_usec;
		results.push_back (result);
	}
	std::string getMedian () const;
	Timer (std::string name, option_t option = raw_data_cerr);
	~Timer (); // print csv table at end

	// data
	struct timeval begin;
	struct timeval end;
	typedef long long timer_t;
	typedef std::vector<timer_t> results_t;
	results_t results;
	std::string name;
	option_t option;
	static const timer_t usec_factor = 1000000LL;
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
				 << result / Timer::usec_factor
				 << "."
				 << std::setw(6)
				 << std::setfill('0')
				 << result % Timer::usec_factor
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
	Timer::timer_t r = *(md.begin () + md.size () / 2);
	std::ostringstream os;
	os << r / Timer::usec_factor << "." << std::setw (6) << std::setfill ('0') << r % Timer::usec_factor;
	return os.str ();
}


inline std::ostream & operator<< (std::ostream & os, Timer const & t)
{
	// clang-format off
	Timer::timer_t r = t.results.back();
	os.width(30);
	os.fill(' ');
	os << t.name << "\t";
	os  << r / Timer::usec_factor
		<< "."
		<< std::setw(6)
		<< std::setfill('0')
		<< r % Timer::usec_factor
		<< " sec";
	r = std::accumulate(t.results.begin(), t.results.end(), 0LL);
	os  << "\t Sum: "
		<< r / Timer::usec_factor
		<< "."
		<< std::setw(6)
		<< std::setfill('0')
		<< r % Timer::usec_factor
		<< " sec";
	r = r / t.results.size();
	os  << "\tAvg: "
		<< r / Timer::usec_factor
		<< "."
		<< std::setw(6)
		<< std::setfill('0')
		<< r % Timer::usec_factor
		<< " sec";
	r = *std::min_element(t.results.begin(), t.results.end());
	os  << "\tMin: "
		<< r / Timer::usec_factor
		<< "."
		<< std::setw(6)
		<< std::setfill('0')
		<< r % Timer::usec_factor
		<< " sec";
	r = *std::max_element(t.results.begin(), t.results.end());
	os  << "\tMax: "
		<< r / Timer::usec_factor
		<< "."
		<< std::setw(6)
		<< std::setfill('0')
		<< r % Timer::usec_factor
		<< " sec";
	os  << "\tMedian: "
		<< t.getMedian()
		<< " sec"
		<< std::endl;
	// clang-format on
	return os;
}

#endif
