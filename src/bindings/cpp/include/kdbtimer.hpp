/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <string>
#include <sys/time.h>

#ifdef __GNUC__
#define TIMER_NOINLINE __attribute__ ((noinline))
#endif

class Timer
{
public:
	TIMER_NOINLINE void start () { gettimeofday (&begin, nullptr); }
	TIMER_NOINLINE void stop ()
	{
		gettimeofday (&end, nullptr);
		// force calculation in long long:
		timer_t result = end.tv_sec - begin.tv_sec;
		result *= usec_factor;
		result += end.tv_usec - begin.tv_usec;
		results.push_back (result);
	}
	Timer (std::string name);
	~Timer (); // print csv table at end
	struct timeval begin;
	struct timeval end;
	typedef long long timer_t;
	typedef std::vector<timer_t> results_t;
	results_t results;
	std::string name;
	static const timer_t usec_factor = 1000000LL;
};

inline Timer::Timer(std::string  name_) :
		begin(),
		end(),
		results(),
		name(std::move(name_))
{
}

inline Timer::~Timer()
{
	for (auto result : results)
	{
		std::cerr << name << ","
			 << result / Timer::usec_factor
			 << "."
			 << std::setw(6)
			 << std::setfill('0')
			 << result % Timer::usec_factor
			 << std::endl;
	}
}


inline std::ostream & operator<< (std::ostream & os, Timer const & t)
{
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
	Timer::results_t md = t.results;
	nth_element(md.begin(),
				md.begin()+md.size()/2, 
				md.end());
	r=*(md.begin()+md.size()/2);
	os  << "\tMedian: "
		<< r / Timer::usec_factor
		<< "."
		<< std::setw(6)
		<< std::setfill('0')
		<< r % Timer::usec_factor
		<< " sec"
		<< std::endl;
	return os;
}

