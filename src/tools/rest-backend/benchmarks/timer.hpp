#ifndef ELEKTRA_REST_BENCHMARK_TIMER_HEADER_GUARD
#define ELEKTRA_REST_BENCHMARK_TIMER_HEADER_GUARD

#include <chrono>
#include <iostream>

namespace kdbrest
{

namespace benchmark
{

class Timer
{
public:
	void start ()
	{
		m_start = std::chrono::high_resolution_clock::now ();
	}

	void stop ()
	{
		m_end = std::chrono::high_resolution_clock::now ();
	}

	long long getStartInSeconds ()
	{
		return std::chrono::time_point_cast<std::chrono::seconds> (m_start).time_since_epoch ().count ();
	}

	long long getEndInSeconds ()
	{
		return std::chrono::time_point_cast<std::chrono::seconds> (m_end).time_since_epoch ().count ();
	}

	long long getDurationInSeconds ()
	{
		return std::chrono::duration_cast<std::chrono::seconds> (m_end - m_start).count ();
	}

	long long getStartInMilliseconds ()
	{
		return std::chrono::time_point_cast<std::chrono::milliseconds> (m_start).time_since_epoch ().count ();
	}

	long long getEndInMilliseconds ()
	{
		return std::chrono::time_point_cast<std::chrono::milliseconds> (m_end).time_since_epoch ().count ();
	}

	long long getDurationInMilliseconds ()
	{
		return std::chrono::duration_cast<std::chrono::milliseconds> (m_end - m_start).count ();
	}

	long long getStartInMicroseconds ()
	{
		return std::chrono::time_point_cast<std::chrono::microseconds> (m_start).time_since_epoch ().count ();
	}

	long long getEndInMicroseconds ()
	{
		return std::chrono::time_point_cast<std::chrono::microseconds> (m_end).time_since_epoch ().count ();
	}

	long long getDurationInMicroseconds ()
	{
		return std::chrono::duration_cast<std::chrono::microseconds> (m_end - m_start).count ();
	}

	long long getStartInNanoseconds ()
	{
		return std::chrono::time_point_cast<std::chrono::nanoseconds> (m_start).time_since_epoch ().count ();
	}

	long long getEndInNanoseconds ()
	{
		return std::chrono::time_point_cast<std::chrono::nanoseconds> (m_end).time_since_epoch ().count ();
	}

	long long getDurationInNanoseconds ()
	{
		return std::chrono::duration_cast<std::chrono::nanoseconds> (m_end - m_start).count ();
	}

	void printStatistic (int indent_spaces = 0)
	{
		std::string spaces = "";
		for (int i = 0; i < indent_spaces; i++)
		{
			spaces += " ";
		}

		std::cout << spaces << "-> Start time: " << this->getStartInNanoseconds () << std::endl;
		std::cout << spaces << "-> End time: " << this->getEndInNanoseconds () << std::endl;
		std::cout << spaces << "-> Duration (nano) : " << this->getDurationInNanoseconds () << std::endl;
		std::cout << spaces << "-> Duration (micro): " << this->getDurationInMicroseconds () << std::endl;
		std::cout << spaces << "-> Duration (milli): " << this->getDurationInMilliseconds () << std::endl;
		std::cout << spaces << "-> Duration (sec)  : " << this->getDurationInSeconds () << std::endl;
	}

private:
	std::chrono::high_resolution_clock::time_point m_start;
	std::chrono::high_resolution_clock::time_point m_end;
};
}
}

#endif
