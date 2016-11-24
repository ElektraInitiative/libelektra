#ifndef ELEKTRA_REST_TEMPLATE_SINGLETON_GUARD
#define ELEKTRA_REST_TEMPLATE_SINGLETON_GUARD

template <typename T>
class singleton
{
public:
	static T & instance ()
	{
		static T instance;
		return instance;
	}

protected:
	singleton ()
	{
	}
};

#endif
