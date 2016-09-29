#ifndef JWT_HEADER_GUARD
#define JWT_HEADER_GUARD
#include <map>
#include <string>

namespace jwt
{

/* CONSTANTS */
const std::string INDEX_ALGORITHM = "alg";
const std::string INDEX_TYPE = "typ";
const std::string INDEX_ISSUER = "iss";
const std::string INDEX_EXP_DATE = "exp";

/* ENUMS */
enum JWTAlgorithm
{
	HS256
};

enum AuthType
{
	JWT
};

static std::map<JWTAlgorithm, std::string> JWTAlgorithms = { { JWTAlgorithm::HS256, "HS256" } };
static std::map<std::string, JWTAlgorithm> JWTAlgorithm_inverse = { { "HS256", JWTAlgorithm::HS256 } };

static std::map<AuthType, std::string> AuthTypes = { { AuthType::JWT, "JWT" } };
static std::map<std::string, AuthType> AuthTypes_inverse = { { "JWT", AuthType::JWT } };


/* STRUCTS */
typedef struct
{
	JWTAlgorithm algorithm;
	AuthType type;
} header;

typedef struct
{
	std::string issuer;
	long expiration_date;
	std::map<std::string, std::string> claims;
} payload;

typedef struct
{
	header s_header;
	payload s_payload;
} jwt;


/* CLASSES */
class JWTBuilder
{

public:
	JWTBuilder ()
	{
	} // default constructor

	// setter
	JWTBuilder & setAlgorithm (JWTAlgorithm alg)
	{
		s_jwt.s_header.algorithm = alg;
		return *this;
	}
	JWTBuilder & setAuthType (AuthType typ)
	{
		s_jwt.s_header.type = typ;
		return *this;
	}

	JWTBuilder & setIssuer (std::string iss)
	{
		s_jwt.s_payload.issuer = iss;
		return *this;
	}
	JWTBuilder & setExpirationDate (long exp_date)
	{
		s_jwt.s_payload.expiration_date = exp_date;
		return *this;
	}
	JWTBuilder & setClaim (std::string claim, std::string value)
	{
		s_jwt.s_payload.claims[claim] = value;
		return *this;
	}

	// token getter
	std::string generateToken (std::string crypt_key);

private:
	jwt s_jwt;

	std::string generateToken ();
};

class JWTValidator
{

public:
	JWTValidator ()
	{
	}

	// setter
	JWTValidator & setToken (std::string tk)
	{
		token = tk;
		return *this;
	};
	JWTValidator & setIssuer (std::string iss)
	{
		s_jwt.s_payload.issuer = iss;
		return *this;
	}
	JWTValidator & setClaim (std::string claim, std::string value)
	{
		s_jwt.s_payload.claims[claim] = value;
		return *this;
	}

	// getter
	jwt getParsedJwt ();

	// validate token
	bool validate (std::string crypt_key);

private:
	jwt s_jwt;
	std::string token;
};

namespace exception
{

class JWTBuildException : public std::exception
{
public:
	virtual const char * what () const throw ()
	{
		return "The JWT generator encountered an error during JWT generation, most likely because he was not correctly "
		       "initialized.";
	}
};

class JWTValidationException : public std::exception
{
public:
	virtual const char * what () const throw ()
	{
		return "The JWT validator encountered an error during JWT validation, most likely because he was not correctly "
		       "initialized.";
	}
};

class JWTParseException : public std::exception
{
public:
	virtual const char * what () const throw ()
	{
		return "The JWT validator encountered an error during JWT parsing, most likely because he was not correctly initialized.";
	}
};

} // namespace exception

} // namespace jwt

namespace cppcms
{

namespace json
{

using namespace ::jwt;

template <>
struct traits<header>
{
	static header get (value const & v)
	{
		if (v.type () != is_object) throw bad_value_cast ();

		header header;
		header.algorithm = JWTAlgorithm_inverse[v.get<std::string> (INDEX_ALGORITHM)];
		header.type = AuthTypes_inverse[v.get<std::string> (INDEX_TYPE)];
		return header;
	}

	static void set (value & v, header const & in)
	{

		v.set (INDEX_ALGORITHM, JWTAlgorithms[in.algorithm]);
		v.set (INDEX_TYPE, AuthTypes[in.type]);
	}
};

template <>
struct traits<payload>
{
	static payload get (value const & v)
	{
		if (v.type () != is_object) throw bad_value_cast ();

		payload payload;
		payload.issuer = v.get<std::string> (INDEX_ISSUER);
		payload.expiration_date = v.get<long> (INDEX_EXP_DATE);
		for (auto elem : v.object ())
		{
			if (!elem.first.str ().compare (INDEX_ISSUER) == 0 && !elem.first.str ().compare (INDEX_EXP_DATE) == 0)
			{
				payload.claims[elem.first] = elem.second.str ();
			}
		}
		return payload;
	}

	static void set (value & v, payload const & in)
	{
		v.set (INDEX_ISSUER, in.issuer);
		v.set (INDEX_EXP_DATE, in.expiration_date);
		for (auto elem : in.claims)
		{
			v.set (elem.first, elem.second);
		}
	}
};

} // namespace json

} // namespace cppcms

#endif
