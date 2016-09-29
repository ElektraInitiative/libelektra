#include <ctime>
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/cstdint.hpp>
#include <cppcms/json.h>

#include "cryptlite/base64.h"
#include "cryptlite/hmac.h"
#include "cryptlite/sha256.h"

#include "jwt.hpp"

namespace jwt
{

/* GENERATOR */

std::string JWTBuilder::generateToken ()
{

	using namespace cryptlite;

	try
	{
		// construct jwt
		std::string str_header;
		std::string str_payload;

		std::stringstream ss;
		cppcms::json::value v_header;
		v_header = s_jwt.s_header;
		ss << v_header.save ();
		str_header = base64::encode_from_string (ss.str ());

		ss.str ("");
		cppcms::json::value v_payload;
		v_payload = s_jwt.s_payload;
		ss << v_payload.save ();
		str_payload = base64::encode_from_string (ss.str ());

		ss.str ("");
		ss << str_header << "." << str_payload;

		return ss.str ();
	}
	catch (const std::exception & e)
	{
		throw exception::JWTBuildException ();
	}
}

std::string JWTBuilder::generateToken (std::string crypt_key)
{

	using namespace cryptlite;

	try
	{
		// generate token without encryption
		std::string pre_token = this->generateToken ();

		std::string str_encoded;
		switch (s_jwt.s_header.algorithm)
		{
		case JWTAlgorithm::HS256:
		default:
			boost::uint8_t hs256digest[sha256::HASH_SIZE];
			hmac<sha256>::calc (pre_token, crypt_key, hs256digest);
			str_encoded = base64::encode_from_array (hs256digest, sha256::HASH_SIZE);
			break;
		}

		// serialize jwt
		std::stringstream ss;
		ss << pre_token << "." << str_encoded;
		return ss.str ();
	}
	catch (const std::exception & e)
	{
		throw exception::JWTBuildException ();
	}
}


/* VALIDATOR */

jwt JWTValidator::getParsedJwt ()
{

	using namespace cryptlite;

	if (token.empty ())
	{
		throw exception::JWTParseException ();
	}

	// split token
	std::vector<std::string> parts;
	boost::split (parts, token, boost::is_any_of ("."), boost::token_compress_on);

	// invalid token format
	if (parts.size () != 3)
	{
		throw exception::JWTParseException ();
	}

	// base64 decode header & payload
	std::string str_header;
	std::string str_payload;
	base64::decode (parts.at (0), str_header);
	base64::decode (parts.at (1), str_payload);

	// parse string to structs
	cppcms::json::value v_header;
	cppcms::json::value v_payload;
	std::stringstream ss;
	ss << str_header;
	ss >> v_header;
	ss.str ("");
	ss << str_payload;
	ss >> v_payload;
	header s_header = v_header.get_value<header> ();
	payload s_payload = v_payload.get_value<payload> ();

	jwt result_jwt;
	result_jwt.s_header = s_header;
	result_jwt.s_payload = s_payload;

	return result_jwt;
}

bool JWTValidator::validate (std::string crypt_key)
{

	using namespace cryptlite;

	try
	{

		// validator preparation checks
		if (token.empty ())
		{
			return false;
		} // no token set

		// split token
		std::vector<std::string> parts;
		boost::split (parts, token, boost::is_any_of ("."), boost::token_compress_on);

		if (parts.size () != 3)
		{
			return false;
		} // invalid token format

		// base64 decode header & payload
		std::string str_header;
		std::string str_payload;
		base64::decode (parts.at (0), str_header);
		base64::decode (parts.at (1), str_payload);

		// parse string to structs
		cppcms::json::value v_header;
		cppcms::json::value v_payload;
		std::stringstream ss;
		ss << str_header;
		ss >> v_header;
		ss.str ("");
		ss << str_payload;
		ss >> v_payload;
		header s_header = v_header.get_value<header> ();
		payload s_payload = v_payload.get_value<payload> ();

		// verify encryption
		ss.str ("");
		ss << parts.at (0) << "." << parts.at (1);
		std::string str_encoded;
		switch (s_header.algorithm)
		{
		case JWTAlgorithm::HS256:
		default:
			boost::uint8_t hs256digest[sha256::HASH_SIZE];
			hmac<sha256>::calc (ss.str (), crypt_key, hs256digest);
			str_encoded = base64::encode_from_array (hs256digest, sha256::HASH_SIZE);
			break;
		}
		if (str_encoded.compare (parts.at (2)) != 0)
		{
			return false;
		} // verification failed

		// check expiration date
		if (s_payload.expiration_date < std::time (NULL))
		{
			return false;
		} // token expired

		// check issuer
		if (s_payload.issuer.compare (s_jwt.s_payload.issuer) != 0)
		{
			return false;
		} // wrong issuer

		// check custom claims
		for (auto & elem : s_jwt.s_payload.claims)
		{
			auto iter = s_payload.claims.find (elem.first);
			if (iter == s_jwt.s_payload.claims.end ())
			{
				return false;
			} // claim missing
			if (iter->second.compare (elem.second) != 0)
			{
				return false;
			} // claim wrong
		}

		// survived all checks, token is valid
		return true;
	}
	catch (const std::exception & e)
	{
		throw exception::JWTValidationException ();
	}
}

} // namespace jwt
