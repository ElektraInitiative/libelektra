/**
 * @file
 *
 * @brief tests for the configformat model
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <regex>

#include <gtest/gtest.h>

#include <crypto.hpp>
#include <user_application.hpp>

/**
 * TESTS for kdbrest::models::ConfigFormat
 */

TEST (kdbrestRegex, UserPassword)
{
	// should match:
	// - min 1 capital letter
	// - min 1 sneak letter
	// - min 1 digit
	// - min 8 characters in total
	std::regex regex (kdbrest::REGEX_PASSWORD);

	ASSERT_TRUE (std::regex_match ("S0mePassword", regex));
	ASSERT_TRUE (std::regex_match ("Blubblub123", regex));
	ASSERT_TRUE (std::regex_match ("NoPr0blem0__", regex));
	ASSERT_TRUE (std::regex_match ("Works#Properly1", regex));
	ASSERT_TRUE (std::regex_match ("Llength8", regex));
	ASSERT_TRUE (std::regex_match ("098NumberFirst", regex));
	ASSERT_TRUE (std::regex_match ("098__SpecialC", regex));
	ASSERT_TRUE (std::regex_match ("aaaBBBcccDDD1", regex));
	ASSERT_TRUE (std::regex_match ("ab123laSölß/23.", regex));
	ASSERT_TRUE (std::regex_match ("NotSecure1", regex));

	ASSERT_FALSE (std::regex_match ("NoNumber", regex));
	ASSERT_FALSE (std::regex_match ("nocapitalletter1", regex));
	ASSERT_FALSE (std::regex_match ("NOSMALLLETTER2", regex));
	ASSERT_FALSE (std::regex_match ("ONLYCAPITAL", regex));
	ASSERT_FALSE (std::regex_match ("onlysmall", regex));
	ASSERT_FALSE (std::regex_match ("123098120938", regex));
	ASSERT_FALSE (std::regex_match ("as#sdkasl.-sd", regex));
	ASSERT_FALSE (std::regex_match ("90lsjd____2öfbß", regex));
}

TEST (kdbrestCrypto, Sha256Encrypt)
{
	using namespace kdbrest::crypto;

	std::string s1 ("UpXGcXMabG");
	std::string se1 ("535dacf7eb5f1d8a0560513c759f74ecd3c03069c034fff3d746718c1cc8915c");

	std::string s2 ("IsY6ehAjz0");
	std::string se2 ("d43df26107bf9b7c53b027b51697c35c80639156c75a057542e40b0af73011af");

	std::string s3 ("ufumoI2SVS");
	std::string se3 ("ad7e5ffa85d63946cba6080e605a6400ab605e57423c1aba6c824130f31f4856");

	std::string s4 ("cVkAxMu9VS");
	std::string se4 ("4aa4d3d3ad09dcbc566f94357cea8b80efa7f8e3d1b08965f4588699e2df4229");

	std::string s5 ("FsZH8xRR4k");
	std::string se5 ("e86ac77fe0af79e7a2c860971c32c55f99b5b7c4124a021acc97adf84f39bfc3");

	std::string s6 ("PX_,.#ß0sö!32");
	std::string se6 ("45343999f6ec371c3d627deaf88ab8c0996caf1fc67d04bb4799c5f97753febf");

	std::string s7 ("__dm12398#ößSD");
	std::string se7 ("f8db496850fbeda234433738a507e068714268d17d3adf530e63dd52476ac3af");

	std::string s8 ("ölsdkö1230_ASD");
	std::string se8 ("b692683abff05044b708d9118c326cbd97488ec1a181998e20aed3dc73b5bc81");

	std::string s9 ("EgzshrCYNb");
	std::string se9 ("95ae67490bd602773eb2e79949a21eb2b89151e491f2aea06be2f39919118185");

	std::string s10 ("9bFbDSDDUE");
	std::string se10 ("9810fd3210a5ff6a4b1a5637f129b32da0a5e05253eec1177c2683dd65688fd4");

	std::string so1;
	ASSERT_TRUE (sha256_encrypt (s1, so1));
	ASSERT_EQ (se1, so1);

	std::string so2;
	ASSERT_TRUE (sha256_encrypt (s2, so2));
	ASSERT_EQ (se2, so2);

	std::string so3;
	ASSERT_TRUE (sha256_encrypt (s3, so3));
	ASSERT_EQ (se3, so3);

	std::string so4;
	ASSERT_TRUE (sha256_encrypt (s4, so4));
	ASSERT_EQ (se4, so4);

	std::string so5;
	ASSERT_TRUE (sha256_encrypt (s5, so5));
	ASSERT_EQ (se5, so5);

	std::string so6;
	ASSERT_TRUE (sha256_encrypt (s6, so6));
	ASSERT_EQ (se6, so6);

	std::string so7;
	ASSERT_TRUE (sha256_encrypt (s7, so7));
	ASSERT_EQ (se7, so7);

	std::string so8;
	ASSERT_TRUE (sha256_encrypt (s8, so8));
	ASSERT_EQ (se8, so8);

	std::string so9;
	ASSERT_TRUE (sha256_encrypt (s9, so9));
	ASSERT_EQ (se9, so9);

	std::string so10;
	ASSERT_TRUE (sha256_encrypt (s10, so10));
	ASSERT_EQ (se10, so10);
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
