import kdb
import unittest

from unittest.mock import patch
from ..main import dns_plugin
from ..main.dns_plugin import socket, get_ipv4_by_hostname, check_key

VALID_DOMAIN = "libelektra.org"
INVALID_DOMAIN = "www.does-not.exist"
VALID_IP = "95.217.75.163"
VALID_ADDR_INFO = [(socket.AddressFamily.AF_INET, socket.SocketKind.SOCK_STREAM, 6, '', (VALID_IP, 0)),
                   (socket.AddressFamily.AF_INET, socket.SocketKind.SOCK_DGRAM, 17, '', (VALID_IP, 0)),
                   (socket.AddressFamily.AF_INET, socket.SocketKind.SOCK_RAW, 0, '', (VALID_IP, 0))]


class DNSPlugin(unittest.TestCase):

    def setUp(self):
        self.parent_key = kdb.Key("user:/python")
        self.plugin = dns_plugin.ElektraPlugin()
        self.localhost_key_with_plugin = kdb.Key("user:/python/hostname",
                                             kdb.KEY_VALUE, "localhost",
                                             kdb.KEY_META, "check/dns", "")
        self.valid_key_with_plugin = kdb.Key("user:/python/hostname",
                                             kdb.KEY_VALUE, VALID_DOMAIN,
                                             kdb.KEY_META, "check/dns", "")
        self.invalid_key_with_plugin = kdb.Key("user:/python/hostname",
                                               kdb.KEY_VALUE, INVALID_DOMAIN,
                                               kdb.KEY_META, "check/dns", "")
        self.valid_key_without_plugin = kdb.Key("user:/foo/bar", kdb.KEY_VALUE, "val")

        self.invalid_ks = kdb.KeySet(10, self.invalid_key_with_plugin, self.valid_key_without_plugin, kdb.KS_END)
        self.valid_ks = kdb.KeySet(10, self.valid_key_with_plugin, self.valid_key_without_plugin, kdb.KS_END)
        self.localhost_ks = kdb.KeySet(10, self.localhost_key_with_plugin, kdb.KS_END)

    # socket.getaddrinfo is mocked in order to make the tests non-reliant on an active internet connection
    @patch.object(socket, 'getaddrinfo', return_value=VALID_ADDR_INFO)
    def test_get_ipv4_from_hostname_exists(self, mock_socket):
        self.assertTrue(get_ipv4_by_hostname(hostname=VALID_DOMAIN))

    @patch.object(socket, 'getaddrinfo', side_effect=Exception)
    def test_get_ipv4_from_hostname_does_not_exist(self, mock_socket):
        with self.assertRaises(Exception):
            get_ipv4_by_hostname(hostname=INVALID_DOMAIN)

    @patch.object(socket, 'getaddrinfo', return_value=VALID_ADDR_INFO)
    def test_check_valid_key_meta_is_set_returns_true(self, mock_socket):
        self.assertTrue(check_key(self.valid_key_with_plugin))

    @patch.object(socket, 'getaddrinfo', side_effect=Exception)
    def test_check_invalid_key_meta_is_set_returns_false(self, mock_socket):
        self.assertFalse(check_key(self.invalid_key_with_plugin))

    def test_check_key_meta_is_not_set_returns_false(self):
        self.assertTrue(check_key(self.valid_key_without_plugin))

    @patch.object(socket, 'getaddrinfo', side_effect=Exception)
    def test_set_containing_invalid_key_returns_failure(self, mock_socket):
        self.assertEqual(-1, self.plugin.set(self.invalid_ks, self.parent_key))

    @patch.object(socket, 'getaddrinfo', return_value=VALID_ADDR_INFO)
    def test_set_containing_valid_key_returns_success(self, mock_socket):
        self.assertEqual(1, self.plugin.set(self.valid_ks, self.parent_key))

    @patch.object(socket, 'getaddrinfo', side_effect=Exception)
    def test_get_containing_invalid_key_returns_failure(self, mock_socket):
        self.assertEqual(-1, self.plugin.get(self.invalid_ks, self.parent_key))

    @patch.object(socket, 'getaddrinfo', return_value=VALID_ADDR_INFO)
    def test_get_containing_valid_key_returns_success(self, mock_socket):
        self.assertEqual(1, self.plugin.get(self.valid_ks, self.parent_key))

    def test_get_containing_localhost_returns_success(self):
        self.assertEqual(1, self.plugin.get(self.localhost_ks, self.parent_key))

    def test_set_containing_localhost_returns_success(self):
        self.assertEqual(1, self.plugin.get(self.localhost_ks, self.parent_key))


if __name__ == '__main__':
    unittest.main()
