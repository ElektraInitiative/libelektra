#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for gopts
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdb'
require 'test/unit'
require_relative 'test_helper.rb'

# basename for all keys written to a configuration used in the following
# test cases
RB_TEST_NS = "/tests/ruby/gopts"

SPEC = Kdb::KeySet.new [
      Kdb::Key.new("spec:#{RB_TEST_NS}", command: ""),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/printversion",
			  description: "print version information and exit (ignoring all other options/commands/parameters)",
			  opt: "v",
			  "opt/arg": "none",
			  "opt/long": "version"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/getter",
			  description: "get a key's value",
			  command: "get"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/getter/verbose",
			  description: "print additional information about where the value comes from",
			  opt: "v",
			  "opt/long": "verbose",
			  "opt/arg": "none"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/getter/keyname",
			  description: "name of the key to read",
			  args: "indexed",
			  "args/index": "0"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/setter",
			  description: "set a key's value",
			  command: "set"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/setter/verbose",
			  description: "print additional information about where the value will be stored",
			  opt: "v",
			  "opt/long": "verbose",
			  "opt/arg": "none"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/setter/keyname",
			  description: "name of the key to write",
			  args: "indexed",
			  "args/index": "0"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/setter/value",
			  description: "value to be written",
			  args: "indexed",
        "args/index": "1"),
		  Kdb::Key.new("spec:#{RB_TEST_NS}/dynamic/#",
			  description: "dynamically call a user-supplied command",
			  args: "remaining")
]

def setupSpec
  h = Kdb::KDB.new
  ks = Kdb::KeySet.new
  h.get ks, "spec:#{RB_TEST_NS}"
	
  assert_equal 0, ks.cut(Kdb::Key.new("spec:#{RB_TEST_NS}")).length, "Couldn't setup spec, keys exist!"

	ks << SPEC
	h.set ks, "spec:#{RB_TEST_NS}"
end

def removeSpec
  h = Kdb::KDB.new
  ks = Kdb::KeySet.new
  h.get ks, "spec:#{RB_TEST_NS}"
  ks.cut Kdb::Key.new("spec:#{RB_TEST_NS}")
  h.set ks, "spec:#{RB_TEST_NS}"
end

class KdbTestCases < Test::Unit::TestCase

  def test_kdb_new_simple
    assert_nothing_raised do
      begin
        setupSpec

        custom_argv = ["test", "get", "-v", "user:/"]
        custom_envp = []
  
        config = Kdb::KeySet.new
        contract = Kdb::KeySet.new
        Kdb::gopts_contract contract, custom_argv, custom_envp, Kdb::Key.new(RB_TEST_NS), config

        h = Kdb::KDB.new contract

        ks = Kdb::KeySet.new
        ret = h.get ks, RB_TEST_NS

        assert ret >= 0

        k = ks.lookup RB_TEST_NS
        assert_not_nil k
        assert_equal "getter", k.value

        k = ks.lookup "#{RB_TEST_NS}/getter/keyname"
        assert_not_nil k
        assert_equal "user:/", k.value
        
        k = ks.lookup "#{RB_TEST_NS}/getter/verbose"
        assert_not_nil k
        assert_equal "1", k.value
      ensure
        removeSpec
      end
    end
  end
end
