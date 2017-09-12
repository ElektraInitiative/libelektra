#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdbtools::BackendBuilder and friends
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdbtools'
require 'test/unit'
require_relative 'test_helper.rb'

class KdbtoolsBackendbuilderTestCases < Test::Unit::TestCase

  def test_shared_ptr
    assert_nothing_raised do
      bb = Kdbtools::BackendBuilder.new

      db = bb.get_plugin_database()

      assert db.respond_to? :list_all_plugins
      assert db.respond_to? :lookup_info
      assert db.respond_to? :lookup_metadata

      assert_instance_of Kdbtools::PluginDatabase, db
      assert db.is_a? Kdbtools::PluginDatabase

      # check if pointer is valid (should not crash)
      plugins = db.list_all_plugins
      assert_instance_of Array, plugins
    end
  end

end

