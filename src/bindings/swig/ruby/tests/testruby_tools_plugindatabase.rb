#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdbtools::PluginDatabase and friends
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdbtools'
require 'test/unit'
require_relative 'test_helper.rb'

class KdbtoolsPluginDatabaseTestCases < Test::Unit::TestCase

  def test_instance
    assert_nothing_raised do
      db = Kdbtools::ModulesPluginDatabase.new

      assert db.respond_to? :list_all_plugins
      assert db.respond_to? :lookup_info
      assert db.respond_to? :lookup_metadata

      assert_instance_of Kdbtools::ModulesPluginDatabase, db

      # check if pointer is valid (should not crash)
      db.list_all_plugins
    end
  end

  def test_vector_of_strings
    assert_nothing_raised do
      db = Kdbtools::ModulesPluginDatabase.new

      plugins = db.list_all_plugins

      assert plugins.respond_to? :size
      assert plugins.respond_to? :each
      assert_instance_of Array, plugins

      # require to have at least one plugin
      assert_instance_of String, plugins[0]
    end
  end

  def test_get_pluginspec
    assert_nothing_raised do
      db = Kdbtools::ModulesPluginDatabase.new

      ps = db.lookup_provides "mini"

      assert_instance_of Kdbtools::PluginSpec, ps
      ps.fullname
    end
  end

  def test_get_pluginspec_vector
    assert_nothing_raised do
      db = Kdbtools::ModulesPluginDatabase.new

      ps = db.lookup_all_provides "mini"

      assert_instance_of Array, ps
      ps.each do |p|
        assert_instance_of Kdbtools::PluginSpec, p
        p.fullname
      end
    end
  end

  def test_get_int_pluginspec_map
    assert_nothing_raised do
      db = Kdbtools::ModulesPluginDatabase.new

      ps = db.lookup_all_provides_with_status "mini"

      assert_instance_of Kdbtools::IntPluginSpecMap, ps
      assert ps.respond_to? :each
      ps.each do |k,v|
        assert_instance_of Fixnum, k
        assert_instance_of Kdbtools::PluginSpec, v
        v.fullname
      end
    end
  end

  def test_plugindatabase_status
    assert_nothing_raised do
      db = Kdbtools::ModulesPluginDatabase.new

      ps = Kdbtools::PluginSpec.new "dump"
      status = db.status ps

      assert_equal Kdbtools::PLUGIN_STATUS_REAL, status
    end
  end
end

