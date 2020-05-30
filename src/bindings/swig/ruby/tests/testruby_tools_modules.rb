#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdbtools::Modules
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdbtools'
require 'test/unit'
require_relative 'test_helper.rb'

class KdbtoolsModulesTestCases < Test::Unit::TestCase

  def test_modules_new
    assert_nothing_raised do
      m = Kdbtools::Modules.new
      assert_instance_of Kdbtools::Modules, m
      assert_not_nil m
    end
  end

  def test_modules_load_by_name
    assert_nothing_raised do
      m = Kdbtools::Modules.new

      p = m.load "dump"

      assert_instance_of Kdbtools::Plugin, p

      assert_equal "dump", p.name
    end
  end

  def test_modules_load_by_name_and_config
    assert_nothing_raised do
      m = Kdbtools::Modules.new

      config = Kdb::KeySet.new(
        Kdb::Key.new("system:/module", value: "without config")
      )

      p = m.load "dump", config

      assert_instance_of Kdbtools::Plugin, p

      assert_equal "dump", p.name
      assert_equal config, p.get_config
    end
  end

  def test_modules_load_by_pluginspec
    assert_nothing_raised do
      m = Kdbtools::Modules.new

      ps = Kdbtools::PluginSpec.new "dump"

      p = m.load ps

      assert_instance_of Kdbtools::Plugin, p

      assert_equal "dump", p.name
      assert_equal ps.config, p.get_config
    end
  end

  def test_load_invalid_plugin

    m = Kdbtools::Modules.new

    assert_raise Kdbtools::BadPluginName do
      m.load "invalid plugin name"
    end

    assert_raise Kdbtools::NoPlugin do
      m.load "thisplugindoesreallynotexist"
    end
  end
end
