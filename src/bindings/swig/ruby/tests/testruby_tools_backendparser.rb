#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdbtools.parse_arguments
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdbtools'
require 'test/unit'
require_relative 'test_helper.rb'

class KdbtoolsParseArgumentsTestCases < Test::Unit::TestCase

  def test_parse_arguments
    assert_nothing_raised do
      args = Kdbtools.parse_arguments "plugin c1=v1 c2=v2"

      puts
      puts args[0].config
      puts

      assert_instance_of Array, args
      assert_equal 1, args.size
      assert_instance_of Kdbtools::PluginSpec, args[0]

      args = Kdbtools.parse_arguments "plugin1 c1=v1,c2=v2 plugin2 d=x"
      assert_equal 2, args.size
    end
  end

  def test_parse_plugin_arguments
    assert_nothing_raised do
      args = Kdbtools.parse_plugin_arguments "c1=v1,c2=v2"

      assert_instance_of Kdb::KeySet, args
    end
  end

  def test_parse_arguments_exceptions
    assert_raise Kdbtools::ParseException do
      # pass args without a plugin name
      Kdbtools.parse_arguments "c1=b1"
    end
  end
end

