#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdbtools::PluginSpec
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdbtools'
require 'test/unit'
require_relative 'test_helper.rb'

class KdbtoolsPluginSpecTestCases < Test::Unit::TestCase

  def test_pluginspec_new
    assert_nothing_raised do
      o = Kdbtools::PluginSpec.new "somename"
      assert_instance_of Kdbtools::PluginSpec, o
      assert_not_nil o
    end
  end

  def test_pluginspec_getters
    assert_nothing_raised do
      p = Kdbtools::PluginSpec.new "somename"

      assert_equal "somename", p.name
      assert_instance_of String, p.name
      assert_instance_of String, p.fullname
      assert_instance_of String, p.refname
      assert_instance_of FalseClass, p.is_refnumber?
      assert_instance_of Kdb::KeySet, p.config

    end
  end

  def test_pluginspec_setters
    assert_nothing_raised do
      p = Kdbtools::PluginSpec.new "somename"

      assert_equal "somename", p.name

      p.name= "other"
      assert_equal "other", p.name

      p.refname= "myref"
      assert_equal "myref", p.refname

      p.fullname= "myfullname"
      assert_equal "myfullname#myref", p.fullname

      p.refnumber= 5
      assert_equal "5", p.refname
      assert_equal true, p.is_refnumber?

      key = Kdb::Key.new "system:/modules", value: "nothing"
      config = Kdb::KeySet.new key

      p.config= config
      assert_equal config, p.config

      key2 = Kdb::Key.new "system:/modules/2", value: "nothing2"
      config2 = Kdb::KeySet.new key2
      p.append_config config2
      config << config2
      assert_equal config, p.config

    end
  end

  def test_pluginspec_setter_exception
    p = Kdbtools::PluginSpec.new "somename"

    assert_raise Kdbtools::BadPluginName do
      p.fullname= "this is an invalid name ! ;."
    end
  end

end
