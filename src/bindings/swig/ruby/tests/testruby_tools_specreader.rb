#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdbtools::SpecReader and friends
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdbtools'
require 'test/unit'
require_relative 'test_helper.rb'

class KdbtoolsSpecReaderTestCases < Test::Unit::TestCase

  def test_spec_reader
    assert_nothing_raised do
      bb = Kdbtools::SpecReader.new

      spec = Kdb::KeySet.new
      spec << Kdb::Key.new("spec:/example/xx", 
                           mountpoint: "/example/xx")

      bb.read_specification spec

      backends = bb.get_backends

      assert_instance_of Kdbtools::SpecBackendBuilderMap, backends
      assert backends.respond_to? :keys
      assert backends.respond_to? :values
      assert backends.respond_to? :each
      assert backends.respond_to? :size

      assert_equal 1, backends.size

      assert_instance_of Kdbtools::SpecBackendBuilder, 
                         backends[Kdb::Key.new "spec:/example/xx"]

    end
  end

end

