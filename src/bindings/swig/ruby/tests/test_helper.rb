#!/usr/bin/env ruby
#encoding: UTF-8
##
# @file
#
# @brief helper class for unit test cases
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'test/unit'

# Ruby 2.3.0 renamed caption_io to capture_output
# we generally use capture_output and define an alias
# if it is missing
#
if !Test::Unit::TestCase.public_instance_methods.include? :capture_output
  module Test::Unit
    class TestCase
      alias capture_output capture_io
    end
  end
end
