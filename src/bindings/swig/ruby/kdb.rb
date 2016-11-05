# encoding: UTF-8
##
# @file
#
# @brief main module for kdb
#
# @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
#
# This module is an extension to the SWIG created wrapper to libelektra
#

require '_kdb'

module Kdb

  # open and extend libelektra class Key
  #
  class Key

    def to_s
      if is_binary?
        "#{name}: (binary) length: #{value.length}"
      else
        "#{name}: #{value}"
      end
    end

    def pretty_print
      puts "key '#{name}'"
      if is_binary?
        puts "  binary key, length: #{value.length}"
        puts "    value: #{value.unpack("H*")[0]}"
      else
        puts "  string value: #{value}"
      end
      if meta.size > 0
        puts "  meta data keys: #{meta.size}"
        meta.each do |k|
          puts "    " + k.to_s
        end
      else
        puts "  key has no meta data"
      end
    end
  end


  class KeySet

    def to_s
      to_a.join(', ')
    end

    def pretty_print
      each do |k|
        puts k.to_s
      end
    end
  end
end
