# encoding: UTF-8
##
# @file
#
# @brief main module for kdb
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#
# This module is an extension to the SWIG created wrapper to libelektra
#

require '_kdb'

module Kdb
  # static open method to easily create a new KDB handle
  # can be used in two ways:
  # without a block:
  #   a new KDB handle will be created and returned
  # with a block given
  #   a new KDB handle will be created and is passed to the block. Once the
  #   block has finished, the KDB handle will be closed again and nil is
  #   returned.
  #
  def self.open(error_key=nil)
    if error_key.nil?
      db = KDB.new
    else
      db = KDB.new error_key
    end

    if ! block_given?
      return db
    end

    begin
      yield db
    rescue KDBException => e
      raise e
    ensure
      db.close
    end
    return nil
  end

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
          puts "    " + k.to_s.sub(/^meta:\//, "")
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


  # Boilerplate for a Ruby based Elektra plugin
  #
  class Plugin

    def open(warningsKey)
      return 0
    end

    def close(warningsKey)
      return 0
    end

    def get(returned, warningsKey)
      return 0
    end

    def set(returned, warningsKey)
      return 0
    end

    def error(returned, warningsKey)
      return 0
    end

  end
end
