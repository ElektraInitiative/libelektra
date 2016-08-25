# This is not required any more
# same functionallity was now implemented by pure 
# SWIG/C
#
#
#require 'kdb_'
#
#module Kdb
#  include Kdb_
#
#  class Key < Kdb_::Key
#
#    def initialize(name="", options={})
#      flags = 0
#      if options.has_key? :flags
#        flags = options[:flags]
#        options.delete(:flags)
#      elsif options.has_key? "flags"
#        flags = options["flags"]
#        options.delete("flags")
#      end
#      # TODO: add error handling for flags
#      #puts "flags: #{flags}"
#      super name, flags
#
#      options.each { |key, value|
#        key = key.to_s
#        if key == "value"
#          set value.to_s
#        else
#          set_meta(key, value)
#        end
#      }
#    end
#
#
#    def get
#      if is_binary?
#        get_binary
#      else
#        get_string
#      end
#    end
#
#    alias value get
#
#    def set value
#      if is_binary?
#        set_binary value
#      else
#        set_string value
#      end
#    end
#
#    alias value= set
#
#    #def set_binary value
#    #  super value 
#    #end
#
#    # override set|getMeta to allow passing 
#    # also symbols as meta name, since we are using
#    # symbols within our custom constructor
#    def set_meta name, value
#      super name.to_s, value.to_s
#    end
#
#    def get_meta name
#      super name.to_s
#    end
#
#  end
#  
#end
