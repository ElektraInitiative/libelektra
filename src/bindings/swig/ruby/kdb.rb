
require 'kdb_'

module Kdb
  include Kdb_

  class Key < Kdb_::Key

    def initialize(name="", options={})
      flags = 0
      if options.has_key? :flags
        flags = options[:flags]
        options.delete(:flags)
      elsif options.has_key? "flags"
        flags = options["flags"]
        options.delete("flags")
      end
      # TODO: add error handling for flags
      #puts "flags: #{flags}"
      super name, flags

      options.each { |key, value|
        key = key.to_s
        if key == "value"
          set_string value.to_s
        else
          set_meta(key, value)
        end
      }
    end

    # override set|getMeta to allow passing 
    # also symbols as meta name, since we are using
    # symbols within our custom constructor
    def set_meta name, value
      super name.to_s, value.to_s
    end

    def get_meta name
      super name.to_s
    end

  end
  
end
