
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
          setString value.to_s
        else
          setMeta(key, value)
        end
      }
    end

    # override set|getMeta to allow passing 
    # also symbols as meta name, since we are using
    # symbols within our custom constructor
    def setMeta name, value
      super name.to_s, value.to_s
    end

    def getMeta name
      super name.to_s
    end

  end
  
end
