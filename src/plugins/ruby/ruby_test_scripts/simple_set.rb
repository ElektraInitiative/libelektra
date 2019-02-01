#!/usr/bin/env ruby

require 'kdb'

Kdb::Plugin.define :simple_set do

  def get(returned, parent)
    return 0
  end

  def set(keyset, parent)
    size = keyset.size

    # now we would write the file or whatever

    return size
  end

end

