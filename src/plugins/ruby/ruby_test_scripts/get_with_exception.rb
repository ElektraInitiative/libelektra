#!/usr/bin/env ruby

require 'kdb'

Kdb::Plugin.define :get_with_exception do

  def get(returned, parent)
    5.times do |i|
      k = Kdb::Key.new parent.name
      k.add_basename "key%d" % i
      k.value = "myvalue%d" % i

      returned << k
    end

    raise "Throwing that expected exception"

    return 0
  end

end

