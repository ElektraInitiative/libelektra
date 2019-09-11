#!/usr/bin/env ruby

require 'kdb'

Kdb::Plugin.define :statefull do

  def get(returned, parent)

    returned << @state unless @state.nil?

    return 0
  end

  def set(returned, parent)

    @state ||= Kdb::KeySet.new

    @state << returned

    return 1
  end

end

