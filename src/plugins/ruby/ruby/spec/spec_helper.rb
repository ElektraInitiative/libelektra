#!/usr/bin/env ruby

require 'rspec'

REGISTERED_PLUGINS = {}


module Kdb
  class Plugin

    def self.define(name, &block)
      plugin = Plugin.new

      plugin.instance_eval(&block)

      REGISTERED_PLUGINS[name] = plugin
    end
  end
end


def file_content(file, content)
  file.write content
  file.close
end

def get_file_content(filename)
  content = ""
  File.open filename, "r" do |file|
    content = file.read
  end
  return content
end
