#!/usr/bin/env ruby

require 'kdb'

def array_index_name(name, index)
  index_str = index.to_s
  (1..(index.to_s.size - 1)).each { index_str = "_#{index_str}" }
  "#{name}/##{index_str}"
end

def key_add_comments(key, comments)
  comments.each_with_index do |comment_line, comment_index|
    meta_name = array_index_name 'comment', comment_index
    if comment_line =~ /^(\s*#\s*)(.*)$/
      key["#{meta_name}/start"] = $1
      key[meta_name] = $2
    else
      key[meta_name] = comment_line
      key["#{meta_name}/start"] = ''
    end
  end
end

class FileElement

  def self.create_from_key(key)
    obj = nil
    if key['internal/simpleline'] == '1'
      obj = Simpleline.new
    else
      obj = Shellvar.new
      obj.value = key.value
    end
    obj.name = key.basename
    obj.order = key['order']
    obj.line = key['line']
    key.meta.each do |mk|
      next unless mk.name =~ /^comment\/#_*\d+$/

      comment = ""
      comment_start_key = "#{mk.name}/start"
      if key.has_meta? comment_start_key
        comment += key[comment_start_key]
      else
        comment += "# "
      end
      comment += mk.value
      obj.comments << comment
    end

    return obj
  end

end

class Simpleline < FileElement

  attr_accessor :name
  attr_accessor :order
  attr_accessor :comments
  attr_accessor :line

  def initialize()
    @comments = []
    @order = '@new'
  end

  def order=(value)
    if value.is_a? Integer
      @order = value
    elsif value.is_a? String and value =~ /^\d+$/
      @order = value.to_i
    elsif value.nil?
      @order = '@new'
    else
      @order = value
    end
  end

  def add_comment_line(line)
    @comments << line
  end


  def create_key(parentname)
    key = Kdb::Key.new parentname
    key.add_basename @name
    key_add_comments key, @comments
    key['order'] = @order.to_s
    key['line'] = @line.to_s
    return key
  end

  def get_key(parentname)
    key = create_key parentname
    key['internal/simpleline'] = '1'
    return key
  end

  def to_file_entry()
    entry = ""
    @comments.each { |c| entry += "#{c}\n" }
    entry += "#{@name}\n"
  end
end

class Shellvar < Simpleline
  attr_accessor :value

  def initialize()
    @comments = []
    @value = ""
  end

  def get_key(parentname)
    key = create_key parentname
    key.value = @value.to_s
    return key
  end

  def to_file_entry()
    entry = ""
    @comments.each { |c| entry += "#{c}\n" }
    entry += "#{@name}=#{@value}\n"
  end
end



Kdb::Plugin.define :shellvars do

  def get(returned, parentKey)
    #puts "\nreading from: #{parentKey.value}"
    vars = []
    File.open parentKey.value, "r" do |file|
      collected_comments = []
      file.each_with_index do |line, line_number|
        var = nil
        if line =~ /^(\s*#\s*)(.*)$/ or line =~ /^\s*$/
          # comment line
          collected_comments << line.chomp
          next
        elsif line =~ /^([^=]+)=(.*)$/
          var = Shellvar.new
          vars << var
          var.name = $1
          var.value = $2
        else
          var = Simpleline.new
          vars << var
          var.name = line.chomp
        end
        var.line = line_number
        var.order = vars.size
        var.comments = collected_comments
        collected_comments = []
      end

      unless collected_comments.empty?
        key_add_comments parentKey, collected_comments
      end
    end
    vars.each { |v| returned << v.get_key(parentKey.name) }

    #puts "vars: #{vars}"
    #returned.pretty_print

  end


  def set(returned, parentKey)
    #puts "shellvar plugin: writing to #{parentKey.value}"
    vars = []
    returned.each do |key|
      # only accept keys below parent key
      next unless key.is_direct_below? parentKey
      vars << FileElement.create_from_key(key)
    end

    numbered = {}
    other = {}
    vars.each do |var|
      if var.order.is_a? Fixnum
        numbered[var.order] ||= []
        numbered[var.order] << var
      else
        other[var.order] ||= []
        other[var.order] << var
      end
    end

    File.open parentKey.value, "w" do |file|
      [numbered, other].each do |hash|
        hash.keys.sort.each do |index|
          hash[index].each do |var|
            file.write var.to_file_entry
          end
        end
      end
      FileElement.create_from_key(parentKey).comments.each do |c|
        file.write c
        file.write "\n"
      end
    end
  end

  def close(errorKey)

  end

end
