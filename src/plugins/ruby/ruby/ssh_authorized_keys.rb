

def array_index_name(name, index)
  index_str = index.to_s
  (1..(index.to_s.size - 1)).each { index_str = "_#{index_str}" }
  "#{name}/##{index_str}"
end

class Ssh_key

  attr_accessor :name
  attr_accessor :keyvalue
  attr_accessor :type
  attr_accessor :comments
  attr_accessor :order
  attr_accessor :line

  def initialize()
    @comments = []
    @type = 'ssh-rsa'
    @order = '__new'
  end

  def add_comment_line(line)
    @comments << line
  end

  def get_keyset(parent)
    key = Kdb::Key.new(parent.name, 
                      value: keyvalue,
                      order: order.to_s,
                      line: line.to_s)
    key.add_basename name
    Ssh_key.key_add_comments(key, comments)
    typekey = Kdb::Key.new key.name, value: type
    typekey.add_basename 'type'

    Kdb::KeySet.new [key, typekey]
  end

  # class method, used for setting comments on parent key too 
  def self.key_add_comments(key, comments)
    comments.each_with_index do |comment_line, comment_index|
      if comment_line =~ /^(\s*#\s*)(.*)$/
        key["#{array_index_name "comment", comment_index}/start"] = $1
        key[array_index_name "comment", comment_index] = $2
      else
        key[array_index_name "comment", comment_index] = comment_line
      end
    end
  end

  def set_from_key(key)
    @name = key.basename
    @keyvalue = key.value
    if key.has_meta? 'order'
      if key['order'] =~ /^\d+$/
        @order = key['order'].to_i
      else
        @order = key['order']
      end
    end
    key.meta.each do |mk|
      next unless mk.name =~ /^comment\/#_*\d+$/

      comment = ""
      comment_start_key = "#{mk.name}/start"
      if key.has_meta? comment_start_key
        comment += key[comment_start_key]
      else
        if !mk.value.strip.empty? and mk.value !~ /^\s*#/
          comment += "#"
        end
      end
      comment += mk.value
      @comments << comment
    end
  end

  def to_file_entry()
    entry = ""
    @comments.each do |cline|
      entry += cline
      entry += "\n"
    end
    entry += "#{type} #{keyvalue} #{name}\n"
  end

end



Kdb::Plugin.define :ssh_authorized_keys do

  def get(returned, parent)
    filename = parent.value
    keys = []
    File.open filename, "r" do |file|

      collected_comments = []

      file.each_with_index do |line, index|
        # empty lines
        if line =~ /^\s*$/ or line =~ /^\s*(#\s*)(.*)$/
          collected_comments << line.chomp

        else
          token = line.split ' '
          # for the moment we do not support options
          key = Ssh_key.new
          key.type = token.shift
          key.keyvalue = token.shift
          key.name = token.join ' '
          key.order = keys.size
          key.line = index
          key.comments = collected_comments
          collected_comments = []

          keys << key
        end
      end
      unless collected_comments.empty?
        Ssh_key.key_add_comments(parent, collected_comments)
      end
    end

    keys.each do |sshkey|
      returned << sshkey.get_keyset(parent)
    end
  end



  def set(returned, parent)
    filename = parent.value

    # walk through the given keySet and create Ssh_key object from it
    keys = []
    current_key = returned.first
    current_sshkey = nil
    returned.each do |key|
      if key.is_direct_below? parent
        current_key = key
        current_sshkey = Ssh_key.new
        current_sshkey.set_from_key key
        keys << current_sshkey

      elsif !current_key.nil? and key.is_below? current_key
        # do not accept keys not direct below
        next unless key.is_direct_below? current_key

        if key.basename == 'type'
          current_sshkey.type = key.value
        end
      end
    end

    # order the keys, depending on the 'order' metadata
    numbered = {}
    other = {}
    keys.each do |key|
      if key.order.is_a? Fixnum
        numbered[key.order] ||= []
        numbered[key.order] << key
      else
        other[key.order] ||= []
        other[key.order] << key
      end
    end

    # now write the file
    File.open filename, "w" do |file|
      numbered.keys.sort.each do |index|
        numbered[index].each do |key|
          file.write key.to_file_entry
        end
      end
      other.keys.sort.each do |index|
        other[index].each do |key|
          file.write key.to_file_entry
        end
      end

      # finally write comments attached to the parent key
      x = Ssh_key.new
      x.set_from_key parent
      x.comments.each do |cline|
        file.write cline
        file.write "\n"
      end
    end

    

  end

end
