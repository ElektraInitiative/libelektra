require 'kdb'

Kdb::Plugin.define :hello do

  def get(returned, parent)
    puts "hello get from the ruby plugin"
    parent.pretty_print
    
    k = Kdb::Key.new parent.name
    k.add_basename "key1"
    k.value = "key1 value"

    returned << k
    returned.pretty_print

    return 0
  end

  def set(returned, parent)
    puts "hello set from the ruby plugin"
    File.open parent.value, "w" do |file|
      file.write "## ruby test plugin\n\n"
      returned.each do |k|
        file.write "#{k.to_s}\n"
      end
    end
    returned.pretty_print
    parent.pretty_print
  end

  def open(errorKey)
    puts "hello open from the ruby plugin"
  end

  def close(errorKey)
    puts "hello close from the ruby plugin"
  end

end
