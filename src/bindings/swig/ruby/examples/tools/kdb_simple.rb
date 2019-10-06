#!/usr/bin/ruby
##
# @file
#
# @brief minimal kdb tool written Rub
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#
#
# This example is a simple 'kdb' replacement to show some function of the
# Elektra tools API
#
# use kdb_simple.rb --help to see some examples

require 'kdbtools'


class Cmdline
  @@commands = Hash.new

  def self.define(name, &block)
    @@commands[name] = block
  end
end


#
# list all plugins
#
# fetches all avalible plugins and prints them on stdout
#
Cmdline.define :list do

  # create a new PluginDatabase instance
  db = Kdbtools::ModulesPluginDatabase.new

  # get the list of plugins
  plugins = db.list_all_plugins

  puts "all available plugins:"
  plugins.each do |p|
    puts "  #{p}"
  end
end


#
# print info about a specific plugin
#
# similar to $> kdb plugin-info <plugin>
#
# fetches plugin info from system/elektra/modules path
# if plugin is not loaded yet, load it and fetch info
# directly from plugin
#
Cmdline.define :info do
  # get the plugin name from the command line (just the last one ;)
  plugin_name = ARGV.pop
  if plugin_name.nil?
    raise ArgmentError.new "missing plugin name"
  end

  # open the kdb
  Kdb.open do |kdb|
    # this is the path where plugin info can be found
    plugin_key_path = "system/elektra/modules/#{plugin_name}"

    # fetch the info from the database
    conf = Kdb::KeySet.new
    kdb.get conf, plugin_key_path

    # check if it is already there
    if conf.lookup(plugin_key_path).nil?
      # plugin info is missing, so we have to load the plugin
      # first

      # load plugin
      modules = Kdbtools::Modules.new
      begin
        plugin = modules.load plugin_name

        # get info and append it to the central conf
        conf << plugin.get_info
      rescue Kdbtools::NoPlugin
        # modules.load will raise an exception if the plugin
        # can not be loaded
        puts $!
      end
    end

    # plugin infos path
    infos_key = Kdb::Key.new plugin_key_path + "/infos"
    # filter all keys we are interested in
    # -> all below infos_key path
    infos = conf.find_all { |k| k.is_below? infos_key }
    # print each key
    infos.each do |i|
      puts "#{i.basename}: #{i.value}"
    end
  end
end


#
# fetch all configured mountpoints and print it on stdout
# (same as '$> kdb mount'
#
Cmdline.define :showmounts do

  # open the KDB
  Kdb.open do |kdb|

    mountconf = Kdb::KeySet.new

    # fetch mountpoints from db
    kdb.get mountconf, Kdbtools::MOUNTPOINTS_PATH

    # parse the info from the retrieved keyset
    mtab = Kdbtools::Backends.get_backend_info mountconf

    puts "currently configured mountpoints:"
    puts

    mtab.each do |b|
      puts "#{b.path} on #{b.mountpoint}"
    end

    puts
    puts "number of mounts: #{mtab.size}"

  end
end



#
# add a new mountpoint
#
# this is a minimal for for '$> kdb mount <file> <mountpoint> <plugin> ...'
#
Cmdline.define :mount do
  args = ARGV.reverse
  args.pop  # remove the command 'mount'

  if args.size < 3
    Cmdline.usage
    exit
  end

  FILE_PATH = args.pop
  MOUNTPOINT = args.pop

  # open KDB
  #
  kdb = Kdb.open

  begin
    # create a new backend builder
    backend = Kdbtools::MountBackendBuilder.new

    # elektra mountpoint to mount to
    mpk = Kdb::Key.new MOUNTPOINT

    if ! mpk.is_valid?
      raise ArgumentError.new "invalid mountpoint given: #{MOUNTPOINT}"
    end

    # KeySet to collect all mounting settings
    mountconf = Kdb::KeySet.new

    kdb.get mountconf, Kdbtools::MOUNTPOINTS_PATH

    # set mountpoint
    # raises a MountpointAlreadyInUseException if mount point is invalid
    # or already used
    backend.set_mountpoint mpk, mountconf

    # config settings for all plugins
    #config = Kdbtools.parse_plugin_arguments "", "system/"
    #backend.set_backend_config config

    # add the resolver plugin
    backend.add_plugin Kdbtools::PluginSpec.new "resolver"

    # file to mount at mountpoint
    backend.use_config_file FILE_PATH

    # specify that we require a storage plugin
    backend.need_plugin "storage"

    # define recommended plugins
    backend.recommend_plugin "sync"

    # the rest of our command line args are can be passed to 
    # Kdbtools.parse_arguments
    plugin_config_args = args.reverse.join " "

    # add specific plugins
    backend.add_plugins Kdbtools.parse_arguments plugin_config_args

    # resolve needed and recommended plugins
    # returns Array of missing recommended plugin name
    # throws NoPlugin exception if a needed or 
    backend.resolve_needs true

    # generate the mount specifications
    backend.serialize mountconf

    # to check
    #mountconf.pretty_print
    kdb.set mountconf, Kdbtools::MOUNTPOINTS_PATH

  rescue Kdbtools::NoPlugin
    # thrown if one of the specifed plugins was not found
    # or a needed plugin is missing
    puts $!

  rescue Kdbtools::TooManyPlugins
    # thrown by serialize, if a plugin can't be placed
    # e.g. adding two storage plugins
    puts $!

  rescue
    puts $!

  ensure
    # do not forget to close it again
    kdb.close
  end
end



#
# unmout a backend
#
# simply do an unmout
#
Cmdline.define :umount do
  if ARGV.size < 2
    raise ArgmentError.new "missing mountpoint"
  end

  # get the mountpoint
  mountpoint = ARGV.pop

  Kdb.open do |db|
    # fetch the mount info from the db
    conf = Kdb::KeySet.new
    db.get conf, Kdbtools::MOUNTPOINTS_PATH

    # do the umount
    Kdbtools::Backends.umount mountpoint, conf

    db.set conf, Kdbtools::MOUNTPOINTS_PATH
  end
end




# command line helper
#
class Cmdline
  def self.usage
    puts "Demo app to show some libtools functionallity"
    puts "usage: #{__FILE__} <command>"
    puts
    puts "available commands:"
    @@commands.keys.each do |c|
      puts "   #{c}"
    end
    puts
    puts <<EOF
examples:
    #{__FILE__} info dump

    #{__FILE__} mount /tmp/myconfig.ini /myconfig/file ini conf1=a,conf2=b

    #{__FILE__} mount /tmp/file system/file ini sync

    #{__FILE__} umount system/file

EOF
  end

  def self.run
    command = :showmounts
    if ARGV.size > 0
      if ["-h", "--help"].include? ARGV[0]
        usage
        exit
      end

      command = ARGV[0].to_sym
    end

    if ! @@commands.include? command
      puts "unknown command: #{command}"
      usage
      exit
    end

    begin
      @@commands[command].call
    rescue
      puts $!
    end
  end

end

Cmdline.run
