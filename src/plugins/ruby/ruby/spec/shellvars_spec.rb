#!/usr/bin/env ruby

$LOAD_PATH << File.absolute_path(File.dirname($FILENAME))
$LOAD_PATH << File.absolute_path("File.dirname($FILENAME)/..")

require 'spec_helper'
require 'tempfile'
require 'shellvars.rb'

describe "shellvars plugin" do
  subject { REGISTERED_PLUGINS[:shellvars] }

  it "it a Kdb::Plugin class" do
    expect(subject).to be_an_instance_of Kdb::Plugin
  end

  context "reads and writes files" do
    let(:file)     { Tempfile.new 'test' }
    let(:keyset)   { Kdb::KeySet.new }
    let(:parent)   { Kdb::Key.new 'user:/tests/shellvar', value: file.path }
    let(:outfile)  { Tempfile.new 'outtest' }
    let(:outparent){ Kdb::Key.new 'user:/tests/shellvar', value: outfile.path }


    after :example do
      file.unlink
    end

    it "reads empty file" do
      subject.get keyset, parent
      expect(keyset.size).to eq 0
    end

    it "reads simple one line shell var" do
      content = "VAR1=value\n"
      file_content file, content
      subject.get keyset, parent

      expect(keyset.size).to eq 1
      expect(keyset.lookup "#{parent.name}/VAR1").to_not be_nil
      expect(keyset.lookup("#{parent.name}/VAR1").value).to eq "value"
    end

    it "read four lines with shell vars" do
      content = <<-EOT
VAR1=abc
var2=def
hello=world
world=hello
      EOT
      file_content file, content
      subject.get keyset, parent

      expect(keyset.size).to eq 4
      expect(keyset.lookup "#{parent.name}/VAR1").to_not be_nil
      expect(keyset.lookup("#{parent.name}/VAR1").value).to eq "abc"
      expect(keyset.lookup("#{parent.name}/VAR1")['order']).to eq "1"
      expect(keyset.lookup "#{parent.name}/var2").to_not be_nil
      expect(keyset.lookup("#{parent.name}/var2").value).to eq "def"
      expect(keyset.lookup("#{parent.name}/var2")['order']).to eq "2"
      expect(keyset.lookup "#{parent.name}/hello").to_not be_nil
      expect(keyset.lookup("#{parent.name}/hello").value).to eq "world"
      expect(keyset.lookup("#{parent.name}/hello")['order']).to eq "3"
      expect(keyset.lookup "#{parent.name}/world").to_not be_nil
      expect(keyset.lookup("#{parent.name}/world").value).to eq "hello"
      expect(keyset.lookup("#{parent.name}/world")['order']).to eq "4"
    end

    it "read shellvar with one comment line" do
      content = <<-EOT
#this is a simple comment
var1=value
# other var
var2=othervalue
      EOT
      file_content file, content
      subject.get keyset, parent

      expect(keyset.size).to eq 2

      key = keyset.lookup "#{parent.name}/var1"
      expect(key.value).to eq "value"
      expect(key['comment/#0']).to eq "this is a simple comment"
      expect(key['comment/#0/start']).to eq "#"

      key = keyset.lookup "#{parent.name}/var2"
      expect(key.value).to eq "othervalue"
      expect(key['comment/#0']).to eq "other var"
      expect(key['comment/#0/start']).to eq "# "
    end

    it "read shellvar with long comment" do
      content = <<-EOT
# long comment
# line 2
# line 3
# line 4
# line 5
# line 6
# line 7
# line 8
# line 9
# line 10
# line 11
var=value
      EOT
      file_content file, content
      subject.get keyset, parent

      expect(keyset.size).to eq 1

      key = keyset.lookup "#{parent.name}/var"
      expect(key.value).to eq "value"
      expect(key['comment/#0']).to eq "long comment"
      expect(key['comment/#5']).to eq "line 6"
      expect(key['comment/#_10']).to eq "line 11"
    end


    it "read/write shellvars with empty lines" do
      content = <<-EOT

# comment, with empty line at start
var1=v1

# another comment

var2=v2
      EOT
      file_content file, content
      subject.get keyset, parent

      expect(keyset.size).to eq 2

      key = keyset.lookup "#{parent.name}/var1"
      expect(key.value).to eq "v1"
      expect(key['comment/#0']).to eq ""
      expect(key['comment/#0/start']).to eq ""
      expect(key['comment/#1']).to eq "comment, with empty line at start"
      expect(key['comment/#1/start']).to eq '# '
      expect(key.has_meta? 'comment/#2').to be false

      key = keyset.lookup "#{parent.name}/var2"
      expect(key.value).to eq "v2"
      expect(key['comment/#0']).to eq ""
      expect(key['comment/#0/start']).to eq ''
      expect(key['comment/#1']).to eq "another comment"
      expect(key['comment/#1/start']).to eq '# '
      expect(key['comment/#2']).to eq ""
      expect(key['comment/#2/start']).to eq ''
      expect(key.has_meta? 'comment/#3').to be false
    end

    it "stores last comments are parentkey" do
      content = <<-EOT
# comment
var1=hello

# last comment
      EOT
      file_content file, content
      subject.get keyset, parent

      expect(parent['comment/#0']).to eq ''
      expect(parent['comment/#0/start']).to eq ''
      expect(parent['comment/#1']).to eq 'last comment'
      expect(parent['comment/#1/start']).to eq '# '
    end



    it "write a simple one line file" do
      content = "var1=value\n"
      file_content file, content

      subject.get keyset, parent
      parent.value = outfile.path
      subject.set keyset, parent

      expect(get_file_content outfile.path).to eq content
    end

    it "preserves the order of vars" do
      content = <<-EOT
zzvar=v1
aavar=v2
kkvar=v3
      EOT
      file_content file, content

      subject.get keyset, parent
      parent.value = outfile.path
      subject.set keyset, parent

      expect(get_file_content outfile.path).to eq content
    end

    it "preserves comments" do
      content = <<-EOT

# first comment line
#another
var1=v1

# hello

abc=def

#last comment

      EOT
      file_content file, content

      subject.get keyset, parent
      parent.value = outfile.path
      subject.set keyset, parent

      expect(get_file_content outfile.path).to eq content
    end
      
  end


end
