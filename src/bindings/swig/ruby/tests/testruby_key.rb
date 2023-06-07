#!/usr/bin/env ruby
#encoding: UTF-8
##
# @file
#
# @brief unit test cases for Kdb::Key
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdb'
require 'test/unit'
require_relative 'test_helper'


class KdbKeyTestCases < Test::Unit::TestCase

  def test_key_new_simple
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_not_nil k
      assert_instance_of Kdb::Key, k
      assert k.is_valid?

      kname = "user:/tmp/k1"
      k = Kdb::Key.new kname
      assert_not_nil k
      assert_equal kname, k.name
      assert k.is_valid?
    end
  end

  def test_key_new_valid_names
    assert_nothing_raised do
      assert Kdb::Key.new("/cascading/tmp/ks1").is_valid?
      assert Kdb::Key.new("spec:/tmp/ks1").is_valid?
      assert Kdb::Key.new("proc:/tmp/ks1").is_valid?
      assert Kdb::Key.new("dir:/tmp/ks1").is_valid?
      assert Kdb::Key.new("user:/tmp/ks1").is_valid?
      assert Kdb::Key.new("system:/tmp/ks1").is_valid?
    end
    assert_raise do
      assert Kdb::Key.new("invalidname")
    end
  end

  def test_key_new_with_options
    assert_nothing_raised do
      name = "user:/tmp/ks1"
      k = Kdb::Key.new name
      assert k.is_valid?
      assert_equal name, k.name
      # it is better here to return "", instead of nil
      assert_equal "", k.get_string

      v = "val"
      k = Kdb::Key.new name, value: v
      assert k.is_valid?
      assert_equal name, k.name
      assert_equal v, k.get_string

      k = Kdb::Key.new name, value: v, owner: "me", comment: "ccc"
      assert k.is_valid?
      assert_equal name, k.name
      assert_equal v, k.get_string
      assert_equal v, k.value
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k.get_meta(:owner)
      assert_equal "ccc", k.get_meta("comment")
      assert_equal "ccc", k.get_meta(:comment)

      k = Kdb::Key.new(name,
                       owner: "me",
                       comment: "ccc",
                       flags: Kdb::KEY_BINARY)
      assert k.is_valid?
      assert k.is_binary?
      assert_equal name, k.name
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k.get_meta(:owner)
      assert_equal "ccc", k.get_meta("comment")
      assert_equal "ccc", k.get_meta(:comment)
      assert_equal "", k.get_meta("flags")
      assert_equal "", k.get_meta(:flags)

    end
  end

  def test_key_binary_value
    assert_nothing_raised do

      name = "user:/tmp/k1"
      v1 = "\000\000\001"
      v2 = "\002\003\004"

      k = Kdb::Key.new(name,
                       value: v1,
                       flags: Kdb::KEY_BINARY)
      assert k.is_valid?
      assert k.is_binary?
      assert_equal v1, k.get_binary
      assert_equal v1, k.get
      assert_equal v1, k.value

      k.value= v2
      assert k.is_valid?
      assert k.is_binary?
      assert_equal v2, k.get_binary
      assert_equal v2, k.get
      assert_equal v2, k.value

      k.set_binary v1
      assert k.is_valid?
      assert k.is_binary?
      assert_equal v1, k.get_binary
      assert_equal v1, k.get
      assert_equal v1, k.value

      k.set_string "abc"
      assert k.is_valid?
      assert ! k.is_binary?
      assert_equal "abc", k.get_string
      assert_equal "abc", k.get
      assert_equal "abc", k.value

      k = Kdb::Key.new name, flags: Kdb::KEY_BINARY
      assert k.is_valid?
      assert k.is_binary?
      k.value= v2
      assert_equal v2, k.get_binary
      assert_equal v2, k.get
      assert_equal v2, k.value

      k = Kdb::Key.new name
      k.set_binary v1
      assert k.is_valid?
      assert k.is_binary?
      assert_equal v1, k.value

      k = Kdb::Key.new name
      k.value= v2
      assert k.is_valid?
      assert ! k.is_binary?
      assert_equal v2, k.value
      k.set_binary v1
      assert k.is_binary?
      assert_equal v1, k.value
    end
  end



  def test_key_get_set_meta
    assert_nothing_raised do
      k = Kdb::Key.new "user:/asdf"

      assert k.is_valid?

      k.set_meta "comment/#0", "hello"
      assert_equal "hello", k.get_meta("comment/#0")
      assert_equal "hello", k["comment/#0"]

      k["owner"] = "me"
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k["owner"]
    end
  end

  def test_key_get_invalid_meta
    k = Kdb::Key.new "user:/asdf"

    assert_equal "", k["does_not_exist"]

  end


  def test_key_delete_meta
    assert_nothing_raised do
      k = Kdb::Key.new "user:/asdf"

      assert k.is_valid?

      k.set_meta "comments/#0", "some value"
      k.set_meta "comments/#1", "2nd line"
      assert_equal "some value", k.get_meta("comments/#0")
      assert_equal "2nd line", k.get_meta("comments/#1")
      assert_equal 2, k.meta.size

      k.del_meta "comments/#1"

      assert_equal 1, k.meta.size
      assert_equal "some value", k.get_meta("comments/#0")
      assert_equal "", k.get_meta("comments/#1")

      k["comments/#2"] = "other line"

      assert_equal 2, k.meta.size
      assert_equal "some value", k["comments/#0"]
      assert_equal "other line", k["comments/#2"]

    end
  end


  def test_key_get_name
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_equal "/", k.name

      name = "user:/tmp/k1"
      k.name = name
      assert_equal name, k.name
      assert_equal name.split('/').reverse[0], k.basename
      assert_equal Kdb::ElektraNamespace_USER, k.namespace

      k.add_basename "b1"
      assert_equal "b1", k.basename
      assert_equal "#{name}/b1", k.name

      k.add_basename "bb2"
      assert_equal "bb2", k.basename
      assert_equal "#{name}/b1/bb2", k.name

      k.add_name "n1/n2"
      assert_equal "n2", k.basename
      assert_equal "#{name}/b1/bb2/n1/n2", k.name

      k.add_name "../../../../new1"
      assert_equal "new1", k.basename
      assert_equal "#{name}/new1", k.name

      k.add_name "..\\/escaped_name\\/k"
      assert_equal "../escaped_name/k", k.basename
      assert_equal "#{name}/new1/..\\/escaped_name\\/k", k.name

    end
  end

  def test_throw_KeyTypeMismatch_on_wrong_get_method
    k = Kdb::Key.new "user:/mykey"
    k.set_binary "\000\001\002"

    assert k.is_binary?

    assert_raise Kdb::KeyTypeMismatch do
      k.get_string
    end

    k = Kdb::Key.new "user:/mykey2"
    k.set_string "hello world"

    assert ! k.is_binary?

    assert_raise Kdb::KeyTypeMismatch do
      k.get_binary
    end
  end

  def test_KeyName_comperators
    assert_nothing_raised do
      k1 = Kdb::Key.new "user:/test"
      k2 = Kdb::Key.new "user:/test"

      assert k1.name == k2.name
      assert k1 == k2
      assert k1 <= k2
      assert k1 >= k2

      assert_equal 0, k1 <=> k2


      k2 = Kdb::Key.new "user:/anothertest"

      assert k1.name != k2.name
      assert k1 != k2

      assert k1 > k2
      assert k2 < k1

      assert_equal (-1), k2 <=> k1
      assert_equal 1, k1 <=> k2

    end
  end

  def test_KeyName_hierarchy
    assert_nothing_raised do
      k1 = Kdb::Key.new "user:/app"
      k2 = Kdb::Key.new "user:/app/v1"
      k3 = Kdb::Key.new "user:/app/v1/conf1"

      assert k1.is_valid?
      assert k2.is_valid?
      assert k3.is_valid?

      assert k2.is_below k1
      assert k2.is_direct_below k1
      assert k2.is_below_or_same k1

    end
  end

  def test_Key_cloning
    k = Kdb::Key.new "user:/key1", value: "hello", meta1: "mv"

    assert k.is_valid?
    assert_equal "user:/key1", k.name
    assert_equal "hello", k.value
    assert_equal "mv", k["meta1"]

    kc = k.clone

    assert k.__id__ != kc.__id__
    assert_instance_of Kdb::Key, kc
    assert_equal "user:/key1", kc.name
    assert_equal "hello", kc.value
    assert_equal "mv", kc["meta1"]

    kc.name = "user:/key2"
    kc.value = "world"
    kc["meta1"] = "vm"
    kc["meta2"] = "vm2"

    assert_equal "user:/key1", k.name
    assert_equal "hello", k.value
    assert_equal "mv", k["meta1"]
    assert ! k.has_meta?("meta2")

    assert_equal "user:/key2", kc.name
    assert_equal "world", kc.value
    assert_equal "vm", kc["meta1"]
    assert_equal "vm2", kc["meta2"]

    kc2 = kc.dup

    assert kc.__id__ != kc2.__id__
    assert k.__id__ != kc2.__id__

    kc2.name = "user:/key3"
    kc2.value = "wonderful"

    assert_equal "user:/key2", kc.name
    assert_equal "world", kc.value

    assert_equal "user:/key3", kc2.name
    assert_equal "wonderful", kc2.value

  end

  def test_meta_iterator
    assert_nothing_raised do
      k = Kdb::Key.new("user:/asdf",
                       owner: "me",
                       comment: "hello",
                       meta1: "meta1 value",
                       othermeta: "othermeta value")

      assert_equal "me", k["owner"]
      assert_equal "hello", k[:comment]
      assert_equal "meta1 value", k["meta1"]
      assert_equal "othermeta value", k["othermeta"]


	  metaKeys = k.meta

      mk = metaKeys[0]
      assert_instance_of Kdb::Key, mk
      assert_equal "hello", mk.value
      assert_equal "meta:/comment", mk.name
      assert_equal "hello", k.get_meta(mk.name)

      mk = metaKeys[1]
      assert_instance_of Kdb::Key, mk
      assert_equal "meta1 value", mk.value
      assert_equal "meta:/meta1", mk.name
      assert_equal "meta1 value", k.get_meta(mk.name)

      mk = metaKeys[2]
      assert_instance_of Kdb::Key, mk
      assert_equal "othermeta value", mk.value
      assert_equal "meta:/othermeta", mk.name
      assert_equal "othermeta value", k.get_meta(mk.name)

      mk = metaKeys[3]
      assert_instance_of Kdb::Key, mk
      assert_equal "me", mk.value
      assert_equal "meta:/owner", mk.name
      assert_equal "me", k.get_meta(mk.name)

      # test Ruby-style meta iterator
      assert_instance_of Kdb::KeySet, k.meta

      a = ["meta:/comment", "meta:/meta1", "meta:/othermeta", "meta:/owner"]
      i = 0;
      k.meta.each { |m|
        assert_equal a[i], m.name
        i += 1
      }
    end
  end

  def test_to_s
    assert_nothing_raised do
      k = Kdb::Key.new

      assert_equal "/: ", k.to_s

      k = Kdb::Key.new "user:/key1", value: "somevalue"

      assert_equal "user:/key1: somevalue", k.to_s

      v = "\000\001\002"
      k = Kdb::Key.new("user:/binkey1",
                        value: v,
                        flags: Kdb::KEY_BINARY)

      assert k.is_binary?
      assert_equal "user:/binkey1: (binary) length: #{v.length}", k.to_s
    end
  end

  def test_pretty_print
    assert_nothing_raised do
      k = Kdb::Key.new

      out, err = capture_output do
        k.pretty_print
      end

      expected = <<EOF
key '/'
  string value: 
  key has no meta data
EOF
      assert_equal expected, out
      assert_equal '', err


      k = Kdb::Key.new "user:/key1", value: "somevalue", meta1: "metavalue"

      out, err = capture_output { k.pretty_print }

      expected = <<EOF
key 'user:/key1'
  string value: somevalue
  meta data keys: 1
    meta1: metavalue
EOF
      assert_equal expected, out
      assert_equal '', err


      k = Kdb::Key.new("user:/key2",
                       value: "\xff\xaa\x55\x01",
                       flags: Kdb::KEY_BINARY,
                       meta_1: "m1 value",
                       meta_2: "m2 value",
                       meta_3: "m3 value")

      assert k.is_binary?

      out, err = capture_output { k.pretty_print }

      expected = <<EOF
key 'user:/key2'
  binary key, length: 4
    value: ffaa5501
  meta data keys: 4
    binary: 
    meta_1: m1 value
    meta_2: m2 value
    meta_3: m3 value
EOF

      assert_equal expected, out
      assert_equal '', err
    end
  end
end
