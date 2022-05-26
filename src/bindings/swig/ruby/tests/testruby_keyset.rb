#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdb::KeySet
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdb'
require 'test/unit'
require_relative 'test_helper'


class KdbKeySetTestCases < Test::Unit::TestCase

  def test_keySet_new_simple
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert_not_nil ks
      assert_instance_of Kdb::KeySet, ks

      assert_equal 0, ks.size

    end
  end

  def test_keySet_new_with_key
    assert_nothing_raised do
      k = Kdb::Key.new "user:/key", value: "hello"
      ks = Kdb::KeySet.new k

      assert_equal 1, ks.size
      assert_equal k, ks[0]
    end
  end

  def test_keySet_new_with_invalid_argument
    assert_raise ArgumentError do
      Kdb::KeySet.new "not a key"
    end
  end

  def test_keySet_new_with_keySet
    assert_nothing_raised do
      ks1 = Kdb::KeySet.new
      ks1 << Kdb::Key.new("user:/ks1")
      ks1 << Kdb::Key.new("user:/ks2")

      ks2 = Kdb::KeySet.new ks1

      assert_equal 2, ks2.size

      ks2 << Kdb::Key.new("user:/ks3")

      assert_equal 3, ks2.size
      assert_equal "user:/ks3", ks2[ks2.size-1].name

      # ensure old KeySet holds only the first 2 Keys
      assert_equal 2, ks1.size
      assert_equal "user:/ks2", ks1[ks1.size-1].name
    end
  end

  def test_keySet_new_with_array
    assert_nothing_raised do
      a = Array.new
      a << Kdb::Key.new("user:/ks1")
      a << Kdb::Key.new("user:/ks2")
      a << Kdb::Key.new("user:/ks3")
      a << Kdb::Key.new("user:/ks4")

      ks = Kdb::KeySet.new a

      assert_equal 4, ks.size
      i = 0
      ks.each { |e|
        assert_equal a[i], e
        i += 1
      }

      ks = Kdb::KeySet.new [
        Kdb::Key.new("user:/key1"),
        Kdb::Key.new("user:/key2"),
        Kdb::Key.new("user:/key3")
      ]

      assert_equal 3, ks.size
      assert_equal "key1", ks[0].basename

      # ensure also larger arrays, with more than 16 (preallocated) elements
      # work correctly
      a = (1..40).map { |n| Kdb::Key.new("user:/key%02d" % n) }
      ks = Kdb::KeySet.new a

      assert_equal 40, ks.size
      assert_equal "key40", ks[ks.size-1].basename
    end

    assert_raise ArgumentError do
      Kdb::KeySet.new [
        Kdb::Key.new("user:/key"),
        "not a key",
        1
      ]
    end
  end


  def test_keySet_append
    assert_nothing_raised do
      k = Kdb::Key.new "user:/ks1", value: "val", meta: "metaval"

      ks = Kdb::KeySet.new

      num = ks.append k

      assert_equal 1, ks.size
      assert_equal 1, num

      assert_equal k, ks[0]
      assert_equal k, ks[ks.size-1]

      ks << Kdb::Key.new("user:/ks2", value: "val2")

      assert_equal 2, ks.size

      assert_equal k, ks[0]
      assert_equal "user:/ks2", ks[ks.size-1].name

    end
  end

  def test_keySet_append_KeySet
    assert_nothing_raised do
      ks1 = Kdb::KeySet.new

      num = ks1 << Kdb::Key.new("user:/ks1")
      assert_equal 1, num
      num = ks1 << Kdb::Key.new("user:/ks2")
      assert_equal 2, num

      ks2 = Kdb::KeySet.new

      num = ks2 << Kdb::Key.new("user:/ks3")
      assert_equal 1, num
      num = ks2 << Kdb::Key.new("user:/ks4")
      assert_equal 2, num

      num = ks1 << ks2
      assert_equal 4, num

      assert_equal 4, ks1.size
      assert_equal 2, ks2.size
    end
  end

  def test_keySet_append_array
    assert_nothing_raised do
      a = Array.new
      a << Kdb::Key.new("user:/ks2")
      a << Kdb::Key.new("user:/ks1")

      ks = Kdb::KeySet.new

      num = ks.append a
      assert_equal 2, num

      assert_equal 2, ks.size
      assert_equal a[0], ks[1]
      assert_equal a[1], ks[0]
    end

    assert_nothing_raised do
      ks = Kdb::KeySet.new

      num = ks << [
        Kdb::Key.new("user:/ks1"),
        Kdb::Key.new("user:/ks2"),
        Kdb::Key.new("user:/ks3")
      ]
      assert_equal 3, num

      assert_equal 3, ks.size
      assert_equal "user:/ks1", ks[0].name
    end


    a = Array.new
    a << "not a Key"
    a << 1

    ks = Kdb::KeySet.new

    assert_raise ArgumentError do
      ks.append a
    end

  end

  def test_keySet_append_invalid_type
    assert_raise ArgumentError do
      ks = Kdb::KeySet.new

      ks.append "not a Key"
    end
  end

  def test_keySet_get_by_cursor_or_index
    assert_nothing_raised do
      ks = Kdb::KeySet.new (0..9).map { |i| Kdb::Key.new "user:/key%02d" % i }

      assert_equal 10, ks.size

      # test get by index
      for i in (0..9) do
        assert_equal ("user:/key%02d" % i), ks.at(i).name
        assert_equal ("user:/key%02d" % i), ks[i].name
      end

      # test get by invalid index
      assert_nil ks[10]
      assert_nil ks[-11]
      assert_nil ks.at(200)
    end
  end

  def test_keySet_pop
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert_nil ks.pop

      k = Kdb::Key.new("user:/k0")
      ks << k
      assert_equal 1, ks.size
      assert_equal k, ks.pop
      assert_equal 0, ks.size

      k = Kdb::Key.new("user:/k1")
      ks << k
      assert_equal 1, ks.size
      assert_equal k, ks.pop
      assert_equal 0, ks.size

      k2 = Kdb::Key.new("user:/k2")
      k3 = Kdb::Key.new("user:/k3")
      k4 = Kdb::Key.new("user:/k4")
      k5 = Kdb::Key.new("user:/k5")

      ks << [k2, k3, k4, k5]

      assert_equal 4, ks.size
      assert_equal k5, ks.pop
      assert_equal k4, ks.pop
      assert_equal k3, ks.pop
      assert_equal k2, ks.pop
      assert_equal 0, ks.size
      assert_nil ks.pop
    end
  end



  def test_keySet_head_tail
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert_equal 0, ks.size

      assert_nil ks[0]
      assert_nil ks[ks.size-1]

      a = (0..3).map { |i| Kdb::Key.new "user:/key#{i}" }

      ks << a[0]

      assert_equal a[0], ks[0]
      assert_equal a[0], ks[ks.size-1]

      ks << a[1]

      assert_equal a[0], ks[0]
      assert_equal a[1], ks[ks.size-1]

      ks << a[2]

      assert_equal a[0], ks[0]
      assert_equal a[2], ks[ks.size-1]

      ks << a[3]

      assert_equal a[0], ks[0]
      assert_equal a[3], ks[ks.size-1]

      assert_equal a[3], ks.pop
      assert_equal a[2], ks[ks.size-1]

      assert_equal a[2], ks.pop
      assert_equal a[1], ks[ks.size-1]

      assert_equal a[1], ks.pop
      assert_equal a[0], ks[ks.size-1]

      assert_equal a[0], ks.pop
      assert_nil ks[ks.size-1]
    end
  end



  def test_keySet_each_enumeralbe
    assert_nothing_raised do
      a = Array.new
      # create test keys
      a << Kdb::Key.new("user:/k0", value: "v0", owner: "me")
      a << Kdb::Key.new("user:/k1", value: "v1", owner: "you")
      a << Kdb::Key.new("user:/k2", value: "v2", owner: "john", m2: "a")
      a << Kdb::Key.new("user:/k3", value: "v3", owner: "jane")
      a << Kdb::Key.new("user:/k4", value: "v4", owner: "max")
      a << Kdb::Key.new("user:/k5", value: "v5", owner: "bob", m2: "b")
      a << Kdb::Key.new("user:/k6", value: "v6", owner: "alice")
      a << Kdb::Key.new("user:/k7", value: "v7", owner: "fritz", m3: "c")
      a << Kdb::Key.new("user:/k8", value: "v8", owner: "anton")
      a << Kdb::Key.new("user:/k9", value: "v9", owner: "berta")

      ks = Kdb::KeySet.new

      # populate keySet
      a.each { |e| ks << e }

      assert_equal 10, ks.size

      # this will only work, if test keys have key names in desc order
      i = 0
      ks.each do |e|
        assert a[i] == e
        i += 1
      end

      assert_equal 10, i


      # test Enumerable mixin
      assert ks.all? { |e| e.namespace == Kdb::ElektraNamespace_USER }
      assert ks.all? { |e| e.has_meta? "owner" }
      assert ks.all? { |e| e.is_string? }

      assert ! ks.all? { |e| e.value == "v0" }
      assert ks.any? { |e| e.value == "v0" }

      k = ks.find { |e| e.name == "user:/k5" }
      assert_instance_of Kdb::Key, k
      assert k.is_valid?
      assert ! k.is_null?
      assert_equal "user:/k5", k.name
      assert a[5] == k

      k = ks.find { |e| e.name == "does_not_exist" }
      assert_nil k

      assert_equal 2, ks.count { |e| e.has_meta? "m2" }

      tmpa = ks.find_all { |e| e.has_meta? "m2" }
      assert_instance_of Array, tmpa
      assert_equal 2, tmpa.size
      assert_equal a[2], tmpa[0]
      assert_equal a[5], tmpa[1]


      # test Enumerable #min, #max
      assert_equal a[9], ks.max
      assert_equal a[0], ks.min

      # owner: you is maximum
      assert_equal a[1], ks.max_by { |e| e["owner"] }


      # our each impl makes inplace modifications
      ks.each { |e| e["new_meta"] = "persisted" }

      assert ks.all? { |e| e.has_meta? "new_meta" }
      assert ks.all? { |e| e["new_meta"] == "persisted" }
    end
  end

  def test_keySet_comparison
    assert_nothing_raised do
      a = (1..5).map { |i| Kdb::Key.new "user:/key#{i}" }

      ks1 = Kdb::KeySet.new a
      ks2 = Kdb::KeySet.new a

      assert_equal ks1.size, ks2.size
      assert_equal ks1[0], ks2[0]
      assert_equal ks1[1], ks2[1]
      assert_equal ks1[2], ks2[2]
      assert_equal ks1[3], ks2[3]
      assert_equal ks1[4], ks2[4]

      assert ks1 == ks2
      assert ! ks1 != ks2

      assert ks1.eql?(ks2)
      assert ks2.eql?(ks1)

      ks2 << Kdb::Key.new("user:/key100")

      assert ks1 != ks2

      assert ! ks1.eql?(ks2)
      assert ! ks2.eql?(ks1)
    end
  end

  def test_keySet_lookup_lookupByName
    assert_nothing_raised do
      ks = Kdb::KeySet.new (1..10).map { |i| Kdb::Key.new("user:/key%02d" % i) }

      assert_equal 10, ks.size

      # lookupByName
      assert_equal "user:/key01", ks.lookup("user:/key01").name
      assert_equal Kdb::Key.new("user:/key02"), ks.lookup("user:/key02")

      # lookup
      assert_equal "user:/key03", ks.lookup(Kdb::Key.new "user:/key03").name

      # lookup unknown key
      assert_nil ks.lookup("user:/key_now_in_keyset")
      assert_nil ks.lookup(Kdb::Key.new "user:/key_now_in_keyset")

      # with options
      lookupkey = Kdb::Key.new "user:/key05"
      assert_equal lookupkey, ks.lookup(lookupkey, Kdb::KDB_O_POP)
      assert_equal 9, ks.size
      assert_nil ks.lookup(lookupkey)

    end
  end

  def test_keySet_dup_or_clone
    assert_nothing_raised do
      a = (0..4).map { |i| Kdb::Key.new "user:/key#{i}" }

      ks = Kdb::KeySet.new a

      assert_equal 5, ks.size

      ks_dup = ks.dup

      assert_equal ks.size, ks_dup.size
      assert ks == ks_dup
      assert ks.__id__ != ks_dup.__id__

      ks_dup << Kdb::Key.new("user:/key5")

      assert_equal 5, ks.size
      assert_equal 6, ks_dup.size

      assert_equal "user:/key4", ks[ks.size-1].name
      assert_equal "user:/key5", ks_dup[ks_dup.size-1].name

      assert_equal a[4], ks.pop
      assert_equal 4, ks.size
      assert_equal 6, ks_dup.size
      assert_equal "user:/key3", ks[ks.size-1].name
      assert_equal "user:/key5", ks_dup[ks_dup.size-1].name

      # however, its just a shallow copy, thus modifying keys has effect
      # to both key sets

      assert_equal "", ks[1].value
      assert_equal "", ks_dup[1].value

      new_value = "some important value"
      ks[1].value = new_value

      assert_equal new_value, ks[1].value
      assert_equal new_value, ks_dup[1].value
    end
  end

  def test_keySet_cut
    assert_nothing_raised do
      ks = Kdb::KeySet.new [
        Kdb::Key.new("user:/app1/setting1"),
        Kdb::Key.new("user:/app1/setting2"),
        Kdb::Key.new("user:/app1/setting3"),
        Kdb::Key.new("user:/app2/setting1"),
        Kdb::Key.new("user:/app2/common/setting1"),
        Kdb::Key.new("user:/app2/common/setting2"),
        Kdb::Key.new("user:/app2/setting2"),
        Kdb::Key.new("user:/app3/setting1")
      ]

      assert_equal 8, ks.size

      app2 = ks.cut Kdb::Key.new("user:/app2")

      assert_equal 4, app2.size
      assert_equal 4, ks.size

      assert_equal "user:/app1/setting1", ks[0].name
      assert_equal "user:/app1/setting2", ks[1].name
      assert_equal "user:/app1/setting3", ks[2].name
      assert_equal "user:/app3/setting1", ks[3].name

      assert_equal "user:/app2/common/setting1", app2[0].name
      assert_equal "user:/app2/common/setting2", app2[1].name
      assert_equal "user:/app2/setting1", app2[2].name
      assert_equal "user:/app2/setting2", app2[3].name


      app4 = ks.cut Kdb::Key.new("user:/app4")

      assert_equal 4, ks.size
      assert_equal 0, app4.size

    end
  end

  def test_keySet_to_array
    assert_nothing_raised do
      ks = Kdb::KeySet.new (0..5).map { |i| Kdb::Key.new "user:/key#{i}" }

      a = ks.to_a

      assert_instance_of Kdb::KeySet, ks
      assert_instance_of Array, a

      assert_equal ks.size, a.size

      for i in (0..5) do
        assert_equal ks[i], a[i]
      end

    end
  end

  def test_keySet_empty
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert ks.empty?

      ks << Kdb::Key.new("user:/k1")

      assert ! ks.empty?

      ks << Kdb::Key.new("user:/k2")

      assert ! ks.empty?

      ks.pop
      assert ! ks.empty?
      ks.pop
      assert ks.empty?
      ks.pop
      assert ks.empty?
    end
  end

  def test_keySet_length
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert_equal 0, ks.size
      assert_equal 0, ks.length

      ks << Kdb::Key.new("user:/k1")

      assert_equal 1, ks.size
      assert_equal 1, ks.length

      (2..10).map do |i|
          ks << Kdb::Key.new("user:/sw/org/my_app/k#{i}")
          assert_equal i, ks.size
          assert_equal i, ks.length
      end

      (0..9).reverse_each do |i|
          assert_not_nil ks.pop
          assert_equal i, ks.size
          assert_equal i, ks.length
      end

      # to be explicite
      assert_equal 0, ks.size
      assert_equal 0, ks.length

      assert_nil ks.pop
      assert_equal 0, ks.size
      assert_equal 0, ks.length
    end
  end

  def test_keySet_delete_by_index
    assert_nothing_raised do
      a = (0..9).map { |i|
        Kdb::Key.new "user:/sw/org/my_app/k#{i}"
      }

      ks = Kdb::KeySet.new a

      assert_equal 10, ks.size

      k = ks.delete_at 5
      ak = a.delete_at 5
      assert_equal 9, ks.size
      assert_equal ak, k


      k = ks.delete_at 0
      ak = a.delete_at 0
      assert_equal 8, ks.size
      assert_equal ak, k
      assert_equal a[0], ks[0]

      k = ks.delete_at(ks.size - 1)
      ak = a.delete_at(a.size - 1)
      assert_equal 7, ks.size
      assert_equal ak, k
      assert_equal a[a.size - 1], ks[ks.size-1]

      assert_nil ks.delete_at 10
    end
  end

  def test_keySet_delete_by_lookup
    assert_nothing_raised do
      a = (0..9).map { |i|
        Kdb::Key.new "user:/sw/org/my_app/k#{i}"
      }

      ks = Kdb::KeySet.new a

      assert_equal 10, ks.size

      ak = a.delete_at 5
      # delete by key
      k = ks.delete ak
      assert_equal ak, k
      assert_equal 9, ks.size

      ak = a.delete_at 0
      k = ks.delete ak
      assert_equal ak, k
      assert_equal 8, ks.size


      assert_nil ks.delete(Kdb::Key.new "user:/doesn_t_exist")

      # delete by name
      ak = a.delete_at 0
      k = ks.delete "user:/sw/org/my_app/k1"
      assert_equal ak, k
      assert_equal 7, ks.size

      ak = a.delete_at 6
      k = ks.delete "user:/sw/org/my_app/k9"
      assert_equal ak, k
      assert_equal 6, ks.size

      assert_nil ks.delete("user:/doesn_t_exist")

    end
  end

  def test_keySet_to_s
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert_equal '', ks.to_s

      ks << Kdb::Key.new("user:/k1", value: "v1")

      assert_equal "user:/k1: v1", ks.to_s

      ks << Kdb::Key.new("user:/k2", value: "v2")

      assert_equal "user:/k1: v1, user:/k2: v2", ks.to_s

      ks << Kdb::Key.new("user:/k3", flags: Kdb::KEY_BINARY, value: "\x00\x00")

      expected = "user:/k1: v1, user:/k2: v2, user:/k3: (binary) length: 2"
      assert_equal expected, ks.to_s
    end
  end

  def test_keySet_pretty_print
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      out, err = capture_output { ks.pretty_print }

      assert_equal '', out
      assert_equal '', err

      ks << Kdb::Key.new("user:/k1", value: "v1")
      ks << Kdb::Key.new("user:/k2", value: "v2")
      ks << Kdb::Key.new("user:/k3", value: "v3")

      out, err = capture_output { ks.pretty_print }

      expected = <<EOF
user:/k1: v1
user:/k2: v2
user:/k3: v3
EOF
      assert_equal expected, out
      assert_equal '', err
    end
  end

end
