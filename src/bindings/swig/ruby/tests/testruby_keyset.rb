
require 'kdb'
require 'test/unit'


class KdbKeySetTestCases < Test::Unit::TestCase

  def test_keySet_new_simple
    assert_nothing_raised do
      ks = Kdb::KeySet.new

      assert_not_nil ks
      assert_instance_of Kdb::KeySet, ks

      assert_equal 0, ks.size

    end
  end

  #def test_keySet_new_alloc_size
  #  ks = Kdb::KeySet.new 100

  #  assert_not_nil ks
  #  assert_instance_of Kdb::KeySet, ks

  #end

  def test_keySet_append
    assert_nothing_raised do
      k = Kdb::Key.new "user/ks1", value: "val", meta: "metaval"

      ks = Kdb::KeySet.new

      num = ks.append k

      assert_equal 1, ks.size
      assert_equal 1, num
      
      assert_equal k, ks.head
      assert_equal k, ks.tail

      ks << Kdb::Key.new("user/ks2", value: "val2")

      assert_equal 2, ks.size

      assert_equal k, ks.head
      assert_equal "user/ks2", ks.tail.name

    end  
  end


  def test_keySet_each_enumeralbe
    assert_nothing_raised do
      a = Array.new
      # create test keys
      a << Kdb::Key.new("user/k0", value: "v0", owner: "me")
      a << Kdb::Key.new("user/k1", value: "v1", owner: "you")
      a << Kdb::Key.new("user/k2", value: "v2", owner: "john", m2: "a")
      a << Kdb::Key.new("user/k3", value: "v3", owner: "jane")
      a << Kdb::Key.new("user/k4", value: "v4", owner: "max")
      a << Kdb::Key.new("user/k5", value: "v5", owner: "bob", m2: "b")
      a << Kdb::Key.new("user/k6", value: "v6", owner: "alice")
      a << Kdb::Key.new("user/k7", value: "v7", owner: "fritz", m3: "c")
      a << Kdb::Key.new("user/k8", value: "v8", owner: "anton")
      a << Kdb::Key.new("user/k9", value: "v9", owner: "berta")

      ks = Kdb::KeySet.new

      # populate keySet
      a.each { |e| ks << e }

      assert_equal 10, ks.size

      # this will only work, if test keys have key names in desc order
      i = 0
      ks.each do |e| 
        assert_true a[i] == e
        i += 1
      end

      assert_equal 10, i

      
      # test Enumerable mixin
      assert_true ks.all? { |e| e.namespace == "user" }
      assert_true ks.all? { |e| e.has_meta? "owner" }
      assert_true ks.all? { |e| e.is_string? }

      assert_false ks.all? { |e| e.value == "v0" }
      assert_true ks.any? { |e| e.value == "v0" }

      k = ks.find { |e| e.name == "user/k5" }
      assert_instance_of Kdb::Key, k
      assert_true k.is_valid?
      assert_false k.is_null?
      assert_equal "user/k5", k.name
      assert_true a[5] == k

      k = ks.find { |e| e.name == "does_not_exist" }
      assert_nil k

      assert_equal 2, ks.count { |e| e.has_meta? "m2" }

      tmpa = ks.find_all { |e| e.has_meta? "m2" }
      assert_instance_of Array, tmpa
      assert_equal 2, tmpa.size
      #puts "name: #{tmpa[0].name}, value: #{tmpa[0].value}"
      #puts "name: #{tmpa[1].name}, value: #{tmpa[1].value}"
      assert_equal a[2], tmpa[0]
      assert_equal a[5], tmpa[1]


      # test Enumerable #min, #max
      assert_equal a[9], ks.max
      assert_equal a[0], ks.min

      # owner: you is maximum
      assert_equal a[1], ks.max_by { |e| e["owner"] }


      # our each impl makes inplace modifications
      ks.each { |e| e["new_meta"] = "persisted" }

      assert_true ks.all? { |e| e.has_meta? "new_meta" }
      assert_true ks.all? { |e| e["new_meta"] == "persisted" }

      # ensure KeySet cursor is unmodified after 'each' call
      ks.rewind
      assert_equal -1, ks.cursor
      ks.each { |e| e }
      assert_equal -1, ks.cursor

      ks.next
      assert_equal 0, ks.cursor
      ks.next
      assert_equal 1, ks.cursor
      ks.each { |e| e }
      assert_equal 1, ks.cursor

      ks.cursor = 5
      assert_equal 5, ks.cursor
      ks.each { |e| e }
      assert_equal 5, ks.cursor

    end
  end

end
