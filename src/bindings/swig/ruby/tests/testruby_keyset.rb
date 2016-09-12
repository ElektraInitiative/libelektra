
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

  def test_keySet_new_with_key
    assert_nothing_raised do
      k = Kdb::Key.new "user/key", value: "hello"
      ks = Kdb::KeySet.new k

      assert_equal 1, ks.size
      assert_equal k, ks.head
    end
  end

  def test_keySet_new_with_invalid_argument
    assert_raise ArgumentError do
      ks = Kdb::KeySet.new "not a key"
    end
  end

  def test_keySet_new_with_keySet
    assert_nothing_raised do
      ks1 = Kdb::KeySet.new
      ks1 << Kdb::Key.new("user/ks1")
      ks1 << Kdb::Key.new("user/ks2")

      ks2 = Kdb::KeySet.new ks1

      assert_equal 2, ks2.size

      ks2 << Kdb::Key.new("user/ks3")

      assert_equal 3, ks2.size
      assert_equal "user/ks3", ks2.tail.name
      
      # ensure old KeySet holds only the first 2 Keys
      assert_equal 2, ks1.size
      assert_equal "user/ks2", ks1.tail.name
    end
  end

  def test_keySet_new_with_array
    assert_nothing_raised do
      a = Array.new
      a << Kdb::Key.new("user/ks1")
      a << Kdb::Key.new("user/ks2")
      a << Kdb::Key.new("user/ks3")
      a << Kdb::Key.new("user/ks4")

      ks = Kdb::KeySet.new a

      assert_equal 4, ks.size
      i = 0
      ks.each { |e|
        assert_equal a[i], e
        i += 1
      }

      ks = Kdb::KeySet.new [
        Kdb::Key.new("user/key1"),
        Kdb::Key.new("user/key2"),
        Kdb::Key.new("user/key3")
      ]

      assert_equal 3, ks.size
      assert_equal "key1", ks.head.base_name

      # ensure also larger arrays, with more than 16 (preallocated) elements
      # work correctly
      a = (1..40).map { |i| Kdb::Key.new("user/key%02d" % i) }
      ks = Kdb::KeySet.new a

      assert_equal 40, ks.size
      assert_equal "key40", ks.tail.base_name
    end

    assert_raise ArgumentError do
      ks = Kdb::KeySet.new [
        Kdb::Key.new("user/key"),
        "not a key",
        1
      ]
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

  def test_keySet_append_KeySet
    assert_nothing_raised do
      ks1 = Kdb::KeySet.new

      ks1 << Kdb::Key.new("user/ks1")
      ks1 << Kdb::Key.new("user/ks2")

      ks2 = Kdb::KeySet.new

      ks2 << Kdb::Key.new("user/ks3")
      ks2 << Kdb::Key.new("user/ks4")

      ks1 << ks2

      assert_equal 4, ks1.size
      assert_equal 2, ks2.size
    end
  end

  def test_keySet_append_array
    assert_nothing_raised do
      a = Array.new
      a << Kdb::Key.new("user/ks2")
      a << Kdb::Key.new("user/ks1")

      ks = Kdb::KeySet.new

      ks.append a

      assert_equal 2, ks.size
      assert_equal a[0], ks[1]
      assert_equal a[1], ks[0]
    end

    assert_nothing_raised do
      ks = Kdb::KeySet.new

      ks << [
        Kdb::Key.new("user/ks1"),
        Kdb::Key.new("user/ks2"),
        Kdb::Key.new("user/ks3")
      ]

      assert_equal 3, ks.size
      assert_equal "user/ks1", ks.head.name
    end

    assert_raise ArgumentError do
      a = Array.new
      a << "not a Key"
      a << 1

      ks = Kdb::KeySet.new

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
      ks = Kdb::KeySet.new (0..9).map { |i| Kdb::Key.new "user/key%02d" % i }

      assert_equal 10, ks.size

      # test get by index
      for i in (0..9) do
        assert_equal ("user/key%02d" % i), ks.at(i).name
        assert_equal ("user/key%02d" % i), ks[i].name
      end

      # test get by cursor
      ks.rewind
      puts "cursor at: %d" % ks.cursor
      assert_true ks.cursor < 0

      assert_nil ks.current

      for i in (0..9) do
        nxt = ks.next
        cur = ks.current
        assert_equal ("user/key%02d" % i), nxt.name
        assert_equal cur, nxt
        
        assert_equal i, ks.cursor
      end

      assert_nil ks.next
      assert_nil ks.current

      puts "cursor at: %d" % ks.cursor
      assert_true ks.cursor < 0

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

      k = Kdb::Key.new("user/k0")
      ks << k
      assert_equal 1, ks.size
      assert_equal k, ks.pop
      assert_equal 0, ks.size

      k = Kdb::Key.new("user/k1")
      ks << k
      assert_equal 1, ks.size
      assert_equal k, ks.pop
      assert_equal 0, ks.size

      k2 = Kdb::Key.new("user/k2")
      k3 = Kdb::Key.new("user/k3")
      k4 = Kdb::Key.new("user/k4")
      k5 = Kdb::Key.new("user/k5")

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

      assert_nil ks.head
      assert_nil ks.tail

      a = (0..3).map { |i| Kdb::Key.new "user/key#{i}" }

      ks << a[0]

      assert_equal a[0], ks.head
      assert_equal a[0], ks.tail

      ks << a[1]

      assert_equal a[0], ks.head
      assert_equal a[1], ks.tail

      ks << a[2]

      assert_equal a[0], ks.head
      assert_equal a[2], ks.tail

      ks << a[3]

      assert_equal a[0], ks.head
      assert_equal a[3], ks.tail

      assert_equal a[3], ks.pop
      assert_equal a[2], ks.tail

      assert_equal a[2], ks.pop
      assert_equal a[1], ks.tail

      assert_equal a[1], ks.pop
      assert_equal a[0], ks.tail

      assert_equal a[0], ks.pop
      assert_nil ks.tail
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

  def test_keySet_comparison
    assert_nothing_raised do
      a = (1..5).map { |i| Kdb::Key.new "user/key#{i}" }

      ks1 = Kdb::KeySet.new a
      ks2 = Kdb::KeySet.new a

      assert_equal ks1.size, ks2.size
      assert_equal ks1[0], ks2[0]
      assert_equal ks1[1], ks2[1]
      assert_equal ks1[2], ks2[2]
      assert_equal ks1[3], ks2[3]
      assert_equal ks1[4], ks2[4]

      assert_true ks1 == ks2
      assert_false ks1 != ks2

      ks2 << Kdb::Key.new("user/key100")

      assert_false ks1 == ks2
      assert_true ks1 != ks2
    end
  end

  def test_keySet_lookup_lookupByName
    assert_nothing_raised do
      ks = Kdb::KeySet.new (1..10).map { |i| Kdb::Key.new("user/key%02d" % i) }

      assert_equal 10, ks.size

      # lookupByName
      assert_equal "user/key01", ks.lookup("user/key01").name
      assert_equal Kdb::Key.new("user/key02"), ks.lookup("user/key02")

      # lookup
      assert_equal "user/key03", ks.lookup(Kdb::Key.new "user/key03").name

      # lookup unknown key
      assert_nil ks.lookup("user/key_now_in_keyset")
      assert_nil ks.lookup(Kdb::Key.new "user/key_now_in_keyset")

      # with options
      #lookupkey = Kdb::Key.new "user/key04"
      #assert_equal lookupkey, ks.lookup(lookupkey, Kdb::KDB_O_DEL)
      #assert_true lookupkey.is_null? # TODO: should this really work this way

      lookupkey = Kdb::Key.new "user/key05"
      assert_equal lookupkey, ks.lookup(lookupkey, Kdb::KDB_O_POP)
      assert_equal 9, ks.size
      assert_nil ks.lookup(lookupkey)

    end
  end

end
