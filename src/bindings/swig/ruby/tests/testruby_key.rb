
require 'kdb'
require 'test/unit'


class KdbKeyTestCases < Test::Unit::TestCase
  
  def test_key_new_simple
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_not_nil k
      assert_instance_of Kdb::Key, k
      assert ! k.is_valid?

      kname = "user/tmp/k1"
      k = Kdb::Key.new kname
      assert_not_nil k
      assert_equal kname, k.name
      assert k.is_valid?

      #puts "hello world - " + k.name
    end
  end

  def test_key_new_valid_names
    assert_nothing_raised do
      assert Kdb::Key.new("/cascading/tmp/ks1").is_valid?
      assert Kdb::Key.new("spec/tmp/ks1").is_valid?
      assert Kdb::Key.new("proc/tmp/ks1").is_valid?
      assert Kdb::Key.new("dir/tmp/ks1").is_valid?
      assert Kdb::Key.new("user/tmp/ks1").is_valid?
      assert Kdb::Key.new("system/tmp/ks1").is_valid?
      assert ! Kdb::Key.new("invalidname").is_valid?
    end
  end

  def test_key_new_with_options
    assert_nothing_raised do
      name = "user/tmp/ks1"
      k = Kdb::Key.new name
      assert k.is_valid?
      assert_equal name, k.name
      assert_equal "", k.get_string # TODO: should we return nil here

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

      name = "user/tmp/k1"
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
      k = Kdb::Key.new "user/asdf"

      assert k.is_valid?

      k.set_meta "comment", "hello"
      assert_equal "hello", k.get_meta("comment")
      assert_equal "hello", k["comment"]

      k["owner"] = "me"
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k["owner"]

    end
  end

  def test_key_get_invalid_meta
    k = Kdb::Key.new "user/asdf"

    assert_equal "", k["does_not_exist"]

  end


  def test_key_get_name 
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_equal "", k.name

      name = "user/tmp/k1"
      k.name = name
      assert_equal name, k.name
      assert_equal name.split('/').reverse[0], k.base_name
      assert_equal "user", k.namespace

      k.add_base_name "b1"
      assert_equal "b1", k.base_name
      assert_equal "#{name}/b1", k.full_name

      k.add_base_name "bb2"
      assert_equal "bb2", k.base_name
      assert_equal "#{name}/b1/bb2", k.full_name

      k.add_name "n1/n2"
      assert_equal "n2", k.base_name
      assert_equal "#{name}/b1/bb2/n1/n2", k.full_name

      k.add_name "../../../../new1"
      assert_equal "new1", k.base_name
      assert_equal "#{name}/new1", k.full_name
    end
  end

  def test_throw_KeyInvalidName_exception_invalid_name 
    k = Kdb::Key.new

    assert_raise Kdb::KeyInvalidName do
      k.name = "invalidname"
    end
    
    assert_raise Kdb::KeyInvalidName do
      k.add_name "x"
    end
    
    assert_raise Kdb::KeyInvalidName do
      k.base_name= "x"
    end
    
    assert_raise Kdb::KeyInvalidName do
      k.add_base_name "x"
    end
    
    assert_nothing_raised do
      k.name = "user/keyname"
      assert_equal "user/keyname", k.name
    end


  end

  def test_throw_KeyTypeMismatch_on_wrong_get_method
    k = Kdb::Key.new "user/mykey"
    k.set_binary "\000\001\002"
    
    assert k.is_binary?
   
    assert_raise Kdb::KeyTypeMismatch do
      k.get_string
    end

    k = Kdb::Key.new "user/mykey2"
    k.set_string "hello world"

    assert ! k.is_binary?

    assert_raise Kdb::KeyTypeMismatch do
      k.get_binary
    end
  end

  def test_KeyName_comperators
    assert_nothing_raised do
      k1 = Kdb::Key.new "user/test"
      k2 = Kdb::Key.new "user/test"

      assert k1.name == k2.name
      assert k1 == k2
      assert k1 <= k2
      assert k1 >= k2

      assert_equal 0, k1 <=> k2


      k2 = Kdb::Key.new "user/anothertest"

      assert k1.name != k2.name
      assert k1 != k2

      assert k1 > k2
      assert k2 < k1

      assert_equal -1, k2 <=> k1
      assert_equal 1, k1 <=> k2

    end
  end

  def test_KeyName_hierarchy
    assert_nothing_raised do
      k1 = Kdb::Key.new "user/app"
      k2 = Kdb::Key.new "user/app/v1"
      k3 = Kdb::Key.new "user/app/v1/conf1"
      
      assert k1.is_valid?
      assert k2.is_valid?
      assert k3.is_valid?

      assert k2.is_below? k1
      assert k2.is_direct_below? k1

    end
  end

  def test_Key_cloning
    k = Kdb::Key.new "user/key1", value: "hello", meta1: "mv"
    
    assert k.is_valid?
    assert_equal "user/key1", k.name
    assert_equal "hello", k.value
    assert_equal "mv", k["meta1"]

    kc = k.clone

    assert k.__id__ != kc.__id__
    assert_instance_of Kdb::Key, kc
    assert_equal "user/key1", kc.name
    assert_equal "hello", kc.value
    assert_equal "mv", kc["meta1"]

    kc.name = "user/key2"
    kc.value = "world"
    kc["meta1"] = "vm"
    kc["meta2"] = "vm2"

    assert_equal "user/key1", k.name
    assert_equal "hello", k.value
    assert_equal "mv", k["meta1"]
    assert ! k.has_meta?("meta2")

    assert_equal "user/key2", kc.name
    assert_equal "world", kc.value
    assert_equal "vm", kc["meta1"]
    assert_equal "vm2", kc["meta2"]

    kc2 = kc.dup

    assert kc.__id__ != kc2.__id__
    assert k.__id__ != kc2.__id__

    kc2.name = "user/key3"
    kc2.value = "wonderful"

    assert_equal "user/key2", kc.name
    assert_equal "world", kc.value

    assert_equal "user/key3", kc2.name
    assert_equal "wonderful", kc2.value

  end

  def test_meta_iterator
    assert_nothing_raised do
      k = Kdb::Key.new("user/asdf",
                       owner: "me",
                       comment: "hello",
                       meta1: "meta1 value",
                       othermeta: "othermeta value")

      assert_equal "me", k["owner"]
      assert_equal "hello", k[:comment]
      assert_equal "meta1 value", k["meta1"]
      assert_equal "othermeta value", k["othermeta"]
     
      k.rewind_meta
      mk = k.current_meta
      assert_nil k.current_meta

      mk = k.next_meta
      assert_instance_of Kdb::Key, mk
      assert_equal "hello", mk.value
      assert_equal "comment", mk.name
      assert_equal "hello", k.current_meta.value
      assert_equal "comment", k.current_meta.name

      mk = k.next_meta
      assert_equal "meta1 value", mk.value
      assert_equal "meta1", mk.name
      assert_equal "meta1 value", k.current_meta.value
      assert_equal "meta1", k.current_meta.name

      mk = k.next_meta
      assert_equal "othermeta value", mk.value
      assert_equal "othermeta", mk.name
      assert_equal "othermeta value", k.current_meta.value
      assert_equal "othermeta", k.current_meta.name

      mk = k.next_meta
      assert_equal "me", mk.value
      assert_equal "owner", mk.name
      assert_equal "me", k.current_meta.value
      assert_equal "owner", k.current_meta.name

      assert_nil k.next_meta
      assert_nil k.current_meta

      # test Ruby-style meta iterator
      assert_instance_of Kdb::KeySet, k.meta

      a = ["comment", "meta1", "othermeta", "owner"]
      i = 0;
      k.meta.each { |m|
        assert_equal a[i], m.name
        i += 1
      }
    end
  end
end
