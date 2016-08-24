
require 'kdb'
require 'test/unit'


class KdbKeyTestCases < Test::Unit::TestCase
  
  def test_key_new_simple
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_not_nil k
      assert_instance_of Kdb::Key, k
      assert_false k.is_valid?

      kname = "user/tmp/k1"
      k = Kdb::Key.new kname
      assert_not_nil k
      assert_equal kname, k.name
      assert_true k.is_valid?

      #puts "hello world - " + k.name
    end
  end

  def test_key_new_valid_names
    assert_nothing_raised do
      assert_true Kdb::Key.new("/cascading/tmp/ks1").is_valid?
      assert_true Kdb::Key.new("spec/tmp/ks1").is_valid?
      assert_true Kdb::Key.new("proc/tmp/ks1").is_valid?
      assert_true Kdb::Key.new("dir/tmp/ks1").is_valid?
      assert_true Kdb::Key.new("user/tmp/ks1").is_valid?
      assert_true Kdb::Key.new("system/tmp/ks1").is_valid?
      assert_false Kdb::Key.new("invalidname").is_valid?
    end
  end

  def test_key_new_with_options
    assert_nothing_raised do
      name = "user/tmp/ks1"
      k = Kdb::Key.new name
      assert_true k.is_valid?
      assert_equal name, k.name
      assert_equal "", k.get_string # TODO: should we return nil here

      v = "val"
      k = Kdb::Key.new name, value: v
      assert_true k.is_valid?
      assert_equal name, k.name
      assert_equal v, k.get_string

      k = Kdb::Key.new name, value: v, owner: "me", comment: "ccc"
      assert_true k.is_valid?
      assert_equal name, k.name
      assert_equal v, k.get_string
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k.get_meta(:owner)
      assert_equal "ccc", k.get_meta("comment")
      assert_equal "ccc", k.get_meta(:comment)

      k = Kdb::Key.new(name, 
                       owner: "me", 
                       comment: "ccc",
                       flags: Kdb::KEY_BINARY)
      assert_true k.is_valid?
      assert_true k.is_binary?
      assert_equal name, k.name
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k.get_meta(:owner)
      assert_equal "ccc", k.get_meta("comment")
      assert_equal "ccc", k.get_meta(:comment)
      assert_equal "", k.get_meta("flags")
      assert_equal "", k.get_meta(:flags)

    end
  end

  def test_key_get_set_meta
    assert_nothing_raised do
      k = Kdb::Key.new "user/asdf"

      assert_true k.is_valid?

      k.set_meta "comment", "hello"
      assert_equal "hello", k.get_meta("comment")
      assert_equal "hello", k["comment"]

      k["owner"] = "me"
      assert_equal "me", k.get_meta("owner")
      assert_equal "me", k["owner"]

    end
  end


  def test_key_get_name 
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_equal "", k.name

      name = "user/tmp/k1"
      k.name = name
      assert_equal name, k.name
      assert_equal name.split('/').reverse[0], k.get_base_name

      k.add_base_name "b1"
      assert_equal "b1", k.get_base_name
      assert_equal "#{name}/b1", k.get_full_name

      k.add_base_name "bb2"
      assert_equal "bb2", k.get_base_name
      assert_equal "#{name}/b1/bb2", k.get_full_name

      k.add_name "n1/n2"
      assert_equal "n2", k.get_base_name
      assert_equal "#{name}/b1/bb2/n1/n2", k.get_full_name

      k.add_name "../../../../new1"
      assert_equal "new1", k.get_base_name
      assert_equal "#{name}/new1", k.get_full_name
    end
  end

  def throw_KeyInvalidName_exception_invalid_name 
    assert_raised Kdb::KeyInvalidName do
      k = Kdb.Key.new
      k.name = "invalidname"
    end
  end


end
