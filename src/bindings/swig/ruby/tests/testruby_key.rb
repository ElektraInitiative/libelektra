
require 'kdb'
require 'test/unit'


class KdbKeyTestCases < Test::Unit::TestCase
  
  def test_key_new_simple
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_not_nil k
      assert_instance_of Kdb::Key, k
      assert_false k.isValid

      kname = "user/tmp/k1"
      k = Kdb::Key.new kname
      assert_not_nil k
      assert_equal kname, k.getName
      assert_true k.isValid

      #puts "hello world - " + k.getName
    end
  end

  def test_key_new_valid_names
    assert_nothing_raised do
      assert_true Kdb::Key.new("/cascading/tmp/ks1").isValid
      assert_true Kdb::Key.new("spec/tmp/ks1").isValid
      assert_true Kdb::Key.new("proc/tmp/ks1").isValid
      assert_true Kdb::Key.new("dir/tmp/ks1").isValid
      assert_true Kdb::Key.new("user/tmp/ks1").isValid
      assert_true Kdb::Key.new("system/tmp/ks1").isValid
      assert_false Kdb::Key.new("invalidname").isValid
    end
  end

  def test_key_get_set_name
    assert_nothing_raised do
      k = Kdb::Key.new
      assert_equal "", k.getName

      name = "user/tmp/k1"
      k.setName name
      assert_equal name, k.getName
      assert_equal name.split('/').reverse[0], k.getBaseName

      k.addBaseName "b1"
      assert_equal "b1", k.getBaseName
      assert_equal "#{name}/b1", k.getFullName

      k.addBaseName "bb2"
      assert_equal "bb2", k.getBaseName
      assert_equal "#{name}/b1/bb2", k.getFullName

      k.addName "n1/n2"
      assert_equal "n2", k.getBaseName
      assert_equal "#{name}/b1/bb2/n1/n2", k.getFullName

      k.addName "../../../../new1"
      assert_equal "new1", k.getBaseName
      assert_equal "#{name}/new1", k.getFullName
    end
  end

  def throw_KeyInvalidName_exception_invalid_name 
    assert_raised Kdb::KeyInvalidName do
      k = Kdb.Key.new
      k.setName "invalidname"
    end
  end


end
