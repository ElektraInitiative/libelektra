
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
end
