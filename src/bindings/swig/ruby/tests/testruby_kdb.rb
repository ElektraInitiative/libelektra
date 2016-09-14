
require 'kdb'
require 'test/unit'

RB_TEST_NS = "user/tests/swig_ruby"

class KdbTestCases < Test::Unit::TestCase
  
  def test_kdb_new_simple
    assert_nothing_raised do
      h = Kdb::KDB.new

      assert_instance_of Kdb::KDB, h
      assert_not_nil h
    end
  end

  def test_kdb_new_with_error_key
    assert_nothing_raised do
      e = Kdb::Key.new

      h = Kdb::KDB.new e

      assert_instance_of Kdb::KDB, h
      assert_not_nil h
    end
  end

  def test_kdb_get_set
    assert_nothing_raised do
      h = Kdb::KDB.new

      ks = Kdb::KeySet.new

      ret = h.get ks, RB_TEST_NS

      assert_true ret >= 0

      ks << Kdb::Key.new("#{RB_TEST_NS}/kdbgetset/c1", value: "v1")
      ks << Kdb::Key.new("#{RB_TEST_NS}/kdbgetset/c2", value: "v2", meta: "m2")
      
      ret = h.set ks, RB_TEST_NS

      assert_equal 1, ret

      h.close

      # close and reopen

      h = Kdb::KDB.new

      ks = Kdb::KeySet.new

      ret = h.get ks, RB_TEST_NS
      assert_equal 1, ret 

      # use KDB_O_POP to remove the keys from the KeySet
      k = ks.lookup "#{RB_TEST_NS}/kdbgetset/c1", Kdb::KDB_O_POP 
      assert_not_nil k
      assert_equal "#{RB_TEST_NS}/kdbgetset/c1", k.name
      assert_equal "v1", k.value

      k = ks.lookup "#{RB_TEST_NS}/kdbgetset/c2", Kdb::KDB_O_POP
      assert_not_nil k
      assert_equal "#{RB_TEST_NS}/kdbgetset/c2", k.name
      assert_equal "v2", k.value
      assert_equal "m2", k["meta"]

      # update db
      ret = h.set ks, RB_TEST_NS
      assert_equal 1, ret
    end
  end

  def test_kdb_throw_KDBException_on_set_before_get
    assert_raise Kdb::KDBException do
      h = Kdb::KDB.new

      ks = Kdb::KeySet.new

      ks << Kdb::Key.new("#{RB_TEST_NS}/myapp")

      h.set ks, RB_TEST_NS
    end
  end
end
