#!/usr/bin/env ruby
##
# @file
#
# @brief unit test cases for Kdb::KDB
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#

require 'kdb'
require 'test/unit'
require_relative 'test_helper.rb'

# basename for all keys written to a configuration used in the following
# test cases
RB_TEST_NS = "/tests/ruby"
# key namespace use when creating new keys
NAMESPACE  = "user"

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

      assert ret >= 0

      ks << Kdb::Key.new("#{NAMESPACE}:#{RB_TEST_NS}/kdbgetset/c1", value: "v1")
      ks << Kdb::Key.new("#{NAMESPACE}:#{RB_TEST_NS}/kdbgetset/c2",
                         value: "v2", meta: "m2")

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
      assert_equal "#{NAMESPACE}:#{RB_TEST_NS}/kdbgetset/c1", k.name
      assert_equal "v1", k.value

      k = ks.lookup "#{RB_TEST_NS}/kdbgetset/c2", Kdb::KDB_O_POP
      assert_not_nil k
      assert_equal "#{NAMESPACE}:#{RB_TEST_NS}/kdbgetset/c2", k.name
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

  def test_kdb_module_open_without_error_key_and_block
    assert_nothing_raised do
      db = Kdb.open

      assert_instance_of Kdb::KDB, db
      assert_not_nil db

      # test if we really have a valid handle
      ks = Kdb::KeySet.new
      ret = db.get ks, "/"
      assert_equal 1, ret
      assert ks.size > 0

      # close has to be called explicitly
      db.close
    end
  end

  def test_kdb_module_open_without_block
    assert_nothing_raised do
      error_key = Kdb::Key.new

      db = Kdb.open error_key

      assert_instance_of Kdb::KDB, db
      assert_instance_of Kdb::Key, error_key
    end
  end

  def test_kdb_module_open_with_block
    assert_nothing_raised do
      # define local variable to have access to the block var
      db_access = nil

      block_ret = Kdb.open do |db|
        assert_instance_of Kdb::KDB, db
        assert_not_nil db

        db_access = db

        ks = Kdb::KeySet.new

        ret = db.get ks, "/"
        assert_equal 1, ret
        assert ks.size > 0
      end

      # if called with block, nil is returned
      assert_nil block_ret

      # test implicit close, we must not be able to get a keyset again
      assert_not_nil db_access
      assert_raise Kdb::KDBException do
        db_access.get Kdb::KeySet.new, "/"
      end
    end
  end

  def test_kdb_module_open_with_exception_in_block
    assert_nothing_raised do
      db_access = nil

      # block should pass the exception
      assert_raise Kdb::KDBException do
        Kdb.open do |db|
          db_access = db
          ks = Kdb::KeySet.new Kdb::Key.new("#{NAMESPACE}:#{RB_TEST_NS}/key1",
                                            value: "v1")
          # raises an exception
          db.set ks, RB_TEST_NS
        end
      end

      # however, the KDB handle shall be closed
      assert_not_nil db_access
      assert_raise Kdb::KDBException do
        db_access.get Kdb::KeySet.new, "/"
      end
    end
  end
end
