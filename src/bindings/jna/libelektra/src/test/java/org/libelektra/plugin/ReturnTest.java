package org.libelektra.plugin;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.libelektra.Key;
import org.libelektra.KeySet;

public class ReturnTest {

  static final String ERROR_KEY_NAME = "/temporary/errorkey";
  static final String ERROR_KEY_VALUE = "error";

  private Return returnPlugin;
  private Key errorKey;

  @Before
  public void setup() {
    returnPlugin = new Return();
    errorKey = Key.create(ERROR_KEY_NAME, ERROR_KEY_VALUE);
  }

  @Test
  public void test_returnOpen_shouldBeCode0() {
    int open = returnPlugin.open(KeySet.create(), errorKey);

    assertEquals(0, open);
  }

  @Test
  public void test_returnGet_ShouldBeCode10() {
    int open = returnPlugin.get(KeySet.create(), errorKey);

    assertEquals(10, open);
  }

  @Test
  public void test_returnSet_ShouldBeCode20() {
    int open = returnPlugin.set(KeySet.create(), errorKey);

    assertEquals(20, open);
  }

  @Test
  public void test_returnError_ShouldBeCode30() {
    int open = returnPlugin.error(KeySet.create(), errorKey);

    assertEquals(30, open);
  }

  @Test
  public void test_returnClose_ShouldBeCode0() {
    int open = returnPlugin.close(errorKey);

    assertEquals(0, open);
  }

  @Test
  public void test_returnGetName_ShouldBeReturn() {
    assertEquals("Return", returnPlugin.getName());
  }
}
