package picante.spice.kernelpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.spice.kernelpool.AssertionUtilities.assertPoolEquality;
import static picante.spice.kernelpool.AssertionUtilities.assertPoolStateOK;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;

/**
 * J-Unit test framework for KernelPool.
 */
public class BasicKernelPoolTest {

  BasicKernelPool pool;

  ArrayList<String> stringsA = new ArrayList<String>();
  ArrayList<Double> doublesA = new ArrayList<Double>();
  ArrayList<String> stringsB = new ArrayList<String>();
  ArrayList<Double> doublesB = new ArrayList<Double>();
  ArrayList<Double> doublesC = new ArrayList<Double>();
  ArrayList<Integer> integersA = new ArrayList<Integer>();
  ArrayList<Integer> integersB = new ArrayList<Integer>();
  ArrayList<Integer> integersC = new ArrayList<Integer>();

  @Before
  public void setUp() throws Exception {

    pool = new BasicKernelPool();

    stringsA.add("a");
    stringsA.add("b");
    stringsA.add("c");

    doublesA.add(10.0);
    doublesA.add(11.0);
    doublesA.add(12.0);

    integersA.add(10);
    integersA.add(11);
    integersA.add(12);

    stringsB.add("d");
    stringsB.add("e");

    doublesB.add(-10.0);
    doublesB.add(-11.0);
    integersB.add(-10);
    integersB.add(-11);

    doublesC.add(-10.5);
    doublesC.add(-10.4);
    doublesC.add(-10.6);
    integersC.add(-10);
    integersC.add(-10);
    integersC.add(-11);
  }

  @Test
  public void testBasicKernelPool() {

    /*
     * The default constructor just creates an empty pool.
     */
    pool = new BasicKernelPool();

    Set<String> keywords = pool.getKeywords();

    assertEquals(0, keywords.size());

  }

  @Test
  public void testBasicKernelPoolBasicKernelPool() {

    /*
     * Buildup a simple pool and copy it.
     */
    pool.addStrings("stringsA", stringsA);
    pool.addStrings("stringsB", stringsB);
    pool.addDoubles("doublesA", doublesA);
    pool.addDoubles("doublesB", doublesB);

    assertPoolStateOK(pool);

    BasicKernelPool copy = new BasicKernelPool(pool);

    assertPoolStateOK(copy);

    /*
     * Now verify that pool and copy are the same.
     */
    assertPoolEquality(pool, copy);

    /*
     * Lastly verify that copy is TRULY a copy of pool, that is to say a deep copy.
     */
    Set<String> keywords = pool.getKeywords();

    for (String key : keywords) {
      if (copy.isDoubleValued(key)) {
        List<Double> cTest = copy.getDoubles(key);
        List<Double> pTest = pool.getDoubles(key);
        assertNotSame(cTest, pTest);
      } else {
        List<String> cTest = copy.getStrings(key);
        List<String> pTest = pool.getStrings(key);
        assertNotSame(cTest, pTest);
      }
    }

  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAddStringsNullEntryInList() {
    ArrayList<String> list = new ArrayList<String>();
    list.add(null);
    pool.addStrings("NULLTEST", list);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAppendStringsNullEntryInList() {
    ArrayList<String> list = new ArrayList<String>();
    list.add(null);
    pool.appendStrings("NULLTEST", list);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAddDoublesNullEntryInList() {
    ArrayList<Double> list = new ArrayList<Double>();
    list.add(null);
    pool.addDoubles("NULLTEST", list);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAppendDoublesNullEntryInList() {
    ArrayList<Double> list = new ArrayList<Double>();
    list.add(null);
    pool.appendDoubles("NULLTEST", list);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAddIntegersNullEntryInList() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(null);
    pool.addIntegers("NULLTEST", list);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testAppendIntegersNullEntryInList() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(null);
    pool.appendIntegers("NULLTEST", list);
  }

  @Test
  public void testAddStrings() {

    /*
     * First exercise the null key insertion UnsupportedOperationException.
     */
    try {
      pool.addStrings(null, stringsA);
      fail("Expected UnsupportedOperationException not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null keys into"));
    }

    assertPoolStateOK(pool);

    /*
     * Next exercise the null value insertion UnsupportedOperationException.
     */
    try {
      pool.addStrings("TEST", null);
      fail("Expected Illegal Argument Exception not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null values into"));
    }

    assertPoolStateOK(pool);

    /*
     * The last of the exceptions to validate is the UnsupportedOperationException thrown when the
     * supplied values List is empty.
     */
    try {
      pool.addStrings("TEST", new ArrayList<String>());
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("There must be at " + "least one value"));
    }

    assertPoolStateOK(pool);

    /*
     * Insert some strings into the kernel pool.
     */
    pool.addStrings("A", stringsA);
    pool.addStrings("B", stringsB);

    assertPoolStateOK(pool);

    /*
     * Test the retrieval of these strings under nominal conditions.
     */
    List<String> list = pool.getStrings("A");

    assertEquals(3, list.size());
    assertEquals("a", list.get(0));
    assertEquals("b", list.get(1));
    assertEquals("c", list.get(2));

    /*
     * Verify that the list returned is not the same as the list we inserted.
     */
    assertNotSame(stringsA, list);

    list = pool.getStrings("B");

    assertEquals(2, list.size());
    assertEquals("d", list.get(0));
    assertEquals("e", list.get(1));

    /*
     * Verify that the list returned is not the same as the list we inserted.
     */
    assertNotSame(stringsB, list);

    /*
     * Now insert stringsB over top of "A". This should result in there being two copies of stringsB
     * associated with both keys "A" and "B".
     */
    pool.addStrings("A", stringsB);

    assertPoolStateOK(pool);

    list = pool.getStrings("A");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals("d", list.get(0));
    assertEquals("e", list.get(1));

    list = pool.getStrings("B");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals("d", list.get(0));
    assertEquals("e", list.get(1));

    /*
     * The very last test we need to make is to verify that if we use the addStrings method to
     * replace a key that exists with numeric data.
     */
    pool.addDoubles("dA", doublesA);

    assertPoolStateOK(pool);

    /*
     * Check that the add worked, otherwise this is moot.
     */
    assertNotNull(pool.getDoubles("dA"));

    /*
     * Replace dA with String data.
     */
    pool.addStrings("dA", stringsA);

    assertPoolStateOK(pool);

    /*
     * And verify that querying the key numerically and string-wise result in the proper response.
     */
    assertNull(pool.getDoubles("dA"));

    list = pool.getStrings("dA");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals("a", list.get(0));
    assertEquals("b", list.get(1));
    assertEquals("c", list.get(2));

    assertNotSame(stringsA, list);
  }

  @Test
  public void testAppendStrings() {

    /*
     * First exercise the null key insertion UnsupportedOperationException.
     */
    try {
      pool.appendStrings(null, stringsA);
      fail("Expected UnsupportedOperationException not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null keys into"));
    }

    assertPoolStateOK(pool);

    /*
     * Next exercise the null value insertion UnsupportedOperationException.
     */
    try {
      pool.appendStrings("TEST", null);
      fail("Expected Illegal Argument Exception not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null values into"));
    }

    assertPoolStateOK(pool);

    /*
     * The last of the exceptions to validate is the UnsupportedOperationException thrown when the
     * supplied values List is empty.
     */
    try {
      pool.appendStrings("TEST", new ArrayList<String>());
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("There must be at " + "least one value"));
    }

    assertPoolStateOK(pool);

    /*
     * Now setup the special append UnsupportedOperationException that results when attempting to
     * append string data to a key that already contains numeric data.
     */
    pool.addDoubles("dA", doublesA);

    assertPoolStateOK(pool);

    try {
      pool.appendStrings("dA", stringsA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Unable to append " + "String values into key"));
    }

    assertPoolStateOK(pool);

    /*
     * First check to see that if we attempt to append to a key that does not exist it gets created
     * and the proper content is added.
     */
    assertNull(pool.getStrings("A"));
    assertNull(pool.getDoubles("A"));

    pool.appendStrings("A", stringsA);

    assertPoolStateOK(pool);

    List<String> list = pool.getStrings("A");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals("a", list.get(0));
    assertEquals("b", list.get(1));
    assertEquals("c", list.get(2));

    /*
     * Verify that the returned list is not the one we supplied to the append method.
     */
    assertNotSame(list, stringsA);

    /*
     * Now since "A" is associated with string data, append the contents of stringsB to the list.
     */
    pool.appendStrings("A", stringsB);

    assertPoolStateOK(pool);

    /*
     * Check that this resulted in the correct action.
     */
    list = pool.getStrings("A");

    assertNotNull(list);
    assertEquals(5, list.size());
    assertEquals("a", list.get(0));
    assertEquals("b", list.get(1));
    assertEquals("c", list.get(2));
    assertEquals("d", list.get(3));
    assertEquals("e", list.get(4));

    assertNotSame(list, stringsB);

    /*
     * Check to see that if we supply the same list as input to two newly created keywords, then we
     * don't end up intermingling the lists. The old version of the kernel pool didn't make deep
     * copies of the supplied lists, so this would have happened.
     */
    pool.addStrings("X-TEST", stringsB);
    pool.addStrings("X-TEST2", stringsB);

    pool.appendStrings("X-TEST2", stringsA);

    assertPoolStateOK(pool);

    list = pool.getStrings("X-TEST");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals("d", list.get(0));
    assertEquals("e", list.get(1));

    list = pool.getStrings("X-TEST2");

    assertNotNull(list);
    assertEquals(5, list.size());
    assertEquals("d", list.get(0));
    assertEquals("e", list.get(1));
    assertEquals("a", list.get(2));
    assertEquals("b", list.get(3));
    assertEquals("c", list.get(4));
  }

  @Test
  public void testAddDoubles() {

    /*
     * First exercise the null key insertion UnsupportedOperationException.
     */
    try {
      pool.addDoubles(null, doublesA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null keys into"));
    }

    assertPoolStateOK(pool);

    /*
     * Next exercise the null value insertion UnsupportedOperationException.
     */
    try {
      pool.addDoubles("TEST", null);
      fail("Expected Illegal Argument Exception not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null values into"));
    }

    assertPoolStateOK(pool);

    /*
     * The last of the exceptions to validate is the UnsupportedOperationException thrown when the
     * supplied values List is empty.
     */
    try {
      pool.addDoubles("TEST", new ArrayList<Double>());
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("There must be at " + "least one value"));
    }

    assertPoolStateOK(pool);

    /*
     * Insert some doubles into the kernel pool.
     */
    pool.addDoubles("dA", doublesA);
    pool.addDoubles("dB", doublesB);

    assertPoolStateOK(pool);

    /*
     * Test the retrieval of these strings under nominal conditions.
     */
    List<Double> list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);

    /*
     * Verify that the list we supplied is not the one returned.
     */
    assertNotSame(list, doublesA);

    list = pool.getDoubles("dB");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    /*
     * Verify that the list we supplied is not the one returned.
     */
    assertNotSame(list, doublesB);

    /*
     * Now insert doublesB over top of "dA". This should result in there being two copies of
     * doublesB associated with both keys "dA" and "dB".
     */
    pool.addDoubles("dA", doublesB);

    assertPoolStateOK(pool);

    list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    assertNotSame(list, doublesB);

    list = pool.getDoubles("dB");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    assertNotSame(list, doublesB);

    /*
     * The very last test we need to make is to verify that if we use the addDoubles method to
     * replace a key that exists with string data.
     */
    pool.addStrings("A", stringsA);

    assertPoolStateOK(pool);

    /*
     * Check that the add worked, otherwise this is moot.
     */
    assertNotNull(pool.getStrings("A"));

    /*
     * Replace "A" with numeric data.
     */
    pool.addDoubles("A", doublesA);

    assertPoolStateOK(pool);

    /*
     * And verify that querying the key numerically and string-wise produce the proper results.
     */
    assertNull(pool.getStrings("A"));

    list = pool.getDoubles("A");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);

    assertNotSame(list, doublesA);
  }

  @Test
  public void testAppendDoubles() {

    /*
     * First exercise the null key insertion UnsupportedOperationException.
     */
    try {
      pool.appendDoubles(null, doublesA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null keys into"));
    }

    assertPoolStateOK(pool);

    /*
     * Next exercise the null value insertion UnsupportedOperationException.
     */
    try {
      pool.appendDoubles("TEST", null);
      fail("Expected Illegal Argument Exception not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null values into"));
    }

    assertPoolStateOK(pool);

    /*
     * The last of the exceptions to validate is the UnsupportedOperationException thrown when the
     * supplied values List is empty.
     */
    try {
      pool.appendDoubles("TEST", new ArrayList<Double>());
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("There must be at " + "least one value"));
    }

    assertPoolStateOK(pool);

    /*
     * Now setup the special append UnsupportedOperationException that results when attempting to
     * append numeric data to a key that already contains string data.
     */
    pool.addStrings("A", stringsA);

    assertPoolStateOK(pool);

    try {
      pool.appendDoubles("A", doublesA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Unable to append " + "Double values into key"));
    }

    assertPoolStateOK(pool);

    /*
     * First check to see that if we attempt to append to a key that does not exist it gets created
     * and the proper content is added.
     */
    assertNull(pool.getDoubles("dA"));
    assertNull(pool.getStrings("dA"));

    pool.appendDoubles("dA", doublesA);

    assertPoolStateOK(pool);

    List<Double> list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);

    assertNotSame(list, doublesA);

    /*
     * Now since "dA" is associated with numeric data, append the contents of doublesB to the list.
     */
    pool.appendDoubles("dA", doublesB);

    assertPoolStateOK(pool);

    /*
     * Check that this resulted in the correct action.
     */
    list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(5, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);
    assertEquals(-10.0, list.get(3), 0.0);
    assertEquals(-11.0, list.get(4), 0.0);

    assertNotSame(list, doublesB);

    /*
     * Check to see that if we supply the same list as input to two newly created keywords, then we
     * don't end up intermingling the lists. The old version of the kernel pool didn't make deep
     * copies of the supplied lists, so this would have happened.
     */
    pool.addDoubles("X-TEST", doublesB);
    pool.addDoubles("X-TEST2", doublesB);

    pool.appendDoubles("X-TEST2", doublesA);

    assertPoolStateOK(pool);

    list = pool.getDoubles("X-TEST");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    list = pool.getDoubles("X-TEST2");

    assertNotNull(list);
    assertEquals(5, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);
    assertEquals(10.0, list.get(2), 0.0);
    assertEquals(11.0, list.get(3), 0.0);
    assertEquals(12.0, list.get(4), 0.0);

  }

  @Test
  public void testAddIntegers() {

    /*
     * First exercise the null key insertion UnsupportedOperationException.
     */
    try {
      pool.addIntegers(null, integersA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null keys into"));
    }

    assertPoolStateOK(pool);

    /*
     * Next exercise the null value insertion UnsupportedOperationException.
     */
    try {
      pool.addIntegers("TEST", null);
      fail("Expected Illegal Argument Exception not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null values into"));
    }

    assertPoolStateOK(pool);

    /*
     * The last of the exceptions to validate is the UnsupportedOperationException thrown when the
     * supplied values List is empty.
     */
    try {
      pool.addIntegers("TEST", new ArrayList<Integer>());
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("There must be at " + "least one value"));
    }

    assertPoolStateOK(pool);

    /*
     * Insert some doubles into the kernel pool.
     */
    pool.addIntegers("dA", integersA);
    pool.addIntegers("dB", integersB);

    assertPoolStateOK(pool);

    /*
     * Test the retrieval of these strings under nominal conditions.
     */
    List<Double> list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);

    /*
     * Verify that the list we supplied is not the one returned.
     */
    assertNotSame(list, doublesA);

    list = pool.getDoubles("dB");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    /*
     * Verify that the list we supplied is not the one returned.
     */
    assertNotSame(list, doublesB);

    /*
     * Now insert doublesB over top of "dA". This should result in there being two copies of
     * doublesB associated with both keys "dA" and "dB".
     */
    pool.addIntegers("dA", integersB);

    assertPoolStateOK(pool);

    list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    assertNotSame(list, doublesB);

    list = pool.getDoubles("dB");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    assertNotSame(list, doublesB);

    /*
     * The very last test we need to make is to verify that if we use the addDoubles method to
     * replace a key that exists with string data.
     */
    pool.addStrings("A", stringsA);

    assertPoolStateOK(pool);

    /*
     * Check that the add worked, otherwise this is moot.
     */
    assertNotNull(pool.getStrings("A"));

    /*
     * Replace "A" with numeric data.
     */
    pool.addIntegers("A", integersA);

    assertPoolStateOK(pool);

    /*
     * And verify that querying the key numerically and string-wise produce the proper results.
     */
    assertNull(pool.getStrings("A"));

    list = pool.getDoubles("A");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);

    assertNotSame(list, doublesA);

  }

  @Test
  public void testAppendIntegers() {
    /*
     * First exercise the null key insertion UnsupportedOperationException.
     */
    try {
      pool.appendIntegers(null, integersA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null keys into"));
    }

    assertPoolStateOK(pool);

    /*
     * Next exercise the null value insertion UnsupportedOperationException.
     */
    try {
      pool.appendIntegers("TEST", null);
      fail("Expected Illegal Argument Exception not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Inserting null values into"));
    }

    assertPoolStateOK(pool);

    /*
     * The last of the exceptions to validate is the UnsupportedOperationException thrown when the
     * supplied values List is empty.
     */
    try {
      pool.appendIntegers("TEST", new ArrayList<Integer>());
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("There must be at " + "least one value"));
    }

    assertPoolStateOK(pool);

    /*
     * Now setup the special append UnsupportedOperationException that results when attempting to
     * append numeric data to a key that already contains string data.
     */
    pool.addStrings("A", stringsA);

    assertPoolStateOK(pool);

    try {
      pool.appendIntegers("A", integersA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Unable to append " + "Double values into key"));
    }

    assertPoolStateOK(pool);

    /*
     * First check to see that if we attempt to append to a key that does not exist it gets created
     * and the proper content is added.
     */
    assertNull(pool.getDoubles("dA"));
    assertNull(pool.getStrings("dA"));

    pool.appendIntegers("dA", integersA);

    assertPoolStateOK(pool);

    List<Double> list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);

    assertNotSame(list, doublesA);

    /*
     * Now since "dA" is associated with numeric data, append the contents of doublesB to the list.
     */
    pool.appendIntegers("dA", integersB);

    assertPoolStateOK(pool);

    /*
     * Check that this resulted in the correct action.
     */
    list = pool.getDoubles("dA");

    assertNotNull(list);
    assertEquals(5, list.size());
    assertEquals(10.0, list.get(0), 0.0);
    assertEquals(11.0, list.get(1), 0.0);
    assertEquals(12.0, list.get(2), 0.0);
    assertEquals(-10.0, list.get(3), 0.0);
    assertEquals(-11.0, list.get(4), 0.0);

    assertNotSame(list, doublesB);

    /*
     * Check to see that if we supply the same list as input to two newly created keywords, then we
     * don't end up intermingling the lists. The old version of the kernel pool didn't make deep
     * copies of the supplied lists, so this would have happened.
     */
    pool.addDoubles("X-TEST", doublesB);
    pool.addDoubles("X-TEST2", doublesB);

    pool.appendIntegers("X-TEST2", integersA);

    assertPoolStateOK(pool);

    list = pool.getDoubles("X-TEST");

    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);

    list = pool.getDoubles("X-TEST2");

    assertNotNull(list);
    assertEquals(5, list.size());
    assertEquals(-10.0, list.get(0), 0.0);
    assertEquals(-11.0, list.get(1), 0.0);
    assertEquals(10.0, list.get(2), 0.0);
    assertEquals(11.0, list.get(3), 0.0);
    assertEquals(12.0, list.get(4), 0.0);
  }

  @Test
  public void testLoad() {

    /*
     * Start by testing the various exceptions that could occur. There are only two, append doubles
     * to a string and append strings to doubles.
     */
    BasicKernelPool toLoad = new BasicKernelPool();
    BasicKernelPool conflictPool = new BasicKernelPool();

    /*
     * Try the double over strings case first.
     */
    conflictPool.addDoubles("MYDOUBLE", doublesA);
    conflictPool.addStrings("MYSTRING", stringsA);

    assertPoolStateOK(conflictPool);

    /*
     * Copy conflictPool, so we can verify that the load operation which fails does not alter it.
     */
    BasicKernelPool testPool = new BasicKernelPool(conflictPool);

    toLoad.appendDoubles("MYSTRING", doublesB);

    assertPoolStateOK(toLoad);

    try {
      conflictPool.load(toLoad);
      fail("Expected UnsupportedOperationException not thrown");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Attempt to append keyword MYSTRING"));
    }

    /*
     * Make certain conflictPool was untouched by the process.
     */
    assertPoolStateOK(conflictPool);
    assertPoolEquality(testPool, conflictPool);

    /*
     * Now try strings over doubles.
     */
    toLoad.clear();
    toLoad.appendStrings("MYDOUBLE", stringsA);

    assertPoolStateOK(toLoad);

    try {
      conflictPool.load(toLoad);
      fail("Expected UnsupportedOperationException not thrown");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Attempt to append keyword MYDOUBLE"));
    }

    /*
     * Make certain conflictPool was untouched by the process.
     */
    assertPoolStateOK(conflictPool);
    assertPoolEquality(testPool, conflictPool);

    /*
     * And lastly, both strings and doubles mixed.
     */
    toLoad.appendDoubles("MYSTRING", doublesA);

    assertPoolStateOK(toLoad);

    try {
      conflictPool.load(toLoad);
      fail("Expected UnsupportedOperationException not thrown");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Attempt to append keyword"));
    }

    /*
     * Make certain conflictPool was untouched by the process.
     */
    assertPoolStateOK(conflictPool);
    assertPoolEquality(testPool, conflictPool);

    /*
     * And if toLoad contains legit appends or adds.
     */
    toLoad.appendDoubles("LEGITDOUBLE", doublesB);
    toLoad.addStrings("LEGITSTRING", stringsB);

    assertPoolStateOK(toLoad);

    try {
      conflictPool.load(toLoad);
      fail("Expected UnsupportedOperationException not thrown");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Attempt to append keyword"));
    }

    /*
     * Make certain conflictPool was untouched by the process.
     */
    assertPoolStateOK(conflictPool);
    assertPoolEquality(testPool, conflictPool);

    /*
     * Now, test every possible state combination for the load method.
     */
    conflictPool = new BasicKernelPool();

    conflictPool.addDoubles("DRDR", doublesA);
    conflictPool.addDoubles("DADR", doublesA);
    conflictPool.addDoubles("SRDR", doublesA);

    conflictPool.appendDoubles("DRDA", doublesA);
    conflictPool.appendDoubles("DADA", doublesA);
    conflictPool.appendDoubles("SRDA", doublesA);

    conflictPool.addStrings("DRSR", stringsA);
    conflictPool.addStrings("SRSR", stringsA);
    conflictPool.addStrings("SASR", stringsA);

    conflictPool.appendStrings("DRSA", stringsA);
    conflictPool.appendStrings("SRSA", stringsA);
    conflictPool.appendStrings("SASA", stringsA);

    assertPoolStateOK(conflictPool);

    toLoad = new BasicKernelPool();

    toLoad.addDoubles("DRDR", doublesB);
    toLoad.appendDoubles("DADR", doublesB);
    toLoad.addStrings("SRDR", stringsB);

    toLoad.addDoubles("DRDA", doublesB);
    toLoad.appendDoubles("DADA", doublesB);
    toLoad.addStrings("SRDA", stringsB);

    toLoad.addDoubles("DRSR", doublesB);
    toLoad.addStrings("SRSR", stringsB);
    toLoad.appendStrings("SASR", stringsB);

    toLoad.addDoubles("DRSA", doublesB);
    toLoad.addStrings("SRSA", stringsB);
    toLoad.appendStrings("SASA", stringsB);

    assertPoolStateOK(toLoad);

    /*
     * Assemble the expected test pool.
     */
    testPool = new BasicKernelPool();

    testPool.addDoubles("DRDR", doublesA);
    testPool.addDoubles("DADR", doublesA);
    testPool.addDoubles("SRDR", doublesA);

    testPool.appendDoubles("DRDA", doublesA);
    testPool.appendDoubles("DADA", doublesA);
    testPool.appendDoubles("SRDA", doublesA);

    testPool.addStrings("DRSR", stringsA);
    testPool.addStrings("SRSR", stringsA);
    testPool.addStrings("SASR", stringsA);

    testPool.appendStrings("DRSA", stringsA);
    testPool.appendStrings("SRSA", stringsA);
    testPool.appendStrings("SASA", stringsA);

    testPool.addDoubles("DRDR", doublesB);
    testPool.appendDoubles("DADR", doublesB);
    testPool.addStrings("SRDR", stringsB);

    testPool.addDoubles("DRDA", doublesB);
    testPool.appendDoubles("DADA", doublesB);
    testPool.addStrings("SRDA", stringsB);

    testPool.addDoubles("DRSR", doublesB);
    testPool.addStrings("SRSR", stringsB);
    testPool.appendStrings("SASR", stringsB);

    testPool.addDoubles("DRSA", doublesB);
    testPool.addStrings("SRSA", stringsB);
    testPool.appendStrings("SASA", stringsB);

    assertPoolStateOK(testPool);

    conflictPool.load(toLoad);

    /*
     * Verify that conflictPool has the appropriate content.
     */
    assertPoolStateOK(conflictPool);
    assertPoolEquality(testPool, conflictPool);

    /*
     * Check the state of the quantities in conflictPool. This requires some subtle manipulation of
     * the pool loader, and loading conflictPool into another pool.
     */
    List<String> stringsC = new ArrayList<String>();
    stringsC.add("g");
    stringsC.add("h");

    List<Double> doublesC = new ArrayList<Double>();
    doublesC.add(-22.0);
    doublesC.add(22.0);

    /*
     * Populate loadInto with each keyword with the appropriate type of data that is present
     * currently in conflictPool. This is easy to determine, just look at the leading two characters
     * of the keyword.
     */
    BasicKernelPool loadInto = new BasicKernelPool();

    loadInto.addDoubles("DRDR", doublesC);
    loadInto.addDoubles("DADR", doublesC);
    loadInto.addStrings("SRDR", stringsC);

    loadInto.addDoubles("DRDA", doublesC);
    loadInto.addDoubles("DADA", doublesC);
    loadInto.addStrings("SRDA", stringsC);

    loadInto.addDoubles("DRSR", doublesC);
    loadInto.addStrings("SRSR", stringsC);
    loadInto.addStrings("SASR", stringsC);

    loadInto.addDoubles("DRSA", doublesC);
    loadInto.addStrings("SRSA", stringsC);
    loadInto.addStrings("SASA", stringsC);

    /*
     * Build a pool that contains our expectations from loading conflictPool into loadInto.
     */
    testPool = new BasicKernelPool();

    testPool.addDoubles("DRDR", doublesB);
    testPool.addDoubles("DADR", doublesA);
    testPool.appendDoubles("DADR", doublesB);
    testPool.addStrings("SRDR", stringsB);

    testPool.addDoubles("DRDA", doublesB);
    testPool.addDoubles("DADA", doublesC);
    testPool.appendDoubles("DADA", doublesA);
    testPool.appendDoubles("DADA", doublesB);
    testPool.addStrings("SRDA", stringsB);

    testPool.addDoubles("DRSR", doublesB);
    testPool.addStrings("SRSR", stringsB);
    testPool.addStrings("SASR", stringsA);
    testPool.appendStrings("SASR", stringsB);

    testPool.addDoubles("DRSA", doublesB);
    testPool.addStrings("SRSA", stringsB);
    testPool.addStrings("SASA", stringsC);
    testPool.appendStrings("SASA", stringsA);
    testPool.appendStrings("SASA", stringsB);

    assertPoolStateOK(testPool);
    assertPoolStateOK(loadInto);

    loadInto.load(conflictPool);

    assertPoolStateOK(conflictPool);

    /*
     * Verify the contents of loadInto match testPool.
     */
    assertPoolStateOK(loadInto);
    assertPoolEquality(testPool, loadInto);
  }

  @Test
  public void testRemoveKeywords() {

    /*
     * Load the lists we created in the setUp method into the pool.
     */
    pool.addDoubles("TESTD1", doublesA);
    pool.addDoubles("TESTD2", doublesB);
    pool.addStrings("TESTS1", stringsA);
    pool.addStrings("TESTS2", stringsB);

    assertPoolStateOK(pool);

    /*
     * First try and remove some keywords from pool that aren't present. This should result in no
     * change to the list.
     */
    List<String> list = new ArrayList<String>();
    list.add("NOT-PRESENT");
    list.add("NOT-PRESENT2");
    pool.removeKeywords(list);

    assertPoolStateOK(pool);

    Set<String> keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(4, keywords.size());
    assertTrue(keywords.contains("TESTD1"));
    assertTrue(keywords.contains("TESTD2"));
    assertTrue(keywords.contains("TESTS1"));
    assertTrue(keywords.contains("TESTS2"));

    /*
     * Next remove a mix of double and string keywords.
     */
    list.clear();
    list.add("TESTD1");
    list.add("TESTS1");
    pool.removeKeywords(list);

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(2, keywords.size());
    assertTrue(keywords.contains("TESTD2"));
    assertTrue(keywords.contains("TESTS2"));

    /*
     * Now remove the remainder with a mix of known and unknown keywords.
     */
    list.clear();
    list.add("NOT-PRESENT3");
    list.add("TESTS2");
    list.add("TESTS1");
    list.add("TESTD2");
    list.add("NOT-PRESENT4");
    pool.removeKeywords(list);

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(0, keywords.size());

  }

  @Test
  public void testRemoveKeyword() {

    /*
     * Load the lists we created in the setUp method into the pool.
     */
    pool.addDoubles("TESTD1", doublesA);
    pool.addDoubles("TESTD2", doublesB);
    pool.addStrings("TESTS1", stringsA);
    pool.addStrings("TESTS2", stringsB);

    assertPoolStateOK(pool);

    /*
     * First try and remove some keywords from pool that aren't present. This should result in no
     * change to the list.
     */
    pool.removeKeyword("NOT-PRESENT");
    pool.removeKeyword("NOT-PRESENT2");

    assertPoolStateOK(pool);

    Set<String> keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(4, keywords.size());
    assertTrue(keywords.contains("TESTD1"));
    assertTrue(keywords.contains("TESTD2"));
    assertTrue(keywords.contains("TESTS1"));
    assertTrue(keywords.contains("TESTS2"));

    /*
     * Next remove a mix of double and string keywords.
     */
    pool.removeKeyword("TESTD1");
    pool.removeKeyword("TESTS1");

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(2, keywords.size());
    assertTrue(keywords.contains("TESTD2"));
    assertTrue(keywords.contains("TESTS2"));

  }

  @Test
  public void testClear() {

    pool.addStrings("A", stringsA);
    pool.addStrings("B", stringsB);
    pool.addDoubles("C", doublesA);
    pool.addDoubles("D", doublesB);

    assertPoolStateOK(pool);

    Set<String> keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(4, keywords.size());

    pool.clear();

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(0, keywords.size());

  }

  @Test
  public void testGetStrings() {

    /*
     * First, since the pool is completely empty, attempt to retrieve a key. It should fail,
     * returning null.
     */
    List<String> list;

    list = pool.getStrings("A");

    assertNull(list);

    /*
     * Now populate the pool with a few double values. Attempt to retrieve them from the string
     * interface.
     */
    pool.addDoubles("dA", doublesA);
    pool.addDoubles("dB", doublesB);

    assertPoolStateOK(pool);

    /*
     * First attempt a retrieval of non-existant keys.
     */
    list = pool.getStrings("A");

    assertNull(list);

    /*
     * Now retrieve dA. This shall also return null, as there is no string key bound to "dA".
     */
    list = pool.getStrings("dA");

    assertNull(list);

    /*
     * Lastly, load in some string variables and attempt to read them from the pool.
     */
    pool.addStrings("A", stringsA);
    pool.addStrings("B", stringsB);

    assertPoolStateOK(pool);

    list = pool.getStrings("A");

    assertEquals(stringsA, list);

    list = pool.getStrings("B");

    assertEquals(stringsB, list);

  }

  @Test
  public void testGetDoubles() {

    /*
     * First, since the pool is completely empty, attempt to retrieve a key. It should fail,
     * returning null.
     */
    List<Double> list;

    list = pool.getDoubles("A");

    assertNull(list);

    /*
     * Now populate the pool with a few string values. Attempt to retrieve them via the double
     * interface.
     */
    pool.addStrings("A", stringsA);
    pool.addStrings("B", stringsB);

    assertPoolStateOK(pool);

    /*
     * First attempt retrieval of non-existant keys. This should still result in null, even though
     * the pool is now initialized and contains some values.
     */
    list = pool.getDoubles("dA");

    assertNull(list);

    /*
     * Now retrieve A. This shall also return null, as there is no double key bound to "A".
     */
    list = pool.getDoubles("A");

    assertNull(list);

    /*
     * Lastly, load in some double variables and attempt to read them from the pool.
     */
    pool.addDoubles("dA", doublesA);
    pool.addDoubles("dB", doublesB);

    assertPoolStateOK(pool);

    list = pool.getDoubles("dA");

    assertEquals(doublesA, list);

    list = pool.getDoubles("dB");

    assertEquals(doublesB, list);

  }

  @Test(expected = KernelPoolFormatException.class)
  public void testGetIntegersConversionExceptionHigh() {

    List<Double> nonIntegerCompliantList = new ArrayList<Double>();
    nonIntegerCompliantList.add(((Integer.MAX_VALUE) + 1.0));

    pool.addDoubles("BADINTS", nonIntegerCompliantList);
    pool.getIntegers("BADINTS");

  }

  @Test(expected = KernelPoolFormatException.class)
  public void testGetIntegersConversionExceptionLow() {

    List<Double> nonIntegerCompliantList = new ArrayList<Double>();
    nonIntegerCompliantList.add(((Integer.MIN_VALUE) - 1.0));

    pool.addDoubles("BADINTS", nonIntegerCompliantList);
    pool.getIntegers("BADINTS");

  }

  @Test
  public void testGetIntegers() {

    /*
     * First, since the pool is completely empty, attempt to retrieve a key. It should fail,
     * returning null.
     */
    List<Integer> list;

    list = pool.getIntegers("A");

    assertNull(list);

    /*
     * Now populate the pool with a few string values. Attempt to retrieve them via the double
     * interface.
     */
    pool.addStrings("A", stringsA);
    pool.addStrings("B", stringsB);

    assertPoolStateOK(pool);

    /*
     * First attempt retrieval of non-existant keys. This should still result in null, even though
     * the pool is now initialized and contains some values.
     */
    list = pool.getIntegers("dA");

    assertNull(list);

    /*
     * Now retrieve A. This shall also return null, as there is no double key bound to "A".
     */
    list = pool.getIntegers("A");

    assertNull(list);

    /*
     * Lastly, load in some double variables and attempt to read them from the pool.
     */
    pool.addDoubles("dA", doublesA);
    pool.addDoubles("dB", doublesB);

    assertPoolStateOK(pool);

    list = pool.getIntegers("dA");

    assertEquals(integersA, list);

    list = pool.getIntegers("dB");

    assertEquals(integersB, list);

    /*
     * Lastly load in actual fractional values and verify that the appropriate integer rounding is
     * occuring.
     */
    pool.addDoubles("dC", doublesC);

    list = pool.getIntegers("dC");

    assertEquals(integersC, list);

  }

  @Test
  public void testGetKeywords() {

    /*
     * Start by extracting the keyword list from an empty pool. It should return an empty list.
     */
    BasicKernelPool pool = new BasicKernelPool();

    Set<String> keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(0, keywords.size());

    /*
     * Load some values into pool and try again.
     */
    pool.addDoubles("DOUBLEA", doublesA);
    pool.addStrings("STRINGA", stringsA);

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertTrue(keywords.contains("DOUBLEA"));
    assertTrue(keywords.contains("STRINGA"));

    pool.addStrings("STRINGB", stringsB);
    pool.addDoubles("DOUBLEB", doublesA);

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertTrue(keywords.contains("DOUBLEA"));
    assertTrue(keywords.contains("STRINGA"));
    assertTrue(keywords.contains("DOUBLEB"));
    assertTrue(keywords.contains("STRINGB"));

    pool.clear();

    assertPoolStateOK(pool);

    keywords = pool.getKeywords();

    assertNotNull(keywords);
    assertEquals(0, keywords.size());

  }

  @Test
  public void testIsStringValued() {

    /*
     * Start by asking the question for keywords in an empty pool.
     */
    assertFalse(pool.isStringValued("TEST"));
    assertFalse(pool.isStringValued("TESTTEST"));

    /*
     * Now populate the pool with double data and try again.
     */
    pool.addDoubles("TEST", doublesA);
    assertFalse(pool.isStringValued("TEST"));
    assertPoolStateOK(pool);

    pool.addStrings("TESTTEST", stringsA);
    assertTrue(pool.isStringValued("TESTTEST"));
    assertPoolStateOK(pool);
  }

  @Test
  public void testIsDoubleValued() {

    /*
     * Start by asking the question for keywords in an empty pool.
     */
    assertFalse(pool.isDoubleValued("TEST"));
    assertFalse(pool.isDoubleValued("TESTTEST"));

    /*
     * Now populate the pool with double data and try again.
     */
    pool.addDoubles("TEST", doublesA);
    assertTrue(pool.isDoubleValued("TEST"));
    assertPoolStateOK(pool);

    pool.addStrings("TESTTEST", stringsA);
    assertFalse(pool.isDoubleValued("TESTTEST"));
    assertPoolStateOK(pool);
  }

  @Test
  public void testIsIntegerValued() {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TEST", ImmutableList.of(1e30));
    pool.addDoubles("TESTLONG",
        ImmutableList.of(1.0, 2.0, 3.0, 4.0, (double) Integer.MAX_VALUE + 1));
    pool.addDoubles("TESTOK", ImmutableList.of(1.0, 2.0));
    pool.addStrings("TESTSTRING", ImmutableList.of("A", "B"));

    assertFalse(pool.isIntegerValued("TEST"));
    assertFalse(pool.isIntegerValued("TESTLONG"));
    assertTrue(pool.isIntegerValued("TESTOK"));
    assertFalse(pool.isIntegerValued("TESTSTRING"));

  }

}
