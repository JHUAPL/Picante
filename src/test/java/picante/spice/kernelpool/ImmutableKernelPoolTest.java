package picante.spice.kernelpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static picante.spice.kernelpool.AssertionUtilities.assertPoolEquality;
import static picante.spice.kernelpool.AssertionUtilities.assertPoolStateOK;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class ImmutableKernelPoolTest {

  UnwritableKernelPool emptyPool;
  UnwritableKernelPool pool;

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

    emptyPool = new UnwritableKernelPool();

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
  public void testImmutableKernelPool() {

    /*
     * The default constructor just creates an empty pool.
     */
    Set<String> keywords = emptyPool.getKeywords();
    assertEquals(0, keywords.size());

  }

  @Test
  public void testImmutableKernelPoolImmutableKernelPool() {

    /*
     * Build up a simple pool and use the copy constructor.
     */
    BasicKernelPool poolToCopy = new BasicKernelPool();
    poolToCopy.addStrings("stringsA", stringsA);
    poolToCopy.addStrings("stringsB", stringsB);
    poolToCopy.addDoubles("doublesA", doublesA);
    poolToCopy.addDoubles("doublesB", doublesB);

    assertPoolStateOK(poolToCopy);

    pool = new UnwritableKernelPool(poolToCopy);

    assertPoolStateOK(pool);

    /*
     * Now verify that pool and copy are the same.
     */
    assertPoolEquality(poolToCopy, pool);

    /*
     * Lastly verify that copy is TRULY a copy of pool, that is to say a deep copy.
     */
    Set<String> keywords = pool.getKeywords();

    for (String key : keywords) {
      if (pool.isDoubleValued(key)) {
        List<Double> cTest = pool.getDoubles(key);
        List<Double> pTest = poolToCopy.getDoubles(key);
        assertNotSame(cTest, pTest);
      } else {
        List<String> cTest = pool.getStrings(key);
        List<String> pTest = poolToCopy.getStrings(key);
        assertNotSame(cTest, pTest);
      }
    }

  }

  @Test
  public void testGetStrings() {

    assertNull(emptyPool.getStrings("A"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addDoubles("dA", doublesA);
    builder.addDoubles("dB", doublesB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);
    assertNull(pool.getStrings("A"));
    assertNull(pool.getStrings("dA"));

    builder.addStrings("A", stringsA);
    builder.addStrings("B", stringsB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);
    assertEquals(stringsA, pool.getStrings("A"));
    assertEquals(stringsB, pool.getStrings("B"));

  }

  @Test
  public void testGetDoubles() {

    assertNull(emptyPool.getDoubles("A"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addStrings("A", stringsA);
    builder.addStrings("B", stringsB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);
    assertNull(pool.getDoubles("dA"));
    assertNull(pool.getDoubles("A"));

    builder.addDoubles("dA", doublesA);
    builder.addDoubles("dB", doublesB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);

    assertEquals(doublesA, pool.getDoubles("dA"));
    assertEquals(doublesB, pool.getDoubles("dB"));
  }

  @Test(expected = KernelPoolFormatException.class)
  public void testGetIntegersConversionExceptionHigh() {

    BasicKernelPool builder = new BasicKernelPool();
    List<Double> nonIntegerCompliantList = new ArrayList<Double>();
    nonIntegerCompliantList.add(((Integer.MAX_VALUE) + 1.0));
    builder.addDoubles("BADINTS", nonIntegerCompliantList);

    pool = new UnwritableKernelPool(builder);
    pool.getIntegers("BADINTS");

  }

  @Test(expected = KernelPoolFormatException.class)
  public void testGetIntegersConversionExceptionLow() {

    BasicKernelPool builder = new BasicKernelPool();
    List<Double> nonIntegerCompliantList = new ArrayList<Double>();
    nonIntegerCompliantList.add(((Integer.MIN_VALUE) - 1.0));
    builder.addDoubles("BADINTS", nonIntegerCompliantList);

    pool = new UnwritableKernelPool(builder);
    pool.getIntegers("BADINTS");

  }

  @Test
  public void testGetIntegers() {

    assertNull(emptyPool.getIntegers("A"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addStrings("A", stringsA);
    builder.addStrings("B", stringsB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);
    assertNull(pool.getIntegers("dA"));
    assertNull(pool.getIntegers("A"));

    builder.addDoubles("dA", doublesA);
    builder.addDoubles("dB", doublesB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);

    assertEquals(integersA, pool.getIntegers("dA"));
    assertEquals(integersB, pool.getIntegers("dB"));

  }

  @Test
  public void testGetKeywords() {

    assertNotNull(emptyPool.getKeywords());
    assertEquals(0, emptyPool.getKeywords().size());

    BasicKernelPool builder = new BasicKernelPool();
    builder.addDoubles("DOUBLEA", doublesA);
    builder.addStrings("STRINGA", stringsA);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);
    assertNotNull(pool.getKeywords());
    assertTrue(pool.getKeywords().contains("DOUBLEA"));
    assertTrue(pool.getKeywords().contains("STRINGA"));
    assertEquals(2, pool.getKeywords().size());

    builder.addStrings("STRINGB", stringsB);
    builder.addDoubles("DOUBLEB", doublesB);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);

    assertNotNull(pool.getKeywords());
    assertTrue(pool.getKeywords().contains("DOUBLEA"));
    assertTrue(pool.getKeywords().contains("STRINGA"));
    assertTrue(pool.getKeywords().contains("DOUBLEB"));
    assertTrue(pool.getKeywords().contains("STRINGB"));
    assertEquals(4, pool.getKeywords().size());

  }

  @Test
  public void testIsStringValued() {

    assertFalse(emptyPool.isStringValued("TEST"));
    assertFalse(emptyPool.isStringValued("TESTTEST"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addDoubles("TEST", doublesA);
    builder.addStrings("TESTTEST", stringsA);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);

    assertFalse(pool.isStringValued("TEST"));
    assertTrue(pool.isStringValued("TESTTEST"));

  }

  @Test
  public void testIsDoubleValued() {

    assertFalse(emptyPool.isDoubleValued("TEST"));
    assertFalse(emptyPool.isDoubleValued("TESTTEST"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addDoubles("TEST", doublesA);
    builder.addStrings("TESTTEST", stringsA);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);

    assertTrue(pool.isDoubleValued("TEST"));
    assertFalse(pool.isDoubleValued("TESTTEST"));

  }

  @Test
  public void testIsIntegerValued() {

    assertFalse(emptyPool.isIntegerValued("TEST"));
    assertFalse(emptyPool.isIntegerValued("TESTTEST"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addDoubles("TEST", doublesA);
    builder.addStrings("TESTTEST", stringsA);

    pool = new UnwritableKernelPool(builder);

    assertPoolStateOK(pool);

    assertTrue(pool.isIntegerValued("TEST"));
    assertFalse(pool.isIntegerValued("TESTTEST"));

  }

  @Test
  public void testHasKeyword() {

    assertFalse(emptyPool.hasKeyword("TEST"));

    BasicKernelPool builder = new BasicKernelPool();
    builder.addDoubles("TEST", doublesA);

    pool = new UnwritableKernelPool(builder);
    assertPoolStateOK(pool);
    assertTrue(pool.hasKeyword("TEST"));

    builder.addStrings("TEST", stringsA);

    pool = new UnwritableKernelPool(builder);
    assertPoolStateOK(pool);
    assertTrue(pool.hasKeyword("TEST"));

  }

  /**
   * Six distinct cases need to be exercised:
   * <ul>
   * <li>giver keyword append state, receiver keyword append state</li>
   * <li>giver keyword append state, receiver keyword no append state</li>
   * <li>giver keyword append state, receiver keyword not present</li>
   * <li>giver keyword no append state, receiver keyword append state</li>
   * <li>giver keyword no append state, receiver keyword no append state</li>
   * <li>giver keyword no append state, receiver keyword not present</li>
   * 
   * </ul>
   */
  @Test
  public void testMergeMaps() {

    Map<String, KernelPoolValues<String>> giver = Maps.newHashMap();
    Map<String, KernelPoolValues<String>> receiver = Maps.newHashMap();

    giver.put("Aa", new KernelPoolValues<String>(true, stringsA));
    giver.put("Ba", new KernelPoolValues<String>(true, stringsA));
    giver.put("Ca", new KernelPoolValues<String>(true, stringsA));

    giver.put("An", new KernelPoolValues<String>(false, stringsA));
    giver.put("Bn", new KernelPoolValues<String>(false, stringsA));
    giver.put("Cn", new KernelPoolValues<String>(false, stringsA));

    receiver.put("Aa", new KernelPoolValues<String>(true, stringsB));
    receiver.put("Ba", new KernelPoolValues<String>(false, stringsB));

    receiver.put("An", new KernelPoolValues<String>(true, stringsB));
    receiver.put("Bn", new KernelPoolValues<String>(false, stringsB));

    UnwritableKernelPool.mergeMaps(giver, receiver);

    List<String> concat = Lists.newArrayList(stringsB);
    concat.addAll(stringsA);

    assertTrue(receiver.get("Ca").appendState);
    assertEquals(stringsA, receiver.get("Ca").values);
    assertFalse(receiver.get("Cn").appendState);
    assertEquals(stringsA, receiver.get("Cn").values);

    assertTrue(receiver.get("Aa").appendState);
    assertEquals(concat, receiver.get("Aa").values);

    assertFalse(receiver.get("Ba").appendState);
    assertEquals(concat, receiver.get("Ba").values);

    assertFalse(receiver.get("An").appendState);
    assertEquals(stringsA, receiver.get("An").values);

    assertFalse(receiver.get("Bn").appendState);
    assertEquals(stringsA, receiver.get("Bn").values);

  }
}
