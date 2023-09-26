package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernelpool.BasicKernelPool;

public class KernelPoolContentFactoryTest {

  private KernelPoolContentFactory factory;

  @Before
  public void setUp() throws Exception {
    factory = new KernelPoolContentFactory();
  }

  @Test
  public void testEphemerisIDMapEmptyKeywords() throws Exception {
    assertEquals(0, factory.createEphemerisIDMap(new BasicKernelPool()).size());
  }

  @Test(expected = TextKernelContentInstantiationException.class)
  public void testEphemerisIDMapAbsentNameArray() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<Integer> codes = new ArrayList<Integer>();
    codes.add(1);
    pool.addIntegers("NAIF_BODY_CODE", codes);
    factory.createEphemerisIDMap(pool);
  }

  @Test(expected = TextKernelContentInstantiationException.class)
  public void testEphemerisIDMapAbsentCodeArray() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<String> names = new ArrayList<String>();
    names.add("NAME_A");
    names.add("NAME_B");
    pool.addStrings("NAIF_BODY_NAME", names);
    factory.createEphemerisIDMap(pool);
  }

  @Test(expected = TextKernelContentInstantiationException.class)
  public void testEphemerisIDMapArraySizeMismatchException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<String> names = new ArrayList<String>();
    names.add("NAME_A");
    names.add("NAME_B");
    List<Integer> codes = new ArrayList<Integer>();
    codes.add(1);
    pool.addStrings("NAIF_BODY_NAME", names);
    pool.addIntegers("NAIF_BODY_CODE", codes);
    factory.createEphemerisIDMap(pool);
  }

  @Test(expected = TextKernelContentInstantiationException.class)
  public void testEphemerisIDMapTabCharacterPresentException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<String> names = new ArrayList<String>();
    names.add("NAME\tA");
    List<Integer> codes = new ArrayList<Integer>();
    codes.add(1);
    pool.addStrings("NAIF_BODY_NAME", names);
    pool.addIntegers("NAIF_BODY_CODE", codes);
    factory.createEphemerisIDMap(pool);
  }

  @Test
  public void testEphemerisIDMapCanonicalizationOfStringKeys() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<String> names = new ArrayList<String>();
    names.add("   LEADSPACE");
    names.add("TRAILSPACE   ");
    names.add("   LEAD_AND_TRAIL   ");
    names.add("MULTI      SPACE");
    names.add("MixEDcaSE");
    names.add("  all  Three   Types   ");
    pool.addStrings("NAIF_BODY_NAME", names);

    List<Integer> codes = new ArrayList<Integer>();
    for (int i = 0; i < names.size(); i++) {
      codes.add(i);
    }
    pool.addIntegers("NAIF_BODY_CODE", codes);

    Map<String, Integer> map = factory.createEphemerisIDMap(pool);

    Set<String> keyset = map.keySet();
    assertEquals(names.size(), keyset.size());
    assertTrue(keyset.contains("LEADSPACE"));
    assertTrue(keyset.contains("TRAILSPACE"));
    assertTrue(keyset.contains("LEAD_AND_TRAIL"));
    assertTrue(keyset.contains("MULTI SPACE"));
    assertTrue(keyset.contains("MIXEDCASE"));
    assertTrue(keyset.contains("ALL THREE TYPES"));
  }

  @Test
  public void testEphemerisIDMapStandardMapConstructionCase() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<String> names = new ArrayList<String>();
    names.add("ONE");
    names.add("TWO");
    names.add("THREE");
    List<Integer> codes = new ArrayList<Integer>();
    codes.add(1);
    codes.add(2);
    codes.add(3);
    pool.addStrings("NAIF_BODY_NAME", names);
    pool.addIntegers("NAIF_BODY_CODE", codes);
    Map<String, Integer> map = factory.createEphemerisIDMap(pool);

    assertEquals(names.size(), map.size());
    assertEquals(Integer.valueOf(1), map.get("ONE"));
    assertEquals(Integer.valueOf(2), map.get("TWO"));
    assertEquals(Integer.valueOf(3), map.get("THREE"));
  }

  @Test
  public void testEphemerisIDMapOverrideMapConstructionCase() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    List<String> names = new ArrayList<String>();
    names.add("ONE");
    names.add("TWO");
    names.add("THREE");
    names.add("THREE");
    List<Integer> codes = new ArrayList<Integer>();
    codes.add(1);
    codes.add(2);
    codes.add(3);
    codes.add(-3);
    pool.addStrings("NAIF_BODY_NAME", names);
    pool.addIntegers("NAIF_BODY_CODE", codes);
    Map<String, Integer> map = factory.createEphemerisIDMap(pool);

    assertEquals(3, map.size());
    assertEquals(Integer.valueOf(1), map.get("ONE"));
    assertEquals(Integer.valueOf(2), map.get("TWO"));
    assertEquals(Integer.valueOf(-3), map.get("THREE"));
  }

}
