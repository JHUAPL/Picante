package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernelpool.BasicKernelPool;

public class FKFactoryTest {

  private FKFactory factory;
  private Set<FrameInfo> frames;
  private Map<String, Integer> aliases;
  private BasicKernelPool pool;
  private KernelPoolValidatingRetriever retriever;
  private Map<String, Integer> emptyEphemerisMap;

  @Before
  public void setUp() throws Exception {
    factory = new FKFactory();

    frames = Sets.newHashSet();
    frames.add(new FrameInfo("TEST_POSITIVE_INERTIAL", 100, FrameType.INERTIAL, 100, 99));
    frames.add(new FrameInfo("TEST_NEGATIVE_INERTIAL", -100, FrameType.INERTIAL, -100, -99));
    frames.add(new FrameInfo("TEST_POSITIVE_PCK", 200, FrameType.PCK, 201, 199));
    frames.add(new FrameInfo("TEST_NEGATIVE_PCK", -200, FrameType.PCK, -201, -199));
    frames.add(new FrameInfo("TEST_POSITIVE_CK", 300, FrameType.CK, 301, 299));
    frames.add(new FrameInfo("TEST_NEGATIVE_CK", -300, FrameType.CK, -301, -299));
    frames.add(new FrameInfo("TEST_POSITIVE_FIXED", 400, FrameType.TK, 401, 399));
    frames.add(new FrameInfo("TEST_NEGATIVE_FIXED", -400, FrameType.TK, -401, -399));
    frames.add(new FrameInfo("TEST_POSITIVE_DYNAMIC", 500, FrameType.DYNAMIC, 501, 499));
    frames.add(new FrameInfo("TEST_NEGATIVE_DYNAMIC", -500, FrameType.DYNAMIC, -501, -499));

    pool = new BasicKernelPool();

    for (FrameInfo info : frames) {
      addFrameAliasToPool(info.getName(), info.getCode(), pool);
      addFrameToPool(info, pool);
    }

    aliases = Maps.newHashMap();
    aliases.put("ALIAS_POSITIVE_INERTIAL", 100);
    aliases.put("ALIAS_NEGATIVE_PCK", -200);
    aliases.put("ALIAS_NOT_PRESENT", -10000);
    aliases.put("ALIAS_ALSO_NOT_PRESENT", -11000);

    for (String name : aliases.keySet()) {
      addFrameAliasToPool(name, aliases.get(name), pool);
    }

    retriever = new KernelPoolValidatingRetriever(pool);

    emptyEphemerisMap = Maps.newHashMap();
  }

  /*
   * Ignoring this method until we have fully flushed out the FK.
   */
  @Ignore
  @Test
  public void testCreate() {
    fail("Not yet implemented");
  }

  @Test(expected = FKInstantiationException.class)
  public void testCreateFrameDefinitionsInvalidConfiguration() throws Exception {
    pool.removeKeyword("FRAME_100_CLASS");
    factory.createFrameDefinitions(retriever, emptyEphemerisMap);
  }

  @Test
  public void testCreateFrameDefinitions() throws Exception {

    /*
     * Just run a well put together frame kernel content through its paces.
     */
    Map<Integer, FrameInfo> result = factory.createFrameDefinitions(retriever, emptyEphemerisMap);

    /*
     * Check to see that all the required bindings are present and as expected.
     */
    for (FrameInfo info : frames) {
      assertEquals(info, result.get(info.getCode()));
    }

    /*
     * These are the only bindings that should be present.
     */
    assertEquals(frames.size(), result.size());

  }

  public void testCreateFrameDefinitionsFromNameBasedKeywords() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addIntegers("FRAME_TEST", Arrays.asList(100));
    pool.addStrings("FRAME_100_NAME", Arrays.asList("TEST"));
    pool.addIntegers("FRAME_TEST_CLASS", Arrays.asList(4));
    pool.addIntegers("FRAME_TEST_CLASS_ID", Arrays.asList(100));
    pool.addStrings("FRAME_TEST_CENTER", Arrays.asList("OBJECT"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> codeMap = Maps.newHashMap();
    codeMap.put("OBJECT", 1010);

    Map<Integer, FrameInfo> result = factory.createFrameDefinitions(retriever, codeMap);

    assertEquals(1, result.size());
    assertEquals(new FrameInfo("TEST", 100, FrameType.TK, 100, 1010), result.get(100));

  }

  @Test(expected = FKInstantiationException.class)
  public void testCreateFrameDefinitionsFromNameBasedKeywordsMixedCaseIncompatibleException()
      throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addIntegers("FRAME_TEST", Arrays.asList(100));
    pool.addStrings("FRAME_100_NAME", Arrays.asList("tESt"));
    pool.addIntegers("FRAME_tESt_CLASS", Arrays.asList(4));
    pool.addIntegers("FRAME_tESt_CLASS_ID", Arrays.asList(100));
    pool.addStrings("FRAME_TEST_CENTER", Arrays.asList("OBJECT"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> codeMap = Maps.newHashMap();
    codeMap.put("OBJECT", 1010);

    factory.createFrameDefinitions(retriever, codeMap);

  }

  @Test
  public void testCreateFrameDefinitionsFromNameBasedKeywordsMixedCase() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addIntegers("FRAME_TEST", Arrays.asList(100));
    pool.addStrings("FRAME_100_NAME", Arrays.asList("tESt"));
    pool.addIntegers("FRAME_tESt_CLASS", Arrays.asList(4));
    pool.addIntegers("FRAME_tESt_CLASS_ID", Arrays.asList(100));
    pool.addStrings("FRAME_tESt_CENTER", Arrays.asList("OBJECT"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> codeMap = Maps.newHashMap();
    codeMap.put("OBJECT", 1010);

    Map<Integer, FrameInfo> result = factory.createFrameDefinitions(retriever, codeMap);

    assertEquals(1, result.size());
    assertEquals(new FrameInfo("TEST", 100, FrameType.TK, 100, 1010), result.get(100));

  }

  @Test
  public void testCreateNameBindingsWithOddBindings() throws Exception {

    /*
     * SPICE does not allow mixed case frame names to be retrieved from the kernel pool. Make sure
     * the factory silently ignores them.
     */
    pool.addIntegers("FRAME_ThisIsAMixedCaseName", Arrays.asList(100));
    pool.addIntegers("FRAME_Test_Positive_Inertial", Arrays.asList(100));
    pool.addIntegers("FRAME_TEST_Positive_inertial", Arrays.asList(200));
    pool.addStrings("FRAME_TEST_PoSiTiVe_INERTIAL", Arrays.asList("TEST"));
    pool.addIntegers("FRAME_MULTIVALUED_NAME", Arrays.asList(100, 200));

    Map<String, Integer> result = factory.createNameBindings(retriever);

    /*
     * Check that all the properly defined frames and the alias in the setUp method are included.
     */
    for (FrameInfo info : frames) {
      assertTrue(result.containsKey(info.getName()));
      assertEquals(Integer.valueOf(info.getCode()), result.get(info.getName()));
    }

    for (String alias : aliases.keySet()) {
      assertTrue(result.containsKey(alias));
      assertEquals(aliases.get(alias), result.get(alias));
    }

  }

  @Test
  public void testCreateNameBindings() throws Exception {

    Map<String, Integer> result = factory.createNameBindings(retriever);

    /*
     * Check that all the properly defined frames and the alias in the setUp method are included.
     */
    for (FrameInfo info : frames) {
      assertTrue(result.containsKey(info.getName()));
      assertEquals(Integer.valueOf(info.getCode()), result.get(info.getName()));
    }

    for (String alias : aliases.keySet()) {
      assertTrue(result.containsKey(alias));
      assertEquals(aliases.get(alias), result.get(alias));
    }

    /*
     * We expect other aliases to be present in the binding list, as any keyword that begins with
     * FRAME_ and maps to a single integer will have an entry in the map. This is to be expected,
     * and will be resolved at a higher layer in the code.
     */

  }

  @Test
  public void testCreateFrameInfo() throws Exception {
    for (FrameInfo info : frames) {
      FrameInfo test = factory.createFrameInfo(info.getCode(), retriever, emptyEphemerisMap);
      assertEquals(info, test);
    }

  }

  @Test
  public void testCreateSCLKMap() throws Exception {

    String[] poolText = new String[] {"CK_-10000_SCLK = -157", "CK_10000_SCLK = 137",
        "FRAME_-10000_NAME = 'TESTFRAMEA'", "FRAME_-10000_CLASS = 3",
        "FRAME_-10000_CLASS_ID = -20000", "FRAME_10000_NAME = 'TESTFRAMEB'",
        "FRAME_10000_CLASS = 3", "FRAME_10000_CLASS_ID = -21000", "FRAME_13333_NAME = 'TESTFRAMEC'",
        "FRAME_13333_CLASS = 4", "FRAME_13333_CLASS_ID = 13333", "FRAME_-14000_NAME = 'TESTFRAMED'",
        "FRAME_-14000_CLASS = 3", "FRAME_-14000_CLASS_ID = -22000",
        "FRAME_-15000_NAME = 'TESTFRAMEE'", "FRAME_-15000_CLASS = 2",
        "FRAME_-15000_CLASS_ID = -22000"};

    KernelPoolValidatingRetriever retriever =
        new KernelPoolValidatingRetriever(KernelPoolBuilder.createPool(poolText));

    Map<Integer, Integer> result = factory.createSCLKMap(retriever);

    assertEquals(result.size(), 2);
    assertTrue(result.containsKey(-10000));
    assertTrue(result.containsKey(10000));
    assertEquals(Integer.valueOf(-157), result.get(-10000));
    assertEquals(Integer.valueOf(137), result.get(10000));
  }

  @Test(expected = FKInstantiationException.class)
  public void testCreateSCLKMapBadPoolContent() throws Exception {

    String[] brokenPoolText = new String[] {"CK_-10000_SCLK = -157", "CK_10000_SCLK = 137",
        "CK_10100_SCLK = 'BADSCLKID'", "FRAME_-10000_NAME = 'TESTFRAMEA'", "FRAME_-10000_CLASS = 3",
        "FRAME_-10000_CLASS_ID = -20000", "FRAME_10000_NAME = 'TESTFRAMEB'",
        "FRAME_10000_CLASS = 3", "FRAME_10000_CLASS_ID = -21000", "FRAME_13333_NAME = 'TESTFRAMEC'",
        "FRAME_13333_CLASS = 4", "FRAME_13333_CLASS_ID = 13333", "FRAME_-14000_NAME = 'TESTFRAMED'",
        "FRAME_-14000_CLASS = 3", "FRAME_-14000_CLASS_ID = 'BADCLASSID'",
        "FRAME_-15000_NAME = 'TESTFRAMEE'", "FRAME_-15000_CLASS = 'BADCLASS'",
        "FRAME_-15000_CLASS_ID = -22000"};

    KernelPoolValidatingRetriever retriever =
        new KernelPoolValidatingRetriever(KernelPoolBuilder.createPool(brokenPoolText));

    factory.createSCLKMap(retriever);
  }

  /**
   * TODO: There should be multiple versions of this test to check that the validating retriever is
   * used to pull all the required data out of the kernel.
   * 
   * @throws Exception
   */
  @Test(expected = FKInstantiationException.class)
  public void testCreateFrameInfoBadValuesException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    addFrameToPool(new FrameInfo("TEST", 100, FrameType.CK, 100, 99), pool);
    pool.addStrings("FRAME_100_CLASS", Arrays.asList("TEST"));
    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);
    factory.createFrameInfo(100, retriever, emptyEphemerisMap);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetCenterCodeNotFoundException() throws Exception {
    factory.getCenterCode(-100, "NAME", new KernelPoolValidatingRetriever(new BasicKernelPool()),
        new HashMap<String, Integer>());
  }

  @Test
  public void testGetCenterCode() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addIntegers("FRAME_-100_CENTER", Arrays.asList(-1000));
    pool.addIntegers("FRAME_NAME_CENTER", Arrays.asList(-1010));
    pool.addStrings("FRAME_100_CENTER", Arrays.asList("1020"));
    pool.addStrings("FRAME_ANAME_CENTER", Arrays.asList("-1030"));
    pool.addStrings("FRAME_200_CENTER", Arrays.asList("OBJECT"));
    pool.addStrings("FRAME_BNAME_CENTER", Arrays.asList("AOBJECT"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> codeMap = Maps.newHashMap();
    codeMap.put("OBJECT", 1040);
    codeMap.put("AOBJECT", 1050);

    assertEquals(-1000, factory.getCenterCode(-100, "NOTFOUND", retriever, codeMap));
    assertEquals(-1000, factory.getCenterCode(-100, "NAME", retriever, codeMap));
    assertEquals(-1010, factory.getCenterCode(0, "NAME", retriever, codeMap));

    assertEquals(1020, factory.getCenterCode(100, "NOTFOUND", retriever, codeMap));
    assertEquals(1020, factory.getCenterCode(100, "ANAME", retriever, codeMap));
    assertEquals(-1030, factory.getCenterCode(0, "ANAME", retriever, codeMap));

    assertEquals(1040, factory.getCenterCode(200, "NOTFOUND", retriever, codeMap));
    assertEquals(1040, factory.getCenterCode(200, "BNAME", retriever, codeMap));
    assertEquals(1050, factory.getCenterCode(0, "BNAME", retriever, codeMap));

  }

  @Test(expected = NumberFormatException.class)
  public void testGetBodyCodeIntegerParseException() {
    factory.getBodyCode(emptyEphemerisMap, "A");
  }

  @Test
  public void testGetBodyCode() {

    Map<String, Integer> codeMap = Maps.newHashMap();
    codeMap.put("A", 100);
    codeMap.put("B", 200);

    assertEquals(100, factory.getBodyCode(codeMap, "A"));
    assertEquals(100, factory.getBodyCode(codeMap, "100"));
    assertEquals(200, factory.getBodyCode(codeMap, "200"));
    assertEquals(200, factory.getBodyCode(codeMap, "B"));
    assertEquals(200, factory.getBodyCode(codeMap, "b"));

  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetSingleIntegerFrameValueIntegerKeywordRetrievalException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("FRAME_-1000_CLASS", Arrays.asList("AB"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    factory.getSingleIntegerFrameValue(-1000, "NAME", "CLASS", retriever);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetSingleIntegerFrameValueNameKeywordRetrievalException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("FRAME_NAME_CLASS", Arrays.asList("AB"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    factory.getSingleIntegerFrameValue(-1000, "NAME", "CLASS", retriever);
  }

  @Test
  public void testGetSingleIntegerFrameValue() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addIntegers("FRAME_-1000_CLASS", Arrays.asList(1));
    pool.addIntegers("FRAME_1000_CLASS", Arrays.asList(2));
    pool.addIntegers("FRAME_NAME_CLASS", Arrays.asList(3));
    pool.addIntegers("FRAME_NaMe_CLASS", Arrays.asList(4));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    assertEquals(1, factory.getSingleIntegerFrameValue(-1000, "NAME", "CLASS", retriever));

    assertEquals(2, factory.getSingleIntegerFrameValue(1000, "NAME", "CLASS", retriever));

    assertEquals(3, factory.getSingleIntegerFrameValue(200, "NAME", "CLASS", retriever));

    assertEquals(4, factory.getSingleIntegerFrameValue(200, "NaMe", "CLASS", retriever));

  }

  @Ignore
  @Test
  public void testCreateTKFrameFunctions() {
    fail();
  }

  private void addFrameAliasToPool(String name, int code, BasicKernelPool pool) {
    pool.addIntegers(String.format("FRAME_%s", name), Arrays.asList(code));
  }

  /**
   * Simple function that injects the appropriate, correct contents into the kernel pool for the
   * supplied frame definition.
   * 
   * @param info
   * @param pool
   */
  private void addFrameToPool(FrameInfo info, BasicKernelPool pool) {

    pool.addIntegers(String.format("FRAME_%d_CLASS", info.getCode()),
        Arrays.asList(info.getType().getTypeID()));
    pool.addIntegers(String.format("FRAME_%d_CLASS_ID", info.getCode()),
        Arrays.asList(info.getClassID()));
    pool.addIntegers(String.format("FRAME_%d_CENTER", info.getCode()),
        Arrays.asList(info.getCenterID()));
    pool.addStrings(String.format("FRAME_%d_NAME", info.getCode()), Arrays.asList(info.getName()));

  }

}
