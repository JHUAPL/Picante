package picante.spice;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createStrictMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import picante.mechanics.FrameID;
import picante.spice.FrameIDBindingManager.BindingResult;
import picante.spice.NameIDBindingList.NameIDPair;
import picante.spice.adapters.SpiceFrameID;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.provided.FrameNames;

public class FrameIDBindingManagerTest {

  private NameIDBindingList<FrameID> mockList;
  private FrameNames mockNames;
  private FrameIDBindingManager manager;

  @Before
  public void setUp() throws Exception {
    mockList = createStrictMock(NameIDBindingList.class);
    mockNames = createMock(FrameNames.class);
    manager = new FrameIDBindingManager(mockList, mockNames);
  }

  @Test
  public void testAdd() {
    FrameID sample = new TestID("sample");
    mockList.add("sample", sample);
    replay(mockList);
    manager.add("sample", sample);
    verify(mockList);
  }

  @Test
  public void testAddAll() {
    Map<String, FrameID> test = Maps.newHashMap();
    mockList.addAll(test);
    replay(mockList);
    manager.addAll(test);
    verify(mockList);
  }

  @Test
  public void testRemove() {
    mockList.remove("sample");
    replay(mockList);
    manager.remove("sample");
    verify(mockList);
  }

  @Test
  public void testRemoveAll() {
    List<String> test = Lists.newArrayList();
    mockList.removeAll(test);
    replay(mockList);
    manager.removeAll(test);
    verify(mockList);
  }

  @Test
  public void testClear() {
    mockList.clear();
    replay(mockList);
    manager.clear();
    verify(mockList);
  }

  private void configureStandardMockNames() {
    BiMap<Integer, FrameInfo> nameCode = HashBiMap.create();
    nameCode.put(10013, new FrameInfo("IAU_EARTH", 10013, FrameType.PCK, 399, 399));
    nameCode.put(10014, new FrameInfo("IAU_MARS", 10014, FrameType.PCK, 499, 499));
    nameCode.put(1, new FrameInfo("J2000", 1, FrameType.INERTIAL, 1, 0));

    Map<Integer, FrameID> codeMap = Maps.newHashMap();
    codeMap.put(10013, new TestID("IAU_EARTH"));
    codeMap.put(10014, new TestID("IAU_MARS"));
    codeMap.put(1, new TestID("J2000"));

    mockNames.getMap();
    expectLastCall().andReturn(nameCode).anyTimes();
    mockNames.getStandardBindings();
    expectLastCall().andReturn(codeMap).anyTimes();
  }

  private void configureStandardMockList() {

    List<NameIDBindingList.NameIDPair<FrameID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<FrameID>("A", new TestID("A")));
    iterable.add(new NameIDPair<FrameID>("B", new TestID("B")));
    iterable.add(new NameIDPair<FrameID>("C", new TestID("C")));
    iterable.add(new NameIDPair<FrameID>("EXTRA", new TestID("EXTRA")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());
  }

  @Test
  public void testCreateDefinitions() {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    /*
     * Now check the situation when there is no overlap between the kernel pool defined frames and
     * those that are "built-in".
     */
    Map<Integer, FrameInfo> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put(100, new FrameInfo("A", 100, FrameType.CK, 100, 100));
    kernelDefinitions.put(200, new FrameInfo("B", 200, FrameType.DYNAMIC, 200, 200));

    ImmutableBiMap<Integer, FrameInfo> result = manager.createDefinitions(kernelDefinitions);

    /*
     * There should be a total of five frames defined:
     */
    Set<FrameInfo> expected = Sets.newHashSet();
    expected.add(new FrameInfo("B", 200, FrameType.DYNAMIC, 200, 200));
    expected.add(new FrameInfo("A", 100, FrameType.CK, 100, 100));
    expected.add(new FrameInfo("IAU_EARTH", 10013, FrameType.PCK, 399, 399));
    expected.add(new FrameInfo("IAU_MARS", 10014, FrameType.PCK, 499, 499));
    expected.add(new FrameInfo("J2000", 1, FrameType.INERTIAL, 1, 0));

    assertEquals(expected, result.values());

  }

  @Test
  public void testCreateDefinitionsAttemptToOverrideBuiltIn() {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    /*
     * Now check the situation when there is overlap between the kernel pool defined frames and
     * those that are "built-in".
     */
    Map<Integer, FrameInfo> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put(100, new FrameInfo("A", 100, FrameType.CK, 100, 100));
    kernelDefinitions.put(200, new FrameInfo("B", 200, FrameType.DYNAMIC, 200, 200));
    kernelDefinitions.put(10013, new FrameInfo("C", 10013, FrameType.CK, 100, 394));

    ImmutableBiMap<Integer, FrameInfo> result = manager.createDefinitions(kernelDefinitions);

    /*
     * There should be a total of five frames defined. The attempt to replace 10013 from the
     * built-in codes should fail to be included.
     */
    Set<FrameInfo> expected = Sets.newHashSet();
    expected.add(new FrameInfo("B", 200, FrameType.DYNAMIC, 200, 200));
    expected.add(new FrameInfo("A", 100, FrameType.CK, 100, 100));
    expected.add(new FrameInfo("IAU_EARTH", 10013, FrameType.PCK, 399, 399));
    expected.add(new FrameInfo("IAU_MARS", 10014, FrameType.PCK, 499, 499));
    expected.add(new FrameInfo("J2000", 1, FrameType.INERTIAL, 1, 0));

    assertEquals(expected, result.values());

  }

  @Test
  public void testCreateAliasMap() {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<String, Integer> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put("A", 100);
    kernelDefinitions.put("B", 200);
    kernelDefinitions.put("C", 300);

    ImmutableMap<String, Integer> result = manager.createAliasMap(kernelDefinitions);

    Map<String, Integer> expected = Maps.newHashMap();
    expected.put("A", 100);
    expected.put("B", 200);
    expected.put("C", 300);
    expected.put("IAU_EARTH", 10013);
    expected.put("IAU_MARS", 10014);
    expected.put("J2000", 1);

    assertEquals(expected, result);
  }

  @Test
  public void testCreateAliasMapWithNonCanonicalName() {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<String, Integer> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put("A", 100);
    kernelDefinitions.put("B", 200);
    kernelDefinitions.put("C", 300);
    kernelDefinitions.put("NonCanonical", 1000);

    ImmutableMap<String, Integer> result = manager.createAliasMap(kernelDefinitions);

    Map<String, Integer> expected = Maps.newHashMap();
    expected.put("A", 100);
    expected.put("B", 200);
    expected.put("C", 300);
    expected.put("IAU_EARTH", 10013);
    expected.put("IAU_MARS", 10014);
    expected.put("J2000", 1);
    /*
     * This will work, because we are not actively defending against it at this level; though it
     * should never happen due to the way the FKFactory class is implemented.
     */
    expected.put("NonCanonical", 1000);

    assertEquals(expected, result);

  }

  @Test
  public void testCreateAliasMapWithAttemptToOverrideBuiltIn() {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<String, Integer> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put("A", 100);
    kernelDefinitions.put("B", 200);
    kernelDefinitions.put("C", 300);
    kernelDefinitions.put("IAU_EARTH", 314);

    ImmutableMap<String, Integer> result = manager.createAliasMap(kernelDefinitions);

    /*
     * The attempt to override IAU_EARTH should fail here.
     */
    Map<String, Integer> expected = Maps.newHashMap();
    expected.put("A", 100);
    expected.put("B", 200);
    expected.put("C", 300);
    expected.put("IAU_EARTH", 10013);
    expected.put("IAU_MARS", 10014);
    expected.put("J2000", 1);

    assertEquals(expected, result);

  }

  @Test
  public void testCreateClassMap() {

    ImmutableMap.Builder<Integer, FrameID> codeMapBuilder = ImmutableMap.builder();
    codeMapBuilder.put(100, new TestID("A"));
    codeMapBuilder.put(200, new TestID("B"));
    codeMapBuilder.put(300, new TestID("C"));
    codeMapBuilder.put(400, new TestID("D"));
    codeMapBuilder.put(500, new TestID("E"));
    codeMapBuilder.put(700, new TestID("G"));

    ImmutableBiMap.Builder<Integer, FrameInfo> frameBuilder = ImmutableBiMap.builder();
    frameBuilder.put(100, new FrameInfo("FRAME_A", 100, FrameType.INERTIAL, 101, 102));
    frameBuilder.put(200, new FrameInfo("FRAME_B", 200, FrameType.PCK, 201, 202));
    frameBuilder.put(300, new FrameInfo("FRAME_C", 300, FrameType.CK, 301, 302));
    frameBuilder.put(400, new FrameInfo("FRAME_D", 400, FrameType.TK, 401, 402));
    frameBuilder.put(500, new FrameInfo("FRAME_E", 500, FrameType.DYNAMIC, 501, 502));
    frameBuilder.put(600, new FrameInfo("FRAME_F", 600, FrameType.CK, 601, 602));
    frameBuilder.put(700, new FrameInfo("FRAME_G", 700, FrameType.CK, 301, 302));

    ImmutableMap<FrameType, ImmutableSetMultimap<Integer, FrameID>> result =
        manager.createClassMap(codeMapBuilder.build(), frameBuilder.build());

    ImmutableSetMultimap<Integer, FrameID> toTest = result.get(FrameType.INERTIAL);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(101));
    assertEquals(1, toTest.get(101).size());
    assertTrue(toTest.get(101).contains(new TestID("A")));

    toTest = result.get(FrameType.PCK);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(201));
    assertEquals(1, toTest.get(201).size());
    assertTrue(toTest.get(201).contains(new TestID("B")));

    toTest = result.get(FrameType.TK);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(401));
    assertEquals(1, toTest.get(401).size());
    assertTrue(toTest.get(401).contains(new TestID("D")));

    toTest = result.get(FrameType.DYNAMIC);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(501));
    assertEquals(1, toTest.get(501).size());
    assertTrue(toTest.get(501).contains(new TestID("E")));

    toTest = result.get(FrameType.CK);
    assertEquals(3, toTest.size());
    assertTrue(toTest.containsKey(601));
    assertEquals(1, toTest.get(601).size());
    assertTrue(toTest.get(601).contains(new SpiceFrameID(600)));
    assertTrue(toTest.containsKey(301));
    assertEquals(2, toTest.get(301).size());
    assertTrue(toTest.get(301).contains(new TestID("C")));
    assertTrue(toTest.get(301).contains(new TestID("G")));

  }

  @Test
  public void testCreateClassMapEmptyType() {

    ImmutableMap.Builder<Integer, FrameID> codeMapBuilder = ImmutableMap.builder();
    codeMapBuilder.put(100, new TestID("A"));
    codeMapBuilder.put(200, new TestID("B"));
    codeMapBuilder.put(300, new TestID("C"));
    codeMapBuilder.put(400, new TestID("D"));
    codeMapBuilder.put(500, new TestID("E"));
    codeMapBuilder.put(700, new TestID("G"));

    ImmutableBiMap.Builder<Integer, FrameInfo> frameBuilder = ImmutableBiMap.builder();
    frameBuilder.put(100, new FrameInfo("FRAME_A", 100, FrameType.INERTIAL, 101, 102));
    frameBuilder.put(200, new FrameInfo("FRAME_B", 200, FrameType.PCK, 201, 202));
    frameBuilder.put(400, new FrameInfo("FRAME_D", 400, FrameType.TK, 401, 402));
    frameBuilder.put(500, new FrameInfo("FRAME_E", 500, FrameType.DYNAMIC, 501, 502));

    ImmutableMap<FrameType, ImmutableSetMultimap<Integer, FrameID>> result =
        manager.createClassMap(codeMapBuilder.build(), frameBuilder.build());

    ImmutableSetMultimap<Integer, FrameID> toTest = result.get(FrameType.INERTIAL);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(101));
    assertEquals(1, toTest.get(101).size());
    assertTrue(toTest.get(101).contains(new TestID("A")));

    toTest = result.get(FrameType.PCK);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(201));
    assertEquals(1, toTest.get(201).size());
    assertTrue(toTest.get(201).contains(new TestID("B")));

    toTest = result.get(FrameType.TK);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(401));
    assertEquals(1, toTest.get(401).size());
    assertTrue(toTest.get(401).contains(new TestID("D")));

    toTest = result.get(FrameType.DYNAMIC);
    assertEquals(1, toTest.size());
    assertTrue(toTest.containsKey(501));
    assertEquals(1, toTest.get(501).size());
    assertTrue(toTest.get(501).contains(new TestID("E")));

    toTest = result.get(FrameType.CK);
    assertEquals(0, toTest.size());

  }

  @Test(expected = BindingConflictException.DuplicateFrameCode.class)
  public void testCreateFrameIDMapReusedBindingException() throws Exception {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<Integer, FrameInfo> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put(10000, new FrameInfo("A", 10000, FrameType.TK, 10001, 10002));

    /*
     * Explicitly setup frame definitions that result in different frame codes being bound to the
     * same frame.
     */
    Map<String, Integer> kernelAliases = Maps.newHashMap();
    kernelAliases.put("A", 10000);
    kernelAliases.put("B", 10000);

    manager.createFrameIDMap(kernelDefinitions, kernelAliases);

  }

  @Test(expected = BindingConflictException.DuplicateFrameID.class)
  public void testCreateFrameIDMapReusedIDException() throws Exception {

    List<NameIDBindingList.NameIDPair<FrameID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<FrameID>("A", new TestID("A")));
    iterable.add(new NameIDPair<FrameID>("B", new TestID("B")));
    iterable.add(new NameIDPair<FrameID>("C", new TestID("A")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());

    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<Integer, FrameInfo> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put(10000, new FrameInfo("A", 10000, FrameType.TK, 10001, 10002));

    /*
     * Explicitly setup a situation where the bindings requested use the same ID for two distinct
     * frames.
     */
    Map<String, Integer> kernelAliases = Maps.newHashMap();
    kernelAliases.put("A", 10000);
    kernelAliases.put("B", 10001);
    kernelAliases.put("C", 10002);

    manager.createFrameIDMap(kernelDefinitions, kernelAliases);

  }

  @Test(expected = BindingConflictException.DuplicateFrameID.class)
  public void testCreateFrameIDMapRepurposedDefaultIDException() throws Exception {

    List<NameIDBindingList.NameIDPair<FrameID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<FrameID>("A", new TestID("A")));
    iterable.add(new NameIDPair<FrameID>("B", new TestID("B")));
    iterable.add(new NameIDPair<FrameID>("C", new TestID("J2000")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());

    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<Integer, FrameInfo> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put(10000, new FrameInfo("A", 10000, FrameType.TK, 10001, 10002));

    /*
     * Explicitly setup a situation where the bindings requested use the same ID for two distinct
     * frames.
     */
    Map<String, Integer> kernelAliases = Maps.newHashMap();
    kernelAliases.put("A", 10000);
    kernelAliases.put("B", 10001);
    kernelAliases.put("C", 10002);

    manager.createFrameIDMap(kernelDefinitions, kernelAliases);

  }

  @Test
  public void testCreateFrameIDMap() throws Exception {

    configureStandardMockList();
    configureStandardMockNames();

    replay(mockList, mockNames);

    Map<Integer, FrameInfo> kernelDefinitions = Maps.newHashMap();
    kernelDefinitions.put(-10000, new FrameInfo("A", -10000, FrameType.TK, -10001, -10002));
    kernelDefinitions.put(-10001, new FrameInfo("B", -10001, FrameType.TK, -10001, -10003));
    kernelDefinitions.put(-10002, new FrameInfo("C", -10002, FrameType.DYNAMIC, -10002, -10003));

    Map<String, Integer> kernelAliases = Maps.newHashMap();
    kernelAliases.put("A", -10000);
    kernelAliases.put("B", -10001);
    kernelAliases.put("C", -10002);

    BindingResult result = manager.createFrameIDMap(kernelDefinitions, kernelAliases);

    /*
     * Check each of the three components of result.
     */
    List<NameIDPair<FrameID>> unused = result.getUnused();
    assertEquals(1, unused.size());
    assertEquals("EXTRA", unused.get(0).getName());
    assertEquals(new TestID("EXTRA"), unused.get(0).getID());

    Map<Integer, FrameID> codeMap = result.getFrameCodeMap();
    Map<Integer, FrameID> expectedCodeMap = Maps.newHashMap();
    expectedCodeMap.put(-10000, new TestID("A"));
    expectedCodeMap.put(-10001, new TestID("B"));
    expectedCodeMap.put(-10002, new TestID("C"));
    expectedCodeMap.put(10013, new TestID("IAU_EARTH"));
    expectedCodeMap.put(10014, new TestID("IAU_MARS"));
    expectedCodeMap.put(1, new TestID("J2000"));
    assertEquals(expectedCodeMap, codeMap);

    SetMultimap<Integer, FrameID> classMap = result.getClassCodeMapForType(FrameType.INERTIAL);
    SetMultimap<Integer, FrameID> expectedClassMap = HashMultimap.create();
    expectedClassMap.put(1, new TestID("J2000"));
    assertEquals(expectedClassMap, classMap);

    classMap = result.getClassCodeMapForType(FrameType.PCK);
    expectedClassMap = HashMultimap.create();
    expectedClassMap.put(399, new TestID("IAU_EARTH"));
    expectedClassMap.put(499, new TestID("IAU_MARS"));
    assertEquals(expectedClassMap, classMap);

    classMap = result.getClassCodeMapForType(FrameType.CK);
    expectedClassMap = HashMultimap.create();
    assertEquals(expectedClassMap, classMap);

    classMap = result.getClassCodeMapForType(FrameType.TK);
    expectedClassMap = HashMultimap.create();
    expectedClassMap.put(-10001, new TestID("A"));
    expectedClassMap.put(-10001, new TestID("B"));
    assertEquals(expectedClassMap, classMap);

    classMap = result.getClassCodeMapForType(FrameType.DYNAMIC);
    expectedClassMap = HashMultimap.create();
    expectedClassMap.put(-10002, new TestID("C"));
    assertEquals(expectedClassMap, classMap);

  }


  private static class TestID implements FrameID {

    private final String name;

    private TestID(String name) {
      this.name = name;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public boolean isInertial() {
      return false;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((name == null) ? 0 : name.hashCode());
      return result;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      TestID other = (TestID) obj;
      if (name == null) {
        if (other.name != null) {
          return false;
        }
      } else if (!name.equals(other.name)) {
        return false;
      }
      return true;
    }

  }

}
