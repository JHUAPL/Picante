package picante.spice;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createStrictMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import picante.mechanics.EphemerisID;
import picante.spice.EphemerisIDBindingManager.BindingResult;
import picante.spice.NameIDBindingList.NameIDPair;
import picante.spice.provided.EphemerisNames;

public class EphemerisIDBindingManagerTest {

  private NameIDBindingList<EphemerisID> mockList;
  private EphemerisNames mockNames;
  private EphemerisIDBindingManager manager;

  @Before
  public void setUp() throws Exception {
    mockList = createStrictMock(NameIDBindingList.class);
    mockNames = createMock(EphemerisNames.class);
    manager = new EphemerisIDBindingManager(mockList, mockNames);
  }

  @Test
  public void testAdd() {
    EphemerisID sample = new TestID("sample");
    mockList.add("sample", sample);
    replay(mockList);
    manager.add("sample", sample);
    verify(mockList);
  }

  @Test
  public void testAddAll() {
    Map<String, EphemerisID> test = Maps.newHashMap();
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
    List<String> test = new ArrayList<String>();
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

    Map<String, Integer> nameCode = Maps.newHashMap();
    nameCode.put("EARTH", 399);
    nameCode.put("EARTH2", 399);
    nameCode.put("MARS", 499);

    Map<Integer, EphemerisID> codeMap = Maps.newHashMap();
    codeMap.put(399, new TestID("EARTH"));
    codeMap.put(499, new TestID("MARS"));

    mockNames.getMap();
    expectLastCall().andReturn(nameCode);
    mockNames.getStandardBindings();
    expectLastCall().andReturn(codeMap);

  }

  private void configureStandardMockList() {

    List<NameIDBindingList.NameIDPair<EphemerisID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<EphemerisID>("A", new TestID("A")));
    iterable.add(new NameIDPair<EphemerisID>("B", new TestID("B")));
    iterable.add(new NameIDPair<EphemerisID>("C", new TestID("C")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());

  }

  private Map<String, Integer> createKernelDefinitions() {

    Map<String, Integer> result = Maps.newHashMap();
    result.put("A", -1);
    result.put("B", -2);
    result.put("ALTERNATEB", -2);

    return result;

  }

  @Test
  public void testCreateEphemerisIDMap() throws Exception {

    configureStandardMockNames();
    configureStandardMockList();

    replay(mockList, mockNames);

    BindingResult result = manager.createEphemerisIDMap(createKernelDefinitions());

    /*
     * We should have only one unused binding, for "C".
     */
    assertEquals(1, result.getUnused().size());
    assertEquals(new TestID("C"), result.getUnused().get(0).getID());
    assertEquals("C", result.getUnused().get(0).getName());

    /*
     * The successful bindings should include the built-in ones as well as A and B.
     */
    assertEquals(4, result.getMap().size());
    assertTrue("Testing presence of -1", result.getMap().containsKey(-1));
    assertTrue("Testing presence of -2", result.getMap().containsKey(-2));
    assertTrue("Testing presence of 399", result.getMap().containsKey(399));
    assertTrue("Testing presence of 499", result.getMap().containsKey(499));

    assertEquals(new TestID("EARTH"), result.getMap().get(399));
    assertEquals(new TestID("MARS"), result.getMap().get(499));
    assertEquals(new TestID("A"), result.getMap().get(-1));
    assertEquals(new TestID("B"), result.getMap().get(-2));

  }

  /**
   * This test should not really be necessary, as given that the underlying map utilized by the
   * EphemerisNames class is a BiMap; this could never happen. It is interesting to note that even
   * if it does occur, the manager will throw an exception, which is a worthwhile test.
   * 
   * @throws Exception
   */
  @Test(expected = BindingConflictException.class)
  public void testCreateEphemerisIDMapBadEphemerisNamesException() throws Exception {

    Map<String, Integer> nameCode = new HashMap<String, Integer>();
    nameCode.put("EARTH", 399);
    nameCode.put("EARTH2", 399);

    /*
     * Use equivalent IDs for different object ID codes.
     */
    Map<Integer, EphemerisID> codeMap = new HashMap<Integer, EphemerisID>();
    codeMap.put(399, new TestID("EARTH"));
    codeMap.put(499, new TestID("EARTH"));

    mockNames.getMap();
    expectLastCall().andReturn(nameCode);
    mockNames.getStandardBindings();
    expectLastCall().andReturn(codeMap);

    configureStandardMockList();

    replay(mockList, mockNames);

    manager.createEphemerisIDMap(createKernelDefinitions());

  }

  @Test(expected = BindingConflictException.DuplicateEphemerisCode.class)
  public void testCreateEphemerisIDMapIntegerCodeConflictException() throws Exception {

    configureStandardMockNames();

    List<NameIDBindingList.NameIDPair<EphemerisID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<EphemerisID>("A", new TestID("A")));
    iterable.add(new NameIDPair<EphemerisID>("B", new TestID("B")));
    /*
     * Introduce a binding for another string, identical to B, that maps to a different code. This
     * will trigger the desired exception.
     */
    iterable.add(new NameIDPair<EphemerisID>("AlternateB", new TestID("C")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());

    replay(mockList, mockNames);

    manager.createEphemerisIDMap(createKernelDefinitions());

  }

  @Test(expected = BindingConflictException.DuplicateEphemerisID.class)
  public void testCreateEphemerisIDMapReuseBoundIDForAnotherCodeException() throws Exception {

    configureStandardMockNames();

    List<NameIDBindingList.NameIDPair<EphemerisID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<EphemerisID>("A", new TestID("A")));
    /*
     * Alter the bindings to use an equivalent ID for two distinct objects.
     */
    iterable.add(new NameIDPair<EphemerisID>("B", new TestID("A")));
    iterable.add(new NameIDPair<EphemerisID>("C", new TestID("C")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());

    replay(mockList, mockNames);
    manager.createEphemerisIDMap(createKernelDefinitions());

  }

  @Test(expected = BindingConflictException.DuplicateEphemerisID.class)
  public void testCreateEphemerisIDMapRepurposeBuiltInIDException() throws Exception {

    configureStandardMockNames();

    List<NameIDBindingList.NameIDPair<EphemerisID>> iterable = Lists.newArrayList();
    iterable.add(new NameIDPair<EphemerisID>("A", new TestID("A")));
    /*
     * Load up an ID that conflicts with an existing built-in ID.
     */
    iterable.add(new NameIDPair<EphemerisID>("B", new TestID("EARTH")));
    iterable.add(new NameIDPair<EphemerisID>("C", new TestID("C")));

    mockList.iterator();
    expectLastCall().andReturn(iterable.iterator());

    replay(mockList, mockNames);
    manager.createEphemerisIDMap(createKernelDefinitions());

  }

  private static class TestID implements EphemerisID {

    private final String name;

    private TestID(String name) {
      this.name = name;
    }

    @Override
    public String getName() {
      return name;
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
