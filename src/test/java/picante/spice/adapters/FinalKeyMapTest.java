package picante.spice.adapters;

import static org.easymock.EasyMock.checkOrder;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import org.junit.Before;
import org.junit.Test;

public class FinalKeyMapTest {

  private FinalKeyMap<String, Integer> emptyMap;
  private FinalKeyMap<String, Integer> defaultMap;
  private FinalKeyMap<String, Integer> augmentedMap;
  private HashMap<String, Integer> baseMap;
  private HashMap<String, Integer> augmentedBaseMap;
  private Map<String, Integer> mock;
  private FinalKeyMap<String, Integer> mockWrapper;
  private HashMap<String, Integer> map;

  @Before
  public void setUp() throws Exception {

    map = new HashMap<String, Integer>();
    map.put("A", 10);
    map.put("B", 20);
    map.put("C", 30);

    baseMap = new HashMap<String, Integer>();
    baseMap.put("A", 1);
    baseMap.put("B", 2);
    baseMap.put("C", 3);

    augmentedBaseMap = new HashMap<String, Integer>(baseMap);
    augmentedBaseMap.put("D", 4);
    augmentedBaseMap.put("E", 4);

    emptyMap = new FinalKeyMap<String, Integer>();

    defaultMap = new FinalKeyMap<String, Integer>(new HashMap<String, Integer>(baseMap));

    augmentedMap = new FinalKeyMap<String, Integer>(new HashMap<String, Integer>(baseMap));
    augmentedMap.put("D", 4);
    augmentedMap.put("E", 4);

    mock = createMock(StringIntegerMap.class);
    checkOrder(mock, true);
    mockWrapper = new FinalKeyMap<String, Integer>(mock);
  }

  @Test
  public void testHashCode() {
    assertEquals(new HashMap<String, Integer>().hashCode(), emptyMap.hashCode());
    assertEquals(baseMap.hashCode(), defaultMap.hashCode());
    assertEquals(augmentedBaseMap.hashCode(), augmentedMap.hashCode());
  }

  @Test
  public void testFinalKeyMapMap() {
    /*
     * Simply check that the default constructor creates a basic map with no entries.
     */
    assertEquals(0, new FinalKeyMap<String, Integer>().size());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testClear() {
    mock.size();
    expectLastCall().andReturn(10);
    replay(mock);
    mockWrapper.clear();
  }

  @Test
  public void testClearEmpty() {
    mock.size();
    expectLastCall().andReturn(0);
    mock.clear();
    replay(mock);
    mockWrapper.clear();
    verify(mock);
  }

  @Test
  public void testContainsKey() {
    mock.containsKey("A");
    expectLastCall().andReturn(true);
    replay(mock);
    assertTrue(mockWrapper.containsKey("A"));
    verify(mock);
  }

  @Test
  public void testContainsValue() {
    mock.containsValue(3);
    expectLastCall().andReturn(true);
    replay(mock);
    assertTrue(mockWrapper.containsValue(3));
    verify(mock);
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test(expected = UnsupportedOperationException.class)
  public void testEntrySet() {
    mock.entrySet();
    expectLastCall().andReturn(baseMap.entrySet());
    replay(mock);
    Set<Entry<String, Integer>> result = mockWrapper.entrySet();
    assertEquals(baseMap.entrySet(), result);
    verify(mock);
    result.remove(1);
  }

  @Test
  public void testEqualsObject() {
    assertTrue(emptyMap.equals(new HashMap<String, Integer>()));
    assertTrue(defaultMap.equals(baseMap));
    assertTrue(augmentedMap.equals(augmentedBaseMap));
  }

  @Test
  public void testGet() {
    mock.get("A");
    expectLastCall().andReturn(1);
    replay(mock);
    assertEquals(Integer.valueOf(1), mockWrapper.get("A"));
    verify(mock);
  }

  @Test
  public void testIsEmpty() {
    mock.isEmpty();
    expectLastCall().andReturn(true);
    replay(mock);
    assertEquals(true, mockWrapper.isEmpty());
    verify(mock);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testKeySetEmpty() {
    mock.keySet();
    expectLastCall().andReturn(new TreeSet<String>());
    replay(mock);
    Set<String> result = mockWrapper.keySet();
    assertEquals(new TreeSet<String>(), result);
    verify(mock);
    result.add("F");
  }

  @Test
  public void testPutNewKey() {
    mock.containsKey("A");
    expectLastCall().andReturn(false);
    mock.put("A", 10);
    expectLastCall().andReturn(null);
    replay(mock);
    assertNull(mockWrapper.put("A", 10));
    verify(mock);
  }

  @Test
  public void testPutNoOp() {
    mock.containsKey("A");
    expectLastCall().andReturn(true);
    mock.get("A");
    expectLastCall().andReturn(10);
    mock.put("A", 10);
    expectLastCall().andReturn(10);
    replay(mock);
    assertEquals(Integer.valueOf(10), mockWrapper.put("A", 10));
    verify(mock);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testPutException() {
    mock.containsKey("A");
    expectLastCall().andReturn(true);
    mock.get("A");
    expectLastCall().andReturn(10);
    replay(mock);
    mockWrapper.put("A", 20);
  }

  @Test
  public void testPutAllNewKeys() {
    for (Entry<String, Integer> e : map.entrySet()) {
      mock.containsKey(e.getKey());
      expectLastCall().andReturn(false);
    }
    mock.putAll(map);
    replay(mock);
    mockWrapper.putAll(map);
    verify(mock);
  }

  @Test
  public void testPutAllMixNewKeysAndOldNoOp() {
    Set<String> keyset = map.keySet();
    Iterator<String> iterator = keyset.iterator();

    /*
     * Just toggle between key present and key not present.
     */
    for (int i = 0; i < keyset.size(); i++) {
      String key = iterator.next();
      if (i % 2 == 0) {
        mock.containsKey(key);
        expectLastCall().andReturn(true);
        mock.get(key);
        expectLastCall().andReturn(map.get(key));
      } else {
        mock.containsKey(key);
        expectLastCall().andReturn(false);
      }
    }
    mock.putAll(map);
    replay(mock);
    mockWrapper.putAll(map);
    verify(mock);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testPutAllException() {
    Iterator<Entry<String, Integer>> iterator = map.entrySet().iterator();
    Entry<String, Integer> entry = iterator.next();
    mock.containsKey(entry.getKey());
    expectLastCall().andReturn(true);
    mock.get(entry.getKey());
    expectLastCall().andReturn(entry.getValue() - 10);
    replay(mock);
    mockWrapper.putAll(map);
  }

  @Test
  public void testCheckKeyValueKeyPresentNoOp() {
    defaultMap.checkKeyValue("A", 1);
    assertEquals(defaultMap, baseMap);
  }

  @Test
  public void testCheckKeyValueKeyAbsentNoOp() {
    defaultMap.checkKeyValue("Z", 12);
    assertEquals(defaultMap, baseMap);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCheckKeyValueException() {
    defaultMap.checkKeyValue("A", 5);
  }

  @Test
  public void testRemoveNoOp() {
    mock.containsKey("A");
    expectLastCall().andReturn(false);
    mock.remove("A");
    expectLastCall().andReturn(null);
    replay(mock);
    assertNull(mockWrapper.remove("A"));
    verify(mock);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testRemoveException() {
    mock.containsKey("A");
    expectLastCall().andReturn(true);
    replay(mock);
    mockWrapper.remove("A");
  }

  @Test
  public void testSize() {
    mock.size();
    expectLastCall().andReturn(4);
    replay(mock);
    assertEquals(4, mockWrapper.size());
    verify(mock);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testValues() {
    mock.values();
    expectLastCall().andReturn(baseMap.values());
    replay(mock);
    Collection<Integer> result = mockWrapper.values();
    assertEquals(new ArrayList<Integer>(baseMap.values()), new ArrayList<Integer>(result));
    verify(mock);
    result.remove(1);
  }

}


/**
 * Interface necessary to remove generic dependency on mock implementation.
 */
interface StringIntegerMap extends Map<String, Integer> {
}
