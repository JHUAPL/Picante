package picante.spice;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import picante.spice.NameIDBindingList.NameIDPair;

public class NameIDBindingListTest {

  private NameIDBindingList<Integer> bindingList;

  @Before
  public void setUp() throws Exception {
    bindingList = new NameIDBindingList<Integer>();
  }

  @Test
  public void testCanonicalizationOfInputNames() {
    bindingList.add(" a ", 1);
    Map<String, Integer> map = new LinkedHashMap<String, Integer>();
    map.put(" b ", 2);
    map.put("c", 3);
    bindingList.addAll(map);

    Iterator<NameIDPair<Integer>> iterator = bindingList.iterator();

    NameIDPair<Integer> pair = iterator.next();
    assertEquals("A", pair.getName());
    assertEquals(Integer.valueOf(1), pair.getID());

    pair = iterator.next();
    assertEquals("B", pair.getName());
    assertEquals(Integer.valueOf(2), pair.getID());

    pair = iterator.next();
    assertEquals("C", pair.getName());
    assertEquals(Integer.valueOf(3), pair.getID());

    assertFalse(iterator.hasNext());
  }

  @Test
  public void testAdd() {
    bindingList.add("A", 1);
    bindingList.add("B", 2);
    bindingList.add("A", 3);

    Iterator<NameIDPair<Integer>> iterator = bindingList.iterator();

    NameIDPair<Integer> pair = iterator.next();
    assertEquals("A", pair.getName());
    assertEquals(Integer.valueOf(1), pair.getID());

    pair = iterator.next();
    assertEquals("B", pair.getName());
    assertEquals(Integer.valueOf(2), pair.getID());

    pair = iterator.next();
    assertEquals("A", pair.getName());
    assertEquals(Integer.valueOf(3), pair.getID());

    assertFalse(iterator.hasNext());
  }

  @Test
  public void testAddAll() {

    Map<String, Integer> map = new LinkedHashMap<String, Integer>();
    map.put("A", 1);
    map.put("B", 2);
    map.put("C", 3);
    bindingList.addAll(map);

    Iterator<NameIDPair<Integer>> iterator = bindingList.iterator();

    NameIDPair<Integer> pair = iterator.next();
    assertEquals("A", pair.getName());
    assertEquals(Integer.valueOf(1), pair.getID());

    pair = iterator.next();
    assertEquals("B", pair.getName());
    assertEquals(Integer.valueOf(2), pair.getID());

    pair = iterator.next();
    assertEquals("C", pair.getName());
    assertEquals(Integer.valueOf(3), pair.getID());

    assertFalse(iterator.hasNext());
  }

  @Test
  public void testRemove() {
    bindingList.add("A", 1);
    Map<String, Integer> map = new LinkedHashMap<String, Integer>();
    map.put("B", 2);
    map.put("C", 3);
    map.put("A", 4);
    map.put("D", 5);
    bindingList.addAll(map);

    bindingList.remove(" a ");

    Iterator<NameIDPair<Integer>> iterator = bindingList.iterator();

    NameIDPair<Integer> pair = iterator.next();
    assertEquals("A", pair.getName());
    assertEquals(Integer.valueOf(1), pair.getID());

    pair = iterator.next();
    assertEquals("B", pair.getName());
    assertEquals(Integer.valueOf(2), pair.getID());

    pair = iterator.next();
    assertEquals("C", pair.getName());
    assertEquals(Integer.valueOf(3), pair.getID());

    pair = iterator.next();
    assertEquals("D", pair.getName());
    assertEquals(Integer.valueOf(5), pair.getID());

    assertFalse(iterator.hasNext());
  }

  @Test
  public void testRemoveAllIterableOfString() {
    bindingList.add("A", 1);
    bindingList.add("B", 2);
    bindingList.add("C", 3);
    bindingList.add("D", 4);
    bindingList.add("E", 5);
    bindingList.add("A", 6);
    bindingList.add("D", 7);

    List<String> toRemove = new ArrayList<String>();
    toRemove.add("A");
    toRemove.add("B");
    toRemove.add("c");
    toRemove.add("  d ");
    toRemove.add("E");

    bindingList.removeAll(toRemove);

    Iterator<NameIDPair<Integer>> iterator = bindingList.iterator();

    NameIDPair<Integer> pair = iterator.next();
    assertEquals("A", pair.getName());
    assertEquals(Integer.valueOf(1), pair.getID());

    pair = iterator.next();
    assertEquals("D", pair.getName());
    assertEquals(Integer.valueOf(4), pair.getID());

    assertFalse(iterator.hasNext());

  }

  @Test
  public void testClear() {
    bindingList.add("A", 1);
    bindingList.add("B", 2);
    bindingList.add("A", 3);

    bindingList.clear();

    assertFalse(bindingList.iterator().hasNext());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIteratorRemoveUnsupportedException() {
    bindingList.add("A", 1);

    Iterator<NameIDPair<Integer>> iterator = bindingList.iterator();
    iterator.next();
    iterator.remove();
  }

}
