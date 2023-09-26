package picante.spice.kernelpool;

import java.util.HashSet;
import java.util.Set;
import junit.framework.TestCase;

public class KernelPoolChangeEventTest extends TestCase {

  private HashSet<String> setA;

  private HashSet<String> setB;

  private HashSet<String> setC;

  private HashSet<String> emptySet;

  @Override
  protected void setUp() throws Exception {
    super.setUp();

    setA = new HashSet<String>();
    setB = new HashSet<String>();
    setC = new HashSet<String>();
    emptySet = new HashSet<String>();

    setA.add("A");
    setA.add("B");
    setA.add("C");

    setB.add("E");
    setB.add("F");
    setB.add("G");
    setB.add("H");

    setC.add("Z");
  }

  /*
   * Test method for
   * 'spice.kernelpool.KernelPoolChangeEvent.KernelPoolChangeEvent(KernelPoolEventType, Set<String>,
   * Set<String>, Set<String>)'
   */
  public void testKernelPoolChangeEvent() {

    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.LOAD, setA, setB, setC);

    Set<String> addedList = new HashSet<String>(event.getAddedKeywords());
    Set<String> modifiedList = new HashSet<String>(event.getModifiedKeywords());
    Set<String> deletedList = new HashSet<String>(event.getDeletedKeywords());

    assertTrue(addedList.containsAll(setC));
    addedList.removeAll(setC);
    assertTrue(addedList.isEmpty());

    assertTrue(modifiedList.containsAll(setB));
    modifiedList.removeAll(setB);
    assertTrue(modifiedList.isEmpty());

    assertTrue(deletedList.containsAll(setA));
    deletedList.removeAll(setA);
    assertTrue(deletedList.isEmpty());

    assertEquals(KernelPoolEventType.LOAD, event.getType());

  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEvent.getAddedKeywords()'
   */
  public void testGetAddedKeywords() {

    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.LOAD, emptySet, setA, setC);

    Set<String> addedList = event.getAddedKeywords();

    assertTrue(addedList.containsAll(setC));

    Set<String> copy = new HashSet<String>(addedList);
    copy.removeAll(setC);

    assertTrue(copy.isEmpty());
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEvent.getDeletedKeywords()'
   */
  public void testGetDeletedKeywords() {

    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.CLEAR, setA, emptySet, emptySet);

    Set<String> deletedList = event.getDeletedKeywords();

    assertTrue(deletedList.containsAll(setA));

    Set<String> copy = new HashSet<String>(deletedList);
    copy.removeAll(setA);
    assertTrue(copy.isEmpty());

  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEvent.getModifiedKeywords()'
   */
  public void testGetModifiedKeywords() {

    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.APPEND, emptySet, setC, setB);

    Set<String> modifiedList = event.getModifiedKeywords();

    assertTrue(modifiedList.containsAll(setC));

    Set<String> copy = new HashSet<String>(modifiedList);
    copy.removeAll(setC);

    assertTrue(copy.isEmpty());

  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEvent.getAllChangedKeywords()'
   */
  public void testGetAllChangedKeywords() {

    KernelPoolChangeEvent event =
        new KernelPoolChangeEvent(KernelPoolEventType.ADD, emptySet, setB, setA);

    Set<String> changeList = event.getAllChangedKeywords();

    /*
     * Verify that changeList contains both setA and setB.
     */
    assertTrue(changeList.containsAll(setA));
    assertTrue(changeList.containsAll(setB));

    /*
     * Now verify that it ONLY contains the contents of setA and setB.
     */
    Set<String> copy = new HashSet<String>(changeList);
    copy.removeAll(setA);
    copy.removeAll(setB);

    assertTrue(copy.isEmpty());
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEvent.getType()'
   */
  public void testGetType() {

    KernelPoolEventType[] types = KernelPoolEventType.values();

    /*
     * Create an event of each type and verify that getType returns the appropriate type.
     */
    for (KernelPoolEventType type : types) {

      KernelPoolChangeEvent event = new KernelPoolChangeEvent(type, emptySet, emptySet, emptySet);

      assertEquals(type, event.getType());
    }

  }

}
