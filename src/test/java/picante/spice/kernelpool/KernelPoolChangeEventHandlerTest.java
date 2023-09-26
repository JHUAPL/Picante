package picante.spice.kernelpool;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import junit.framework.TestCase;

public class KernelPoolChangeEventHandlerTest extends TestCase {

  private KernelPoolChangeEventHandler handler;

  private Set<String> setA;
  private Set<String> setB;
  private Set<String> setC;
  private SimpleChangeListener listenerA;
  private SimpleChangeListener listenerB;
  private SimpleChangeListener listenerC;

  /**
   * Simple event capturing class.
   */
  public class SimpleChangeListener implements KernelPoolChangeListener {

    private List<KernelPoolChangeEvent> events = new LinkedList<KernelPoolChangeEvent>();

    @Override
    public void poolChanged(KernelPoolChangeEvent event) {
      events.add(event);
    }

    public List<KernelPoolChangeEvent> getEventList() {
      return Collections.unmodifiableList(events);
    }
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();

    setA = new HashSet<String>();
    setB = new HashSet<String>();
    setC = new HashSet<String>();

    setA.add("A");
    setA.add("B");
    setA.add("C");

    setB.add("E");
    setB.add("F");

    setC.add("Z");

    listenerA = new SimpleChangeListener();
    listenerB = new SimpleChangeListener();
    listenerC = new SimpleChangeListener();

    handler = new KernelPoolChangeEventHandler();
  }

  /*
   * Test method for
   * 'spice.kernelpool.KernelPoolChangeEventHandler.addChangeListener(KernelPoolChangeListener)'
   */
  public void testAddChangeListener() {

    handler.addChangeListener(listenerA);
    handler.fireAdditionEvent("A", false);

    /*
     * Check to see that the appropriate event was fired.
     */
    assertEquals(1, listenerA.getEventList().size());

    KernelPoolChangeEvent event = listenerA.getEventList().get(0);
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().contains("A"));
    assertEquals(1, event.getAddedKeywords().size());

    /*
     * Add two new listeners, and verify that they receive fired events as well.
     */
    handler.addChangeListener(listenerB);
    handler.addChangeListener(listenerC);

    handler.fireClearingEvent(setA);

    /*
     * Check to see that the appropriate event was fired.
     */
    assertEquals(2, listenerA.getEventList().size());
    event = listenerA.getEventList().get(1);
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(setA.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(setA));

    assertEquals(1, listenerB.getEventList().size());
    event = listenerB.getEventList().get(0);
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(setA.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(setA));

    assertEquals(1, listenerC.getEventList().size());
    event = listenerC.getEventList().get(0);
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(setA.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(setA));

  }

  /*
   * Test method for
   * 'spice.kernelpool.KernelPoolChangeEventHandler.removeChangeListener(KernelPoolChangeListener)'
   */
  public void testRemoveChangeListener() {

    /*
     * Queue up an event in listenerA.
     */
    handler.addChangeListener(listenerA);
    handler.fireAdditionEvent("A", false);

    /*
     * Register two additional listeners; fire an event.
     */
    handler.addChangeListener(listenerB);
    handler.addChangeListener(listenerC);
    handler.fireAdditionEvent("B", false);

    /*
     * Now remove listenerB and fire another event.
     */
    handler.removeChangeListener(listenerB);
    handler.fireAdditionEvent("C", false);

    /*
     * Now remove listenerA and listenerC; fire another event.
     */
    handler.removeChangeListener(listenerA);
    handler.removeChangeListener(listenerC);
    handler.fireAdditionEvent("D", false);

    /*
     * Verify the sequence of events each listener received.
     */
    assertEquals(3, listenerA.getEventList().size());
    assertTrue(listenerA.getEventList().get(0).getAddedKeywords().contains("A"));
    assertTrue(listenerA.getEventList().get(1).getAddedKeywords().contains("B"));
    assertTrue(listenerA.getEventList().get(2).getAddedKeywords().contains("C"));

    assertEquals(1, listenerB.getEventList().size());
    assertTrue(listenerB.getEventList().get(0).getAddedKeywords().contains("B"));

    assertEquals(2, listenerC.getEventList().size());
    assertTrue(listenerC.getEventList().get(0).getAddedKeywords().contains("B"));
    assertTrue(listenerC.getEventList().get(1).getAddedKeywords().contains("C"));
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEventHandler.fireClearingEvent(Set<String>)'
   */
  public void testFireClearingEvent() {

    handler.addChangeListener(listenerA);
    handler.addChangeListener(listenerB);

    /*
     * Fire a clearing event.
     */
    handler.fireClearingEvent(setC);

    /*
     * Verify that the clearing event is properly structured.
     */
    assertEquals(1, listenerA.getEventList().size());
    assertEquals(1, listenerB.getEventList().size());

    KernelPoolChangeEvent event = listenerA.getEventList().get(0);
    assertSame(event, listenerB.getEventList().get(0));

    assertEquals(KernelPoolEventType.CLEAR, event.getType());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(setC.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(setC));
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEventHandler.fireRemovalEvent(Set<String>)'
   */
  public void testFireRemovalEvent() {

    handler.addChangeListener(listenerA);
    handler.addChangeListener(listenerB);

    /*
     * Fire a removal event.
     */
    handler.fireRemovalEvent(setB);

    /*
     * Verify that the removal event is properly structured.
     */
    assertEquals(1, listenerA.getEventList().size());
    assertEquals(1, listenerB.getEventList().size());

    KernelPoolChangeEvent event = listenerA.getEventList().get(0);
    assertSame(event, listenerB.getEventList().get(0));

    assertEquals(KernelPoolEventType.REMOVE, event.getType());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(setB.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(setB));
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEventHandler.fireAdditionEvent(String,
   * String)'
   */
  public void testFireAdditionEvent() {

    handler.addChangeListener(listenerA);
    handler.addChangeListener(listenerB);

    /*
     * Fire an addition event.
     */
    handler.fireAdditionEvent("A", true);

    KernelPoolChangeEvent event = listenerA.getEventList().get(0);
    assertSame(event, listenerB.getEventList().get(0));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("A"));

    /*
     * And now an event that adds, rather than modifies.
     */
    handler.fireAdditionEvent("B", false);

    event = listenerA.getEventList().get(1);
    assertSame(event, listenerB.getEventList().get(1));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("B"));
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEventHandler.fireAppendingEvent(String,
   * String)'
   */
  public void testFireAppendingEvent() {

    handler.addChangeListener(listenerA);
    handler.addChangeListener(listenerB);

    /*
     * Fire an addition event.
     */
    handler.fireAppendingEvent("A", true);

    KernelPoolChangeEvent event = listenerA.getEventList().get(0);
    assertSame(event, listenerB.getEventList().get(0));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("A"));

    /*
     * And now an event that adds, rather than modifies.
     */
    handler.fireAppendingEvent("B", false);

    event = listenerA.getEventList().get(1);
    assertSame(event, listenerB.getEventList().get(1));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("B"));
  }

  /*
   * Test method for 'spice.kernelpool.KernelPoolChangeEventHandler.fireLoadingEvent(Set<String>,
   * Set<String>)'
   */
  public void testFireLoadingEvent() {

    handler.addChangeListener(listenerA);
    handler.addChangeListener(listenerB);

    /*
     * Fire a loading event.
     */
    handler.fireLoadingEvent(setA, setB);

    KernelPoolChangeEvent event = listenerA.getEventList().get(0);
    assertSame(event, listenerB.getEventList().get(0));

    assertEquals(KernelPoolEventType.LOAD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertEquals(setA.size(), event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().containsAll(setA));
    assertEquals(setB.size(), event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().containsAll(setB));
  }

}
