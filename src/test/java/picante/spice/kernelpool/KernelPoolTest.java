package picante.spice.kernelpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.spice.kernelpool.AssertionUtilities.assertPoolEquality;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;

/*
 * Since this test class is testing a direct subclass of BasicKernelPool and we know it just wraps
 * the functionality in BasicKernelPool with a listener management API, we need only test that
 * component of the API.
 */
public class KernelPoolTest {

  private SimpleChangeListener listenerA;

  private SimpleChangeListener listenerB;

  private List<String> stringsA;

  private List<String> stringsB;

  private List<String> stringsC;

  private List<Double> doublesA;

  private List<Double> doublesB;

  private List<Double> doublesC;

  private List<Integer> integersA;
  private List<Integer> integersB;
  private List<Integer> integersC;

  private KernelPool pool;

  private KernelPool loadedPool;

  public class SimpleChangeListener implements KernelPoolChangeListener {

    private List<KernelPoolChangeEvent> events = new LinkedList<KernelPoolChangeEvent>();

    public List<KernelPoolChangeEvent> getEvents() {
      return Collections.unmodifiableList(events);
    }

    @Override
    public void poolChanged(KernelPoolChangeEvent event) {
      events.add(event);
    }
  }

  @Before
  public void setUp() throws Exception {
    stringsA = new ArrayList<String>();
    stringsB = new ArrayList<String>();
    stringsC = new ArrayList<String>();

    doublesA = new ArrayList<Double>();
    doublesB = new ArrayList<Double>();
    doublesC = new ArrayList<Double>();

    integersA = new ArrayList<Integer>();
    integersB = new ArrayList<Integer>();
    integersC = new ArrayList<Integer>();

    stringsA.add("A");
    stringsA.add("B");
    stringsA.add("C");

    stringsB.add("E");
    stringsB.add("F");

    stringsC.add("G");

    doublesA.add(-10.0);
    doublesA.add(-11.0);
    doublesA.add(-12.0);

    integersA.add(-10);
    integersA.add(-11);
    integersA.add(-12);

    doublesB.add(13.0);
    doublesB.add(14.0);

    integersB.add(13);
    integersB.add(14);

    doublesC.add(-15.0);

    integersC.add(15);

    pool = new KernelPool();
    loadedPool = new KernelPool();

    loadedPool.addStrings("PRESTR-A", stringsA);
    loadedPool.addStrings("PRESTR-B", stringsB);
    loadedPool.addStrings("PRESTR-C", stringsC);
    loadedPool.addDoubles("PREDBL-A", doublesA);
    loadedPool.addDoubles("PREDBL-B", doublesB);
    loadedPool.addDoubles("PREDBL-C", doublesC);

    listenerA = new SimpleChangeListener();
    listenerB = new SimpleChangeListener();
  }

  @Test
  public void testAddStrings() {

    /*
     * Register multiple listeners with the pool.
     */
    pool.addListener(listenerA);
    pool.addListener(listenerB);

    /*
     * Add a non-existant string keyword value assignment to the pool.
     */
    pool.addStrings("LOADA", stringsA);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("LOADA"));

    /*
     * Now add to an existant keyword in the pool. This should result in a slightly different
     * structured event.
     */
    pool.addStrings("LOADA", stringsB);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("LOADA"));
  }

  @Test
  public void testAppendStrings() {

    /*
     * First, verify that in the exceptional case of appending string data to double data that the
     * appropriate runtime exception is generated and no listeners are fired.
     */
    SimpleChangeListener listener = new SimpleChangeListener();
    loadedPool.addListener(listener);

    try {
      loadedPool.appendStrings("PREDBL-A", stringsA);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Unable to append " + "String values into key"));
      assertTrue(listener.getEvents().isEmpty());
    }

    /*
     * Register multiple listeners with the pool.
     */
    pool.addListener(listenerA);
    pool.addListener(listenerB);

    /*
     * Add a non-existant string keyword value assignment to the pool.
     */
    pool.appendStrings("LOADA", stringsA);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("LOADA"));

    /*
     * Now add to an existant keyword in the pool. This should result in a slightly different
     * structured event.
     */
    pool.appendStrings("LOADA", stringsB);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("LOADA"));
  }

  @Test
  public void testAddDoubles() {

    /*
     * Register multiple listeners with the pool.
     */
    pool.addListener(listenerA);
    pool.addListener(listenerB);

    /*
     * Add a non-existant string keyword value assignment to the pool.
     */
    pool.addDoubles("LOADA", doublesA);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("LOADA"));

    /*
     * Now add to an existant keyword in the pool. This should result in a slightly different
     * structured event.
     */
    pool.addDoubles("LOADA", doublesB);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("LOADA"));
  }

  @Test
  public void testAppendDoubles() {

    /*
     * First, verify that in the exceptional case of appending double data to string data that the
     * appropriate runtime exception is generated and no listeners are fired.
     */
    SimpleChangeListener listener = new SimpleChangeListener();
    loadedPool.addListener(listener);

    try {
      loadedPool.appendDoubles("PRESTR-A", doublesC);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Unable to append " + "Double values into key"));
      assertTrue(listener.getEvents().isEmpty());
    }

    /*
     * Register multiple listeners with the pool.
     */
    pool.addListener(listenerA);
    pool.addListener(listenerB);

    /*
     * Add a non-existant string keyword value assignment to the pool.
     */
    pool.appendDoubles("LOADA", doublesA);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("LOADA"));

    /*
     * Now add to an existant keyword in the pool. This should result in a slightly different
     * structured event.
     */
    pool.appendDoubles("LOADA", doublesB);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("LOADA"));
  }

  @Test
  public void testAddIntegers() {
    /*
     * Register multiple listeners with the pool.
     */
    pool.addListener(listenerA);
    pool.addListener(listenerB);

    /*
     * Add a non-existant string keyword value assignment to the pool.
     */
    pool.addIntegers("LOADA", integersA);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("LOADA"));

    /*
     * Now add to an existant keyword in the pool. This should result in a slightly different
     * structured event.
     */
    pool.addIntegers("LOADA", integersB);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.ADD, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("LOADA"));

  }

  @Test
  public void testAppendIntegersToStringException() {
    /*
     * First, verify that in the exceptional case of appending double data to string data that the
     * appropriate runtime exception is generated and no listeners are fired.
     */
    SimpleChangeListener listener = new SimpleChangeListener();
    loadedPool.addListener(listener);

    try {
      loadedPool.appendDoubles("PRESTR-A", doublesC);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(uoe.getMessage().contains("Unable to append " + "Double values into key"));
      assertTrue(listener.getEvents().isEmpty());
    }
  }

  @Test
  public void testAppendIntegers() {

    /*
     * Register multiple listeners with the pool.
     */
    pool.addListener(listenerA);
    pool.addListener(listenerB);

    /*
     * Add a non-existant string keyword value assignment to the pool.
     */
    pool.appendIntegers("LOADA", integersA);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertEquals(1, event.getAddedKeywords().size());
    assertTrue(event.getAddedKeywords().contains("LOADA"));

    /*
     * Now add to an existant keyword in the pool. This should result in a slightly different
     * structured event.
     */
    pool.appendIntegers("LOADA", integersB);

    /*
     * Check to see that the event listeners received the correct change event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.APPEND, event.getType());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getModifiedKeywords().size());
    assertTrue(event.getModifiedKeywords().contains("LOADA"));

  }

  @Test
  public void testLoad() {

    /*
     * Register listenerA with pool and listenerB with loadedPool.
     */
    pool.addListener(listenerA);
    loadedPool.addListener(listenerB);

    /*
     * First attempt to process the exceptional case. Generate a pool that contains appended data
     * which conflicts with the type of data in loadedPool and load it. A runtime exception should
     * be generated and no events should be fired.
     */
    KernelPool conflict = new KernelPool();
    conflict.appendDoubles("PRESTR-A", doublesA);
    conflict.appendStrings("PREDBL-C", stringsA);

    try {
      loadedPool.load(conflict);
      fail("Expected UnsupportedOperationException was not thrown.");
    } catch (UnsupportedOperationException uoe) {
      assertTrue(listenerA.getEvents().isEmpty());
    }

    /*
     * Now load the empty pool into loadedPool.
     */
    loadedPool.load(pool);

    /*
     * Check the event generated for listenerB.
     */
    assertEquals(1, listenerB.getEvents().size());
    assertEquals(0, listenerA.getEvents().size());

    KernelPoolChangeEvent event = listenerB.getEvents().get(0);
    assertEquals(KernelPoolEventType.LOAD, event.getType());

    assertTrue(event.getAddedKeywords().isEmpty());
    assertTrue(event.getDeletedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());

    /*
     * Add some content to pool that conflicts with keywords in loadedPool.
     */
    pool.addStrings("PREDBL-A", stringsA);
    pool.addDoubles("MYDBL-C", doublesC);
    pool.addStrings("MYSTR-A", stringsA);

    loadedPool.load(pool);

    /*
     * Check the event that was generated.
     */
    assertEquals(3, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerB.getEvents().get(1);
    assertEquals(KernelPoolEventType.LOAD, event.getType());

    assertTrue(event.getDeletedKeywords().isEmpty());

    List<String> modified = new ArrayList<String>();
    List<String> added = new ArrayList<String>();

    modified.add("PREDBL-A");
    added.add("MYDBL-C");
    added.add("MYSTR-A");

    assertEquals(1, event.getModifiedKeywords().size());
    assertEquals(2, event.getAddedKeywords().size());

    assertTrue(event.getAddedKeywords().containsAll(added));
    assertTrue(event.getModifiedKeywords().containsAll(modified));

  }

  @Test
  public void testRemoveKeywords() {

    /*
     * Add multiple listeners to the loadedPool.
     */
    loadedPool.addListener(listenerA);
    loadedPool.addListener(listenerB);

    /*
     * Remove the following subset of keywords from the pool.
     */
    List<String> keywords = new ArrayList<String>();
    keywords.add("PRESTR-A");
    keywords.add("PREDBL-B");
    keywords.add("PREDBL-C");

    loadedPool.removeKeywords(keywords);

    /*
     * Verify both the event listeners received the event, and that they are the same.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.REMOVE, event.getType());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(keywords.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(keywords));

    /*
     * Verify that if the removal request results in no keyword removals that no event is fired.
     */
    keywords.clear();
    keywords.add("NOT-PRESENT");
    keywords.add("NOT-PRESENT2");

    loadedPool.removeKeywords(keywords);
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());
  }

  @Test
  public void testRemoveKeyword() {

    /*
     * Add multiple listeners to the loadedPool.
     */
    loadedPool.addListener(listenerA);
    loadedPool.addListener(listenerB);

    /*
     * Remove the following keyword from the pool.
     */
    loadedPool.removeKeyword("PRESTR-A");

    /*
     * Verify both the event listeners received the event, and that they are the same.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.REMOVE, event.getType());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(1, event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().contains("PRESTR-A"));

    /*
     * Now check to see that no event is fired if the keyword is absent.
     */
    loadedPool.removeKeyword("NOT-PRESENT");

    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());
  }

  @Test
  public void testClear() {

    /*
     * Add multiple listeners to the loadedPool.
     */
    loadedPool.addListener(listenerA);
    loadedPool.addListener(listenerB);

    /*
     * Capture the keywords loaded into pool.
     */
    Set<String> keywords = loadedPool.getKeywords();

    /*
     * Clear loaded pool.
     */
    loadedPool.clear();

    /*
     * Verify both the event listeners received the event, and that they are the same.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    KernelPoolChangeEvent event = listenerA.getEvents().get(0);
    assertSame(event, listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.CLEAR, event.getType());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertEquals(keywords.size(), event.getDeletedKeywords().size());
    assertTrue(event.getDeletedKeywords().containsAll(keywords));

    /*
     * What happens if we clear an empty pool?
     */
    loadedPool.clear();

    /*
     * Verify that both event listeners received the clear event.
     */
    assertEquals(2, listenerA.getEvents().size());
    assertEquals(2, listenerB.getEvents().size());

    event = listenerA.getEvents().get(1);
    assertSame(event, listenerB.getEvents().get(1));

    assertEquals(KernelPoolEventType.CLEAR, event.getType());
    assertTrue(event.getAddedKeywords().isEmpty());
    assertTrue(event.getModifiedKeywords().isEmpty());
    assertTrue(event.getDeletedKeywords().isEmpty());
  }

  @Test
  public void testKernelPool() {

    /*
     * This constructor just builds a pool that is the default. Make certain it has no assigned data
     * content.
     */
    KernelPool myPool = new KernelPool();

    assertTrue(myPool.getKeywords().isEmpty());

  }

  @Test
  public void testKernelPoolBasicKernelPool() {

    /*
     * Add some listeners to loadedPool, just to make certain the constructor isn't doing something
     * nasty like copying listeners by downcast.
     */
    loadedPool.addListener(listenerA);

    /*
     * This constructor copies a pool. Instantiate a new pool, using loadedPool as the pool to copy.
     */
    KernelPool myPool = new KernelPool(loadedPool);

    /*
     * Verify the pools are the same.
     */
    assertPoolEquality(loadedPool, myPool);

    /*
     * Check that myPool does not have any of loadedPool's listeners registered.
     */
    myPool.clear();
    assertEquals(0, listenerA.getEvents().size());

  }

  @Test
  public void testAddListener() {

    /*
     * Add a listener to pool.
     */
    pool.addListener(listenerA);

    /*
     * Generate an event.
     */
    pool.addStrings("MYSTRINGS", stringsC);

    /*
     * Check that an addition event was generated.
     */
    assertEquals(1, listenerA.getEvents().size());
    assertEquals(KernelPoolEventType.ADD, listenerA.getEvents().get(0).getType());

    /*
     * Now add a new listener to pool.
     */
    pool.addListener(listenerB);

    /*
     * Generate an event.
     */
    pool.appendDoubles("MYDOUBLES", doublesA);

    assertEquals(1, listenerB.getEvents().size());
    assertEquals(2, listenerA.getEvents().size());

    assertSame(listenerA.getEvents().get(1), listenerB.getEvents().get(0));
    assertEquals(KernelPoolEventType.APPEND, listenerB.getEvents().get(0).getType());
  }

  @Test
  public void testRemoveListener() {

    /*
     * Add a listener to the pool.
     */
    pool.addListener(listenerA);

    /*
     * Fire an event.
     */
    pool.addStrings("MYSTRINGS", stringsA);

    /*
     * Add another listener to the pool.
     */
    pool.addListener(listenerB);

    /*
     * Fire another event.
     */
    pool.addDoubles("MYDOUBLES", doublesC);

    /*
     * Now, remove listenerB.
     */
    pool.removeListener(listenerB);

    /*
     * Verify that listenerB is no longer in the list of listeners by firing an event.
     */
    pool.appendDoubles("MYDOUBLES", doublesB);

    assertEquals(3, listenerA.getEvents().size());
    assertEquals(1, listenerB.getEvents().size());

    /*
     * The last event that listenerA received should not be the same as the last event listenerB
     * received.
     */
    assertNotSame(listenerA.getEvents().get(2), listenerB.getEvents().get(0));

    assertEquals(KernelPoolEventType.ADD, listenerB.getEvents().get(0).getType());

    /*
     * Add listenerA to the list again, then remove it. Make certain that an instance of listenerA
     * is still in the list.
     */
    pool.addListener(listenerA);

    /*
     * Fire an event that should be registered twice, thanks to double registration of listenerA.
     */
    pool.clear();

    assertEquals(5, listenerA.getEvents().size());
    assertSame(listenerA.getEvents().get(4), listenerA.getEvents().get(3));
    assertEquals(KernelPoolEventType.CLEAR, listenerA.getEvents().get(4).getType());

    pool.removeListener(listenerA);

    /*
     * Fire another event, and verify that listenerA receives a single copy of it.
     */
    pool.addDoubles("MOREDOUBLES", doublesA);

    assertEquals(6, listenerA.getEvents().size());
    assertEquals(KernelPoolEventType.ADD, listenerA.getEvents().get(5).getType());

    /*
     * Remove the final instance of listenerA.
     */
    pool.removeListener(listenerA);

    /*
     * Fire one more event, and check that listenerA does not receive it.
     */
    pool.clear();

    assertEquals(6, listenerA.getEvents().size());
  }

}
