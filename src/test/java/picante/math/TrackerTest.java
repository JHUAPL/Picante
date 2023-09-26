package picante.math;

import static org.junit.Assert.assertEquals;
import static picante.math.Statistics.Tracker.ALL;
import static picante.math.Statistics.Tracker.FIRST;
import static picante.math.Statistics.Tracker.LAST;
import static picante.math.Statistics.Tracker.NONE;
import java.util.LinkedList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

public class TrackerTest {

  private LinkedList<Integer> emptyIndices;
  private LinkedList<Integer> singleEntryIndices;
  private LinkedList<Integer> populatedIndices;
  private ImmutableList<Integer> copyOfSingleEntryIndices;
  private ImmutableList<Integer> copyOfPopulatedIndices;

  private static final int TEST_INDEX = 20;
  private static final ImmutableList<Integer> TEST_INDEX_LIST = ImmutableList.of(TEST_INDEX);

  @Before
  public void setUp() throws Exception {
    emptyIndices = Lists.newLinkedList();
    singleEntryIndices = Lists.newLinkedList(ImmutableList.of(1));
    copyOfSingleEntryIndices = ImmutableList.copyOf(singleEntryIndices);
    populatedIndices = Lists.newLinkedList(ImmutableList.of(1, 2, 3));
    copyOfPopulatedIndices = ImmutableList.copyOf(populatedIndices);
  }

  /*
   * NONE should only ever be presented with an empty list.
   */
  @Test
  public void testNoneEqualAdd() {
    NONE.record(emptyIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * Nothing should happen, no tracking is occurring.
     */
    assertEquals(Lists.newLinkedList(), emptyIndices);
  }

  @Test
  public void testNoneNewAdd() {
    NONE.record(emptyIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * Nothing should happen, no tracking is occurring.
     */
    assertEquals(Lists.newLinkedList(), emptyIndices);
  }

  /*
   * First may be presented with an empty list or one with a single entry.
   */
  @Test
  public void testFirstEmptyEqualAdd() {
    FIRST.record(emptyIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As the list is empty, this should record the index supplied.
     */
    assertEquals(TEST_INDEX_LIST, emptyIndices);
  }

  @Test
  public void testFirstEmptyNewAdd() {
    FIRST.record(emptyIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * As the list is empty, this should record the index supplied.
     */
    assertEquals(TEST_INDEX_LIST, emptyIndices);
  }

  @Test
  public void testFirstSingleEqualAdd() {
    FIRST.record(singleEntryIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As this method captures the first occurrence, nothing should change.
     */
    assertEquals(copyOfSingleEntryIndices, singleEntryIndices);
  }

  @Test
  public void testFirstSingleNewAdd() {
    FIRST.record(singleEntryIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * The value was updated, so it will be captured.
     */
    assertEquals(TEST_INDEX_LIST, singleEntryIndices);
  }

  /*
   * Last may be presented with an empty list or one with a single entry.
   */
  @Test
  public void testLastEmptyEqualAdd() {
    LAST.record(emptyIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As the list is empty, this should record the index supplied.
     */
    assertEquals(TEST_INDEX_LIST, emptyIndices);
  }

  @Test
  public void testLastEmptyNewAdd() {
    LAST.record(emptyIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * As the list is empty, this should record the index supplied.
     */
    assertEquals(TEST_INDEX_LIST, emptyIndices);
  }

  @Test
  public void testLastSingleEqualAdd() {
    LAST.record(singleEntryIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As this method captures the last occurrence, the value should be updated.
     */
    assertEquals(TEST_INDEX_LIST, singleEntryIndices);
  }

  @Test
  public void testLastSingleNewAdd() {
    LAST.record(singleEntryIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * The value was updated, so it will be captured.
     */
    assertEquals(TEST_INDEX_LIST, singleEntryIndices);
  }

  /*
   * ALL may be presented with an empty list, a single entry, or a fully populated one.
   */
  @Test
  public void testAllEmptyEqualAdd() {
    ALL.record(emptyIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As the list is empty, this should record the index supplied.
     */
    assertEquals(TEST_INDEX_LIST, emptyIndices);
  }

  @Test
  public void testAllEmptyNewAdd() {
    ALL.record(emptyIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * As the list is empty, this should record the index supplied.
     */
    assertEquals(TEST_INDEX_LIST, emptyIndices);
  }

  @Test
  public void testAllSingleEqualAdd() {
    ALL.record(singleEntryIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As this method captures every index, the value should be added to the list.
     */
    List<Integer> expected = Lists.newLinkedList();
    expected.addAll(copyOfSingleEntryIndices);
    expected.add(TEST_INDEX);
    assertEquals(expected, singleEntryIndices);
  }

  @Test
  public void testAllSingleNewAdd() {
    ALL.record(singleEntryIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * The value was updated, so it should dump the previous values and capture the new one.
     */
    assertEquals(TEST_INDEX_LIST, singleEntryIndices);
  }

  @Test
  public void testAllMultipleEqualAdd() {
    ALL.record(populatedIndices, 10.0, 10.0, TEST_INDEX);
    /*
     * As this method captures all the values, the new value should be appended to the list.
     */
    List<Integer> expected = Lists.newLinkedList();
    expected.addAll(copyOfPopulatedIndices);
    expected.add(TEST_INDEX);
    assertEquals(expected, populatedIndices);
  }

  @Test
  public void testAllMultipleNewAdd() {
    ALL.record(populatedIndices, 11.0, 10.0, TEST_INDEX);
    /*
     * The value was updated, so it will wipe out the predecessors and be captured.
     */
    assertEquals(TEST_INDEX_LIST, populatedIndices);
  }

}
