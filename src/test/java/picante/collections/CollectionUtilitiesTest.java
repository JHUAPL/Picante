package picante.collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;

public class CollectionUtilitiesTest {

  private List<Integer> sortedList;
  private List<Integer> rSortedList;
  private List<Integer> singleton;
  private List<Integer> empty;
  private List<Integer> repeatedList;
  private List<Integer> rRepeatedList;

  private static final Comparator<Integer> REVERSE_COMPARATOR = new Comparator<Integer>() {

    @Override
    public int compare(Integer o1, Integer o2) {
      return o2.compareTo(o1);
    }

  };

  @Before
  public void setUp() throws Exception {

    empty = new ArrayList<Integer>();

    singleton = new ArrayList<Integer>();

    singleton.add(0);

    sortedList = new ArrayList<Integer>();
    sortedList.add(0);
    sortedList.add(10);
    sortedList.add(20);
    sortedList.add(30);

    Collections.sort(sortedList);

    rSortedList = new ArrayList<Integer>();

    rSortedList.addAll(sortedList);
    Collections.sort(rSortedList, REVERSE_COMPARATOR);

    repeatedList = new ArrayList<Integer>();

    repeatedList.add(0);
    repeatedList.add(10);
    repeatedList.add(10);
    repeatedList.add(10);
    repeatedList.add(10);
    repeatedList.add(10);
    repeatedList.add(20);

    Collections.sort(repeatedList);

    rRepeatedList = new ArrayList<Integer>();

    rRepeatedList.addAll(repeatedList);
    Collections.sort(rRepeatedList, REVERSE_COMPARATOR);
  }

  @Test
  public void testLastLessThanOrEqualToListOfQextendsComparableOfQsuperTT() {

    assertEquals(-1, CollectionUtilities.lastLessThanOrEqualTo(empty, 0));

    assertEquals(-1, CollectionUtilities.lastLessThanOrEqualTo(singleton, -10));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(singleton, 0));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(singleton, 10));

    assertEquals(-1, CollectionUtilities.lastLessThanOrEqualTo(sortedList, -10));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 0));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 5));
    assertEquals(1, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 10));
    assertEquals(1, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 15));
    assertEquals(2, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 20));
    assertEquals(2, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 25));
    assertEquals(3, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 30));
    assertEquals(3, CollectionUtilities.lastLessThanOrEqualTo(sortedList, 35));

    assertEquals(-1, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, -10));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, 0));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, 5));
    assertEquals(5, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, 10));
    assertEquals(5, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, 15));
    assertEquals(6, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, 20));
    assertEquals(6, CollectionUtilities.lastLessThanOrEqualTo(repeatedList, 25));

  }

  @Test
  public void testLastLessThanOrEqualToListOfQextendsTTComparatorOfQsuperT() {

    assertEquals(-1, CollectionUtilities.lastLessThanOrEqualTo(empty, 0, REVERSE_COMPARATOR));

    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(singleton, -10, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(singleton, 0, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThanOrEqualTo(singleton, 10, REVERSE_COMPARATOR));

    assertEquals(3,
        CollectionUtilities.lastLessThanOrEqualTo(rSortedList, -10, REVERSE_COMPARATOR));
    assertEquals(3, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 0, REVERSE_COMPARATOR));
    assertEquals(2, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 5, REVERSE_COMPARATOR));
    assertEquals(2, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 10, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 15, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 20, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 25, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 30, REVERSE_COMPARATOR));
    assertEquals(-1,
        CollectionUtilities.lastLessThanOrEqualTo(rSortedList, 35, REVERSE_COMPARATOR));

    assertEquals(6,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, -10, REVERSE_COMPARATOR));
    assertEquals(6,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, 0, REVERSE_COMPARATOR));
    assertEquals(5,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, 5, REVERSE_COMPARATOR));
    assertEquals(5,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, 10, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, 15, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, 20, REVERSE_COMPARATOR));
    assertEquals(-1,
        CollectionUtilities.lastLessThanOrEqualTo(rRepeatedList, 25, REVERSE_COMPARATOR));
  }

  @Test
  public void testLastLessThanListOfQextendsComparableOfQsuperTT() {

    assertEquals(-1, CollectionUtilities.lastLessThan(empty, 0));

    assertEquals(-1, CollectionUtilities.lastLessThan(singleton, -10));
    assertEquals(-1, CollectionUtilities.lastLessThan(singleton, 0));
    assertEquals(0, CollectionUtilities.lastLessThan(singleton, 10));

    assertEquals(-1, CollectionUtilities.lastLessThan(sortedList, -10));
    assertEquals(-1, CollectionUtilities.lastLessThan(sortedList, 0));
    assertEquals(0, CollectionUtilities.lastLessThan(sortedList, 5));
    assertEquals(0, CollectionUtilities.lastLessThan(sortedList, 10));
    assertEquals(1, CollectionUtilities.lastLessThan(sortedList, 15));
    assertEquals(1, CollectionUtilities.lastLessThan(sortedList, 20));
    assertEquals(2, CollectionUtilities.lastLessThan(sortedList, 25));
    assertEquals(2, CollectionUtilities.lastLessThan(sortedList, 30));
    assertEquals(3, CollectionUtilities.lastLessThan(sortedList, 35));

    assertEquals(-1, CollectionUtilities.lastLessThan(repeatedList, -10));
    assertEquals(-1, CollectionUtilities.lastLessThan(repeatedList, 0));
    assertEquals(0, CollectionUtilities.lastLessThan(repeatedList, 5));
    assertEquals(0, CollectionUtilities.lastLessThan(repeatedList, 10));
    assertEquals(5, CollectionUtilities.lastLessThan(repeatedList, 15));
    assertEquals(5, CollectionUtilities.lastLessThan(repeatedList, 20));
    assertEquals(6, CollectionUtilities.lastLessThan(repeatedList, 25));

  }

  @Test
  public void testLastLessThanListOfQextendsTTComparatorOfQsuperT() {

    assertEquals(-1, CollectionUtilities.lastLessThan(empty, 0, REVERSE_COMPARATOR));

    assertEquals(0, CollectionUtilities.lastLessThan(singleton, -10, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThan(singleton, 0, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThan(singleton, 10, REVERSE_COMPARATOR));

    assertEquals(3, CollectionUtilities.lastLessThan(rSortedList, -10, REVERSE_COMPARATOR));
    assertEquals(2, CollectionUtilities.lastLessThan(rSortedList, 0, REVERSE_COMPARATOR));
    assertEquals(2, CollectionUtilities.lastLessThan(rSortedList, 5, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.lastLessThan(rSortedList, 10, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.lastLessThan(rSortedList, 15, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThan(rSortedList, 20, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThan(rSortedList, 25, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThan(rSortedList, 30, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThan(rSortedList, 35, REVERSE_COMPARATOR));

    assertEquals(6, CollectionUtilities.lastLessThan(rRepeatedList, -10, REVERSE_COMPARATOR));
    assertEquals(5, CollectionUtilities.lastLessThan(rRepeatedList, 0, REVERSE_COMPARATOR));
    assertEquals(5, CollectionUtilities.lastLessThan(rRepeatedList, 5, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThan(rRepeatedList, 10, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.lastLessThan(rRepeatedList, 15, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThan(rRepeatedList, 20, REVERSE_COMPARATOR));
    assertEquals(-1, CollectionUtilities.lastLessThan(rRepeatedList, 25, REVERSE_COMPARATOR));
  }

  @Test
  public void testFirstGreaterThanOrEqualToListOfQextendsComparableOfQsuperTT() {

    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(empty, 0));

    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(singleton, -10));
    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(singleton, 0));
    assertEquals(1, CollectionUtilities.firstGreaterThanOrEqualTo(singleton, 10));

    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, -10));
    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 0));
    assertEquals(1, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 5));
    assertEquals(1, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 10));
    assertEquals(2, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 15));
    assertEquals(2, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 20));
    assertEquals(3, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 25));
    assertEquals(3, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 30));
    assertEquals(4, CollectionUtilities.firstGreaterThanOrEqualTo(sortedList, 35));

    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, -10));
    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, 0));
    assertEquals(1, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, 5));
    assertEquals(1, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, 10));
    assertEquals(6, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, 15));
    assertEquals(6, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, 20));
    assertEquals(7, CollectionUtilities.firstGreaterThanOrEqualTo(repeatedList, 25));

  }

  @Test
  public void testFirstGreaterThanOrEqualToListOfQextendsTTComparatorOfQsuperT() {

    assertEquals(0, CollectionUtilities.firstGreaterThanOrEqualTo(empty, 0, REVERSE_COMPARATOR));

    assertEquals(1,
        CollectionUtilities.firstGreaterThanOrEqualTo(singleton, -10, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.firstGreaterThanOrEqualTo(singleton, 0, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.firstGreaterThanOrEqualTo(singleton, 10, REVERSE_COMPARATOR));

    assertEquals(4,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, -10, REVERSE_COMPARATOR));
    assertEquals(3,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 0, REVERSE_COMPARATOR));
    assertEquals(3,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 5, REVERSE_COMPARATOR));
    assertEquals(2,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 10, REVERSE_COMPARATOR));
    assertEquals(2,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 15, REVERSE_COMPARATOR));
    assertEquals(1,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 20, REVERSE_COMPARATOR));
    assertEquals(1,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 25, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 30, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.firstGreaterThanOrEqualTo(rSortedList, 35, REVERSE_COMPARATOR));

    assertEquals(7,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, -10, REVERSE_COMPARATOR));
    assertEquals(6,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, 0, REVERSE_COMPARATOR));
    assertEquals(6,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, 5, REVERSE_COMPARATOR));
    assertEquals(1,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, 10, REVERSE_COMPARATOR));
    assertEquals(1,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, 15, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, 20, REVERSE_COMPARATOR));
    assertEquals(0,
        CollectionUtilities.firstGreaterThanOrEqualTo(rRepeatedList, 25, REVERSE_COMPARATOR));

  }

  @Test
  public void testFirstGreaterThanListOfQextendsComparableOfQsuperTT() {

    assertEquals(0, CollectionUtilities.firstGreaterThan(empty, 0));

    assertEquals(0, CollectionUtilities.firstGreaterThan(singleton, -10));
    assertEquals(1, CollectionUtilities.firstGreaterThan(singleton, 0));
    assertEquals(1, CollectionUtilities.firstGreaterThan(singleton, 10));

    assertEquals(0, CollectionUtilities.firstGreaterThan(sortedList, -10));
    assertEquals(1, CollectionUtilities.firstGreaterThan(sortedList, 0));
    assertEquals(1, CollectionUtilities.firstGreaterThan(sortedList, 5));
    assertEquals(2, CollectionUtilities.firstGreaterThan(sortedList, 10));
    assertEquals(2, CollectionUtilities.firstGreaterThan(sortedList, 15));
    assertEquals(3, CollectionUtilities.firstGreaterThan(sortedList, 20));
    assertEquals(3, CollectionUtilities.firstGreaterThan(sortedList, 25));
    assertEquals(4, CollectionUtilities.firstGreaterThan(sortedList, 30));
    assertEquals(4, CollectionUtilities.firstGreaterThan(sortedList, 35));

    assertEquals(0, CollectionUtilities.firstGreaterThan(repeatedList, -10));
    assertEquals(1, CollectionUtilities.firstGreaterThan(repeatedList, 0));
    assertEquals(1, CollectionUtilities.firstGreaterThan(repeatedList, 5));
    assertEquals(6, CollectionUtilities.firstGreaterThan(repeatedList, 10));
    assertEquals(6, CollectionUtilities.firstGreaterThan(repeatedList, 15));
    assertEquals(7, CollectionUtilities.firstGreaterThan(repeatedList, 20));
    assertEquals(7, CollectionUtilities.firstGreaterThan(repeatedList, 25));

  }

  @Test
  public void testFirstGreaterThanListOfQextendsTTComparatorOfQsuperT() {

    assertEquals(0, CollectionUtilities.firstGreaterThan(empty, 0, REVERSE_COMPARATOR));

    assertEquals(1, CollectionUtilities.firstGreaterThan(singleton, -10, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.firstGreaterThan(singleton, 0, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.firstGreaterThan(singleton, 10, REVERSE_COMPARATOR));

    assertEquals(4, CollectionUtilities.firstGreaterThan(rSortedList, -10, REVERSE_COMPARATOR));
    assertEquals(4, CollectionUtilities.firstGreaterThan(rSortedList, 0, REVERSE_COMPARATOR));
    assertEquals(3, CollectionUtilities.firstGreaterThan(rSortedList, 5, REVERSE_COMPARATOR));
    assertEquals(3, CollectionUtilities.firstGreaterThan(rSortedList, 10, REVERSE_COMPARATOR));
    assertEquals(2, CollectionUtilities.firstGreaterThan(rSortedList, 15, REVERSE_COMPARATOR));
    assertEquals(2, CollectionUtilities.firstGreaterThan(rSortedList, 20, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.firstGreaterThan(rSortedList, 25, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.firstGreaterThan(rSortedList, 30, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.firstGreaterThan(rSortedList, 35, REVERSE_COMPARATOR));

    assertEquals(7, CollectionUtilities.firstGreaterThan(rRepeatedList, -10, REVERSE_COMPARATOR));
    assertEquals(7, CollectionUtilities.firstGreaterThan(rRepeatedList, 0, REVERSE_COMPARATOR));
    assertEquals(6, CollectionUtilities.firstGreaterThan(rRepeatedList, 5, REVERSE_COMPARATOR));
    assertEquals(6, CollectionUtilities.firstGreaterThan(rRepeatedList, 10, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.firstGreaterThan(rRepeatedList, 15, REVERSE_COMPARATOR));
    assertEquals(1, CollectionUtilities.firstGreaterThan(rRepeatedList, 20, REVERSE_COMPARATOR));
    assertEquals(0, CollectionUtilities.firstGreaterThan(rRepeatedList, 25, REVERSE_COMPARATOR));

  }

  @Test
  public void testAddAllIterable() {
    List<Integer> iterable = Arrays.asList(1, 2, 3, 1, 4, 5, 6);

    List<Integer> list = new ArrayList<Integer>();
    CollectionUtilities.addAll(iterable, list);
    assertEquals(iterable, list);

    Set<Integer> set = new HashSet<Integer>();
    CollectionUtilities.addAll(iterable, set);
    Set<Integer> expected = new HashSet<Integer>(iterable);
    assertEquals(expected, set);
  }

  @Test
  public void testAddAllIterator() {
    List<Integer> iterable = Arrays.asList(1, 2, 3, 1, 4, 5, 6);

    Iterator<Integer> iterator = iterable.iterator();
    List<Integer> list = new ArrayList<Integer>();
    CollectionUtilities.addAll(iterator, list);
    assertEquals(iterable, list);

    iterator = iterable.iterator();
    Set<Integer> set = new HashSet<Integer>();
    CollectionUtilities.addAll(iterator, set);
    Set<Integer> expected = new HashSet<Integer>(iterable);
    assertEquals(expected, set);
  }

  @Test
  public void testConvertToListOfSuperclass() {
    ImmutableList<Integer> ints = ImmutableList.of(1, 2, 3, 1, 4, 5, 6);
    ImmutableList<Number> nums = CollectionUtilities.convertToListOfSuperclass(ints);

    assertSame(nums, ints);

    // These should be the same object:
    Object expected = ints.get(0);
    Object actual = nums.get(0);
    assertSame(expected, actual);
  }

}
