package picante.timeline;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import java.util.List;
import org.easymock.EasyMock;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Function;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.Lists;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;

public class AbstractStateTimelineTest {

  private static final int UNIQUE_SIZE = 20;
  private static final int REPEATING_SIZE = 20;
  private static final int REPEATING_MODULO = 3;


  private ImmutableSortedMap<UnwritableInterval, Integer> emptyMap;
  private ImmutableSortedMap<UnwritableInterval, Integer> uniqueMap;
  private ImmutableSortedMap<UnwritableInterval, Integer> repeatingMap;

  private AbstractStateTimeline<Integer> unique;
  private AbstractStateTimeline<Integer> repeating;


  @Before
  public void setUp() throws Exception {

    emptyMap = ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
        .build();

    ImmutableSortedMap.Builder<UnwritableInterval, Integer> uniqueBuilder =
        ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      uniqueBuilder.put(new UnwritableInterval(i, i + 1), i);
    }
    uniqueMap = uniqueBuilder.build();

    unique = EasyMock.partialMockBuilder(AbstractStateTimeline.class).withConstructor(uniqueMap)
        .createMock();
    EasyMock.expect(unique.getDomain()).andReturn(new UnwritableInterval(0, UNIQUE_SIZE))
        .anyTimes();
    EasyMock.replay(unique);

    ImmutableSortedMap.Builder<UnwritableInterval, Integer> repeatingBuilder =
        ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);
    for (int i = 0; i < REPEATING_SIZE; i++) {
      repeatingBuilder.put(new UnwritableInterval(i, i + 1), i % REPEATING_MODULO);
    }
    repeatingMap = repeatingBuilder.build();

    repeating = EasyMock.partialMockBuilder(AbstractStateTimeline.class)
        .withConstructor(repeatingMap).createMock();
    EasyMock.expect(repeating.getDomain()).andReturn(new UnwritableInterval(0, UNIQUE_SIZE))
        .anyTimes();
    EasyMock.replay(repeating);

  }

  @After
  public void tearDown() throws Exception {
    EasyMock.verify(unique);
    EasyMock.verify(repeating);
  }

  @Test
  public void testCoveringSubsetMapEmptyMap() {
    assertNull(
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(0.0, 10.0), emptyMap));
  }

  @Test
  public void testCoveringSubsetMapZeroLengthInterval() {
    assertNull(
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(1.0, 1.0), uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapBeginBoundaryPointInterval() {
    assertNull(
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(0.0, 0.0), uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapEndBoundaryPointInterval() {
    assertNull(AbstractStateTimeline
        .coveringSubsetMap(new UnwritableInterval(UNIQUE_SIZE + 1, UNIQUE_SIZE + 1), uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapPriorInterval() {
    assertNull(
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(-10.0, -9.0), uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapLaterInterval() {
    assertNull(AbstractStateTimeline
        .coveringSubsetMap(new UnwritableInterval(UNIQUE_SIZE + 10, UNIQUE_SIZE + 11), uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapLargerQueryInterval() {
    assertEquals(uniqueMap, AbstractStateTimeline
        .coveringSubsetMap(new UnwritableInterval(-1.0, UNIQUE_SIZE + 2), uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapBeginLargerQueryInterval() {
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(0.0, 0.0), true, new UnwritableInterval(10.0, 10.0),
            true),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(-1.0, 10.5), uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(0.0, 0.0), true, new UnwritableInterval(10.0, 10.0),
            true),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(-1.0, 11.0), uniqueMap));

  }

  @Test
  public void testCoveringSubsetMapEndLargerQueryInterval() {
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(9.0, 9.0), true,
            new UnwritableInterval(UNIQUE_SIZE + 2, UNIQUE_SIZE + 2), true),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(9.5, UNIQUE_SIZE + 3),
            uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(10.0, 10.0), true,
            new UnwritableInterval(UNIQUE_SIZE + 2, UNIQUE_SIZE + 2), true),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(10.0, UNIQUE_SIZE + 3),
            uniqueMap));

  }

  @Test
  public void testCoveringSubsetMapSmallInterval() {
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(5.0, 6.0), new UnwritableInterval(6.0, 7.0)),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(5.5, 5.8), uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(0.0, 1.0), new UnwritableInterval(1.0, 2.0)),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(0.0, 0.0 + Math.ulp(0.0)),
            uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(0.0, 1.0), new UnwritableInterval(1.0, 2.0)),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(1.0 - Math.ulp(1.0), 1.0),
            uniqueMap));
  }

  @Test
  public void testCoveringSubsetMapExactInterval() {
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(5.0, 6.0), new UnwritableInterval(10.0, 11.0)),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(5.0, 10.0), uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(0.0, 1.0), new UnwritableInterval(1.0, 2.0)),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(0.0, 1.0), uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(5.0, 6.0), new UnwritableInterval(6.0, 7.0)),
        AbstractStateTimeline.coveringSubsetMap(new UnwritableInterval(5.0, 6.0), uniqueMap));
    assertEquals(
        uniqueMap.subMap(new UnwritableInterval(UNIQUE_SIZE - 1, UNIQUE_SIZE),
            new UnwritableInterval(UNIQUE_SIZE, UNIQUE_SIZE + 1)),
        AbstractStateTimeline
            .coveringSubsetMap(new UnwritableInterval(UNIQUE_SIZE - 1, UNIQUE_SIZE), uniqueMap));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetStateDoubleOffBeginException() {
    unique.getState(-0.1);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetStateDoubleOffEndException() {
    unique.getState(UNIQUE_SIZE + 0.1);
  }

  @Test
  public void testGetStateDoubleIntermediateValues() {
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), unique.getState(i + 0.1));
    }
    for (int i = 0; i < REPEATING_SIZE; i++) {
      assertEquals(Integer.valueOf(i % REPEATING_MODULO), repeating.getState(i + 0.1));
    }
  }

  @Test
  public void testGetStateOnValues() {
    assertEquals(Integer.valueOf(0), unique.getState(0.0));
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), unique.getState(i));
    }
    assertEquals(Integer.valueOf(UNIQUE_SIZE - 1), unique.getState(UNIQUE_SIZE));

    assertEquals(Integer.valueOf(0 % REPEATING_MODULO), repeating.getState(0.0));
    for (int i = 1; i < REPEATING_SIZE; i++) {
      assertEquals(Integer.valueOf(i % REPEATING_MODULO), repeating.getState(i));
    }
    assertEquals(Integer.valueOf((UNIQUE_SIZE - 1) % REPEATING_MODULO),
        repeating.getState(UNIQUE_SIZE));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetStateDoubleDirectionOffBeginException() {
    unique.getState(-0.1, Direction.PREVIOUS);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetStateDoubleDirectionOffEndException() {
    unique.getState(UNIQUE_SIZE + 0.1, Direction.NEXT);
  }


  @Test
  public void testGetStateDoubleDirectionIntermediateValues() {
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), unique.getState(i + 0.1, Direction.PREVIOUS));
      assertEquals(Integer.valueOf(i), unique.getState(i + 0.1, Direction.NEXT));

    }
    for (int i = 0; i < REPEATING_SIZE; i++) {
      assertEquals(Integer.valueOf(i % REPEATING_MODULO),
          repeating.getState(i + 0.1, Direction.PREVIOUS));
      assertEquals(Integer.valueOf(i % REPEATING_MODULO), repeating.getState(i + 0.1, Direction.NEXT));
    }

  }

  @Test
  public void testGetStateDoubleDirectionNextOnValues() {
    assertEquals(Integer.valueOf(0), unique.getState(0.0, Direction.NEXT));
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), unique.getState(i, Direction.NEXT));
    }
    assertEquals(Integer.valueOf(UNIQUE_SIZE - 1), unique.getState(UNIQUE_SIZE, Direction.NEXT));

    assertEquals(Integer.valueOf(0 % REPEATING_MODULO), repeating.getState(0.0, Direction.NEXT));
    for (int i = 1; i < REPEATING_SIZE; i++) {
      assertEquals(Integer.valueOf(i % REPEATING_MODULO), repeating.getState(i, Direction.NEXT));
    }
    assertEquals(Integer.valueOf((UNIQUE_SIZE - 1) % REPEATING_MODULO),
        repeating.getState(UNIQUE_SIZE, Direction.NEXT));
  }

  @Test
  public void testGetStateDoubleDirectionPreviousOnValues() {
    assertEquals(Integer.valueOf(0), unique.getState(0.0, Direction.PREVIOUS));
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i - 1), unique.getState(i, Direction.PREVIOUS));
    }
    assertEquals(Integer.valueOf(UNIQUE_SIZE - 1), unique.getState(UNIQUE_SIZE, Direction.PREVIOUS));

    assertEquals(Integer.valueOf(0 % REPEATING_MODULO), repeating.getState(0.0, Direction.PREVIOUS));
    for (int i = 1; i < REPEATING_SIZE; i++) {
      assertEquals(Integer.valueOf((i - 1) % REPEATING_MODULO),
          repeating.getState(i, Direction.PREVIOUS));
    }
    assertEquals(Integer.valueOf((UNIQUE_SIZE - 1) % REPEATING_MODULO),
        repeating.getState(UNIQUE_SIZE, Direction.PREVIOUS));
  }

  @Test
  public void testFunction() {
    Function<Double, Integer> function = unique.function();
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), function.apply(i + 0.1));
    }
    assertEquals(Integer.valueOf(0), function.apply(0.0));
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), function.apply((double) i));
    }
    assertEquals(Integer.valueOf(UNIQUE_SIZE - 1), function.apply((double) UNIQUE_SIZE));
  }

  @Test
  public void testFunctionDirection() {
    Function<Double, Integer> function = unique.function(Direction.PREVIOUS);
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i), function.apply(i + 0.1));
    }

    assertEquals(Integer.valueOf(0), function.apply(0.0));
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      assertEquals(Integer.valueOf(i - 1), function.apply((double) i));
    }
    assertEquals(Integer.valueOf(UNIQUE_SIZE - 1), function.apply((double) UNIQUE_SIZE));
  }

  @Test
  public void testGetTransitionTimes() {
    List<Double> expected = Lists.newArrayList();
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      expected.add((double) i);
    }
    assertEquals(expected, Lists.newArrayList(unique.getTransitionTimes()));

    expected.clear();

    for (int i = 1; i < REPEATING_SIZE; i++) {
      expected.add((double) i);
    }
    assertEquals(expected, Lists.newArrayList(repeating.getTransitionTimes()));
  }

  @Test
  public void testGetStates() {
    List<Integer> expected = Lists.newArrayList();
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      expected.add(i);
    }
    assertEquals(expected, ImmutableList.copyOf(unique.getStates()));

    expected = Lists.newArrayList();
    for (int i = 0; i < REPEATING_SIZE; i++) {
      expected.add(i % REPEATING_MODULO);
    }
    assertEquals(expected, ImmutableList.copyOf(repeating.getStates()));
  }

  @Test
  public void testGetEntries() {
    assertEquals(uniqueMap.entrySet(), unique.getEntries());
    assertEquals(repeatingMap.entrySet(), repeating.getEntries());
  }

  @Test
  public void testEquals() {
    assertNotEquals(repeating, unique);
    assertEquals(repeating, repeating);
    assertEquals(unique, unique);
    assertNotEquals(unique, null);
    assertNotEquals(null, unique);

    /*
     * ImmutableSortedMap.Builder<UnwritableInterval, Integer> uniqueBuilder =
     * ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR); for (int i = 0; i < UNIQUE_SIZE;
     * i++) { uniqueBuilder.put(new UnwritableInterval(i, i + 1), i); } StateTimeline<Integer>
     * notUnique = new StateTimeline<>(uniqueBuilder.build());
     * 
     * assertEquals(unique, notUnique);
     */

    /*
     * Since the underlying code is using the equals() definition on SortedMap, which utilizes the
     * comparator to compare keys, make certain we are properly checking key equality in the sense
     * we wish to define it here.
     */
    StateTimeline<Integer> small = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(1.1, 1.2), Integer.valueOf(1)).build());
    StateTimeline<Integer> notSmall = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(1.1, 150), Integer.valueOf(1)).build());

    assertNotEquals(small, notSmall);

    StateTimeline<Integer> x = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(1.1, 1.2), Integer.valueOf(50)).build());
    StateTimeline<Integer> notX = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(1.1, 1.2), Integer.valueOf(51)).build());

    assertNotEquals(x, notX);


  }


}
