package picante.timeline;

import static com.google.common.base.Preconditions.checkArgument;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.math.DoubleMath;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

public class StateTimelineBuilderTest {

  private StateTimeline.Builder<Integer> prepopulatedExpanding;
  private StateTimeline.Builder<Integer> prepopulated;

  @Before
  public void setUp() throws Exception {

    prepopulatedExpanding = StateTimeline.createExpanding(new UnwritableInterval(0, 1), 0);
    prepopulatedExpanding.add(new UnwritableInterval(1, 2), 1);
    prepopulatedExpanding.add(new UnwritableInterval(2, 3), 2);
    prepopulatedExpanding.add(new UnwritableInterval(3, 4), 3);

    prepopulated = StateTimeline.create(prepopulatedExpanding.build());

  }

  @Test
  public void testGetDomain() {
    assertEquals(new UnwritableInterval(0, 4), prepopulatedExpanding.getDomain());
    assertEquals(new UnwritableInterval(0, 4), prepopulated.getDomain());
  }

  @Test
  public void testGetDomainAsView() {
    UnwritableInterval domain = prepopulatedExpanding.getDomain();
    assertEquals(new UnwritableInterval(0, 4), domain);

    prepopulatedExpanding.add(5, 6, -1);
    assertEquals(new UnwritableInterval(0, 6), domain);
  }

  @Test
  public void testGetTransitionTimesAsView() {
    Iterable<Double> view = prepopulatedExpanding.getTransitionTimes();
    List<Double> expected = Lists.newArrayList(1.0, 2.0, 3.0);
    assertEquals(expected, Lists.newArrayList(view));
    prepopulatedExpanding.add(0.5, 0.75, 200);
    List<Double> viewAlteredExpected = Lists.newArrayList(0.5, 0.75, 1.0, 2.0, 3.0);
    assertEquals(viewAlteredExpected, Lists.newArrayList(view));

    view = prepopulated.getTransitionTimes();
    assertEquals(expected, Lists.newArrayList(view));
    prepopulated.add(0.5, 0.75, 200);
    assertEquals(viewAlteredExpected, Lists.newArrayList(view));
  }

  @Test
  public void testGetStatesAsView() {
    Iterable<Integer> view = prepopulatedExpanding.getStates();
    List<Integer> expected = Lists.newArrayList(0, 1, 2, 3);
    assertEquals(expected, Lists.newArrayList(view));
    prepopulatedExpanding.add(0.5, 0.75, 200);
    List<Integer> viewAlteredExpected = Lists.newArrayList(0, 200, 0, 1, 2, 3);
    assertEquals(viewAlteredExpected, Lists.newArrayList(view));

    view = prepopulated.getStates();
    assertEquals(expected, Lists.newArrayList(view));
    prepopulated.add(0.5, 0.75, 200);
    assertEquals(viewAlteredExpected, Lists.newArrayList(view));
  }

  @Test
  public void testGetEntriesAsView() {
    Set<Entry<UnwritableInterval, Integer>> view = prepopulatedExpanding.getEntries();
    NavigableMap<UnwritableInterval, Integer> expected = Maps.newTreeMap(Interval.BEGIN_COMPARATOR);
    expected.put(new UnwritableInterval(0, 1), 0);
    expected.put(new UnwritableInterval(1, 2), 1);
    expected.put(new UnwritableInterval(2, 3), 2);
    expected.put(new UnwritableInterval(3, 4), 3);
    assertEquals(expected.entrySet(), view);

    prepopulatedExpanding.add(3, 4, 200);
    NavigableMap<UnwritableInterval, Integer> viewAlteredExpected =
        Maps.newTreeMap(Interval.BEGIN_COMPARATOR);
    viewAlteredExpected.putAll(expected);
    viewAlteredExpected.put(new UnwritableInterval(3, 4), 200);
    assertEquals(viewAlteredExpected.entrySet(), view);

    view = prepopulated.getEntries();
    assertEquals(expected.entrySet(), view);

    prepopulated.add(3, 4, 200);
    assertEquals(viewAlteredExpected.entrySet(), view);

  }

  @Test
  public void testInitialStateBuild() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 1);
    StateTimeline<Integer> nonExpanding =
        StateTimeline.create(new UnwritableInterval(0, 1), 1).build();
    assertEquals(expected, nonExpanding);

    StateTimeline<Integer> expanding =
        StateTimeline.createExpanding(new UnwritableInterval(0, 1), 1).build();
    assertEquals(expected, expanding);
  }

  @Test
  public void testIntervalReplaceWithDifferentState() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 200, 1, 2, 1, 2, 3, 2, 3, 4, 3);
    prepopulatedExpanding.add(new UnwritableInterval(0, 1), 200);
    assertEquals(expected, prepopulatedExpanding.build());
    prepopulated.add(new UnwritableInterval(0, 1), 200);
    assertEquals(expected, prepopulated.build());
  }

  @Test
  public void testIncrementalBuilding() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testOverwriteEntireDomain() {
    prepopulatedExpanding.add(new UnwritableInterval(-1, 5), -1);
    assertEquals(createTimeline(-1, 5, -1), prepopulatedExpanding.build());
  }

  @Test
  public void testOverwriteStartBeforeEndAtDomain() {
    prepopulatedExpanding.add(new UnwritableInterval(-1, 4), -1);
    assertEquals(createTimeline(-1, 4, -1), prepopulatedExpanding.build());
  }

  @Test
  public void testOverwriteStartAtDomainAfterEnd() {
    prepopulatedExpanding.add(new UnwritableInterval(0, 5), -1);
    assertEquals(createTimeline(0, 5, -1), prepopulatedExpanding.build());
  }

  @Test
  public void testOverwriteStartAtDomainEndAtDomain() {
    prepopulatedExpanding.add(new UnwritableInterval(0, 4), -1);
    assertEquals(createTimeline(0, 4, -1), prepopulatedExpanding.build());
  }

  @Test
  public void testIsolatedBeginIntervalDifferentStates() {
    prepopulatedExpanding.add(new UnwritableInterval(0, 1), 200);
    prepopulatedExpanding.add(new UnwritableInterval(-2, -1), -1);
    assertEquals(createTimeline(-2, -1, -1, -1, 0, 0, 0, 1, 200, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testIsolatedBeginIntervalSameStateAsDefault() {
    prepopulatedExpanding.add(new UnwritableInterval(0, 1), 200);
    prepopulatedExpanding.add(new UnwritableInterval(-2, -1), 0);
    assertEquals(createTimeline(-2, 0, 0, 0, 1, 200, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testIsolatedBeginIntervalSameStateAsDefaultAndAdjacentInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, -1), 0);
    assertEquals(createTimeline(-2, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testAdjacentBeginInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, 0), -1);
    assertEquals(createTimeline(-2, 0, -1, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testAdjacentBeginIntervalSameState() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, 0), 0);
    assertEquals(createTimeline(-2, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapBeginInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, 0.5), -1);
    assertEquals(createTimeline(-2.0, 0.5, -1, 0.5, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapFirstInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, 1), -1);
    assertEquals(createTimeline(-2, 1, -1, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapBeginIntervalSameState() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, 0.5), 0);
    assertEquals(createTimeline(-2, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3),
        prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapFirstIntervalSameState() {
    prepopulatedExpanding.add(new UnwritableInterval(-2, 1), 1);
    assertEquals(createTimeline(-2, 2, 1, 2, 3, 2, 3, 4, 3), prepopulatedExpanding.build());
  }

  @Test
  public void testIsolatedEndIntervalDifferentStates() {
    prepopulatedExpanding.add(new UnwritableInterval(3, 4), 200);
    prepopulatedExpanding.add(new UnwritableInterval(5, 6), -1);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 200, 4, 5, 0, 5, 6, -1),
        prepopulatedExpanding.build());
  }

  @Test
  public void testIsolatedEndIntervalSameStateAsDefault() {
    prepopulatedExpanding.add(new UnwritableInterval(3, 4), 200);
    prepopulatedExpanding.add(new UnwritableInterval(5, 6), 0);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 200, 4, 6, 0),
        prepopulatedExpanding.build());
  }

  @Test
  public void testIsolatedEndIntervalSameStateAsDefaultAndAdjacentInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(3, 4), 0);
    prepopulatedExpanding.add(new UnwritableInterval(5, 6), 0);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 6, 0), prepopulatedExpanding.build());
  }

  @Test
  public void testAdjacentEndInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(4, 6), -1);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 6, -1),
        prepopulatedExpanding.build());
  }

  @Test
  public void testAdjacentEndIntervalSameState() {
    prepopulatedExpanding.add(new UnwritableInterval(4, 6), 3);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 6, 3), prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapEndInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(3.5, 6), -1);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 3.5, 3, 3.5, 6, -1),
        prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapLastInterval() {
    prepopulatedExpanding.add(new UnwritableInterval(3, 6), -1);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 6, -1),
        prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapEndIntervalSameState() {
    prepopulatedExpanding.add(new UnwritableInterval(3.5, 6), 3);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 6, 3), prepopulatedExpanding.build());
  }

  @Test
  public void testOverlapLastIntervalSameState() {
    prepopulatedExpanding.add(new UnwritableInterval(3, 6), 2);
    assertEquals(createTimeline(0, 1, 0, 1, 2, 1, 2, 6, 2), prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalInFirstAtFront() {
    StateTimeline<Integer> expected =
        createTimeline(0, 0.1, 200, 0.1, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(0.0, 0.1), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(0.0, 0.1), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalInFirstAtMiddle() {
    StateTimeline<Integer> expected =
        createTimeline(0, 0.5, 0, 0.5, 0.6, 200, 0.6, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(0.5, 0.6), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(0.5, 0.6), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalInFirstAtBack() {
    StateTimeline<Integer> expected =
        createTimeline(0, 0.9, 0, 0.9, 1, 200, 1, 2, 1, 2, 3, 2, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(0.9, 1), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(0.9, 1), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalInLastAtFront() {
    StateTimeline<Integer> expected =
        createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 3.2, 200, 3.2, 4, 3);
    prepopulated.add(new UnwritableInterval(3.0, 3.2), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(3.0, 3.2), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalInLastAtMiddle() {
    StateTimeline<Integer> expected =
        createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 3.4, 3, 3.4, 3.5, 200, 3.5, 4, 3);
    prepopulated.add(new UnwritableInterval(3.4, 3.5), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(3.4, 3.5), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalInLastAtBack() {
    StateTimeline<Integer> expected =
        createTimeline(0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 3.9, 3, 3.9, 4, 200);
    prepopulated.add(new UnwritableInterval(3.9, 4), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(3.9, 4), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalCoveringSingleTransition() {
    StateTimeline<Integer> expected =
        createTimeline(0, 1, 0, 1, 1.9, 1, 1.9, 2.1, 200, 2.1, 3, 2, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(1.9, 2.1), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(1.9, 2.1), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalCoveringSingleTransitionPreviousState() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 2.1, 1, 2.1, 3, 2, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(1.9, 2.1), 1);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(1.9, 2.1), 1);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testSmallIntervalCoveringSingleTransitionNextState() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 1.9, 1, 1.9, 3, 2, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(1.9, 2.1), 2);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(1.9, 2.1), 2);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testLargeIntervalReplacement() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 3, 200, 3, 4, 3);
    prepopulated.add(new UnwritableInterval(1, 3), 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(new UnwritableInterval(1, 3), 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testLargeIntervalReplacementWithPredicate() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 3, 200, 3, 4, 3);
    assertTrue(prepopulated.add(new UnwritableInterval(1, 3), 200, Predicates.alwaysFalse()));
    assertEquals(expected, prepopulated.build());
    assertTrue(
        prepopulatedExpanding.add(new UnwritableInterval(1, 3), 200, Predicates.alwaysFalse()));
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testLargeIntervalReplacementWithProtectedPredicate() {
    StateTimeline<Integer> expected = prepopulated.build();
    assertFalse(prepopulated.add(new UnwritableInterval(1, 3), 200, Predicates.alwaysTrue()));
    assertEquals(expected, prepopulated.build());
    assertFalse(
        prepopulatedExpanding.add(new UnwritableInterval(1, 3), 200, Predicates.alwaysTrue()));
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testIterableUnwritableInterval() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 2, 200, 2, 3, 2, 3, 4, 200);
    List<UnwritableInterval> intervals =
        Lists.newArrayList(new UnwritableInterval(1, 2), new UnwritableInterval(3, 4));
    prepopulated.add(intervals, 200);
    assertEquals(expected, prepopulated.build());
    prepopulatedExpanding.add(intervals, 200);
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testIterableUnwritableIntervalException() {
    StateTimeline<Integer> expected = prepopulated.build();
    try {
      List<UnwritableInterval> intervals =
          Lists.newArrayList(new UnwritableInterval(1, 2), new UnwritableInterval(3.5, 4.5));
      prepopulated.add(intervals, 200);
    } catch (IllegalArgumentException e) {
      assertEquals(expected, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }

  @Test
  public void testIterableUnwritableIntervalWithPredicateFalse() {
    StateTimeline<Integer> expected = createTimeline(0, 1, 0, 1, 2, 200, 2, 3, 2, 3, 4, 200);
    List<UnwritableInterval> intervals =
        Lists.newArrayList(new UnwritableInterval(1, 2), new UnwritableInterval(3, 4));
    assertTrue(prepopulated.add(intervals, 200, Predicates.alwaysFalse()));
    assertEquals(expected, prepopulated.build());
    assertTrue(prepopulatedExpanding.add(intervals, 200, Predicates.alwaysFalse()));
    assertEquals(expected, prepopulatedExpanding.build());
  }

  @Test
  public void testIterableUnwritableIntervalWithPredicateTrue() {
    StateTimeline<Integer> expected = prepopulated.build();
    List<UnwritableInterval> intervals =
        Lists.newArrayList(new UnwritableInterval(1, 2), new UnwritableInterval(3, 4));
    assertFalse(prepopulated.add(intervals, 200, Predicates.alwaysTrue()));
    assertEquals(expected, prepopulated.build());
    assertFalse(prepopulatedExpanding.add(intervals, 200, Predicates.alwaysTrue()));
    assertEquals(expected, prepopulatedExpanding.build());
  }


  @Test
  public void testIterableUnwritableIntervalExceptionWithPredicate() {
    StateTimeline<Integer> expected = prepopulated.build();
    try {
      List<UnwritableInterval> intervals =
          Lists.newArrayList(new UnwritableInterval(1, 2), new UnwritableInterval(3.5, 4.5));
      prepopulated.add(intervals, 200, Predicates.alwaysFalse());
    } catch (IllegalArgumentException e) {
      assertEquals(expected, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }


  @Test
  public void testConstrainedLargerThanDomain() {
    StateTimeline<Integer> prior = prepopulated.build();
    try {
      prepopulated.add(new UnwritableInterval(-1, 5), 200);
    } catch (IllegalArgumentException e) {
      assertEquals(prior, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }

  @Test
  public void testConstrainedLargerThanDomainNonAdjacentBefore() {
    StateTimeline<Integer> prior = prepopulated.build();
    try {
      prepopulated.add(new UnwritableInterval(-2, -1), 200);
    } catch (IllegalArgumentException e) {
      assertEquals(prior, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }

  @Test
  public void testConstrainedLargerThanDomainAdjacentBefore() {
    StateTimeline<Integer> prior = prepopulated.build();
    try {
      prepopulated.add(new UnwritableInterval(-1, 0), 200);
    } catch (IllegalArgumentException e) {
      assertEquals(prior, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }


  @Test
  public void testConstrainedLargerThanDomainNonAdjacentAfter() {
    StateTimeline<Integer> prior = prepopulated.build();
    try {
      prepopulated.add(new UnwritableInterval(5, 6), 200);
    } catch (IllegalArgumentException e) {
      assertEquals(prior, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }

  @Test
  public void testConstrainedLargerThanDomainAdjacentAfter() {
    StateTimeline<Integer> prior = prepopulated.build();
    try {
      prepopulated.add(new UnwritableInterval(4, 5), 200);
    } catch (IllegalArgumentException e) {
      assertEquals(prior, prepopulated.build());
      return;
    }
    fail("Expected exception not thrown.");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetPrecedingInterval() {
    prepopulated.buildSubset(new UnwritableInterval(-10, -9));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetPrecedingAdjacentInterval() {
    prepopulated.buildSubset(new UnwritableInterval(-5, 0));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetPrecedingOverlappingInterval() {
    prepopulated.buildSubset(new UnwritableInterval(-5, 2));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetPrecedingOverlappingInBetweenInterval() {
    prepopulated.buildSubset(new UnwritableInterval(-5, 2.1));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetAfterAdjacentInterval() {
    prepopulated.buildSubset(new UnwritableInterval(4, 7));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetAfterOverlappingInterval() {
    prepopulated.buildSubset(new UnwritableInterval(2, 7));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetAfterOverlappingInBetweenInterval() {
    prepopulated.buildSubset(new UnwritableInterval(2.2, 7));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetAfterInterval() {
    prepopulated.buildSubset(new UnwritableInterval(11, 12));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstrainedBuildSubsetLargerThanDomainInterval() {
    prepopulated.buildSubset(new UnwritableInterval(-10, 10));
  }


  @Test
  public void testConstrainedBuildSubsetEntireInterval() {
    assertEquals(prepopulated.build(), prepopulated.buildSubset(prepopulated.getDomain()));
  }

  @Test
  public void testConstrainedBuildSubsetTinyInterval() {
    assertEquals(createTimeline(1.2, 1.4, 1.0),
        prepopulated.buildSubset(new UnwritableInterval(1.2, 1.4)));
  }

  @Test
  public void testConstrainedBuildSubsetLargeOffsetInterval() {
    assertEquals(createTimeline(1.2, 2.0, 1.0, 2.0, 3.0, 2.0, 3.0, 3.5, 3.0),
        prepopulated.buildSubset(new UnwritableInterval(1.2, 3.5)));
  }

  @Test
  public void testBuildSubsetPrecedingInterval() {
    assertEquals(createTimeline(-10, -9, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(-10, -9)));
  }

  @Test
  public void testBuildSubsetPrecedingAdjacentInterval() {
    assertEquals(createTimeline(-5, 0, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(-5, 0)));
  }

  @Test
  public void testBuildSubsetPrecedingOverlappingInterval() {
    assertEquals(createTimeline(-5, 1, 0, 1, 2, 1),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(-5, 2)));
  }

  @Test
  public void testBuildSubsetPrecedingOverlappingInBetweenInterval() {
    assertEquals(createTimeline(-5, 1, 0, 1, 2, 1, 2, 2.1, 2),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(-5, 2.1)));
  }

  @Test
  public void testBuildSubsetAfterAdjacentInterval() {
    assertEquals(createTimeline(4, 7, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(4, 7)));
  }

  @Test
  public void testBuildSubsetAfterOverlappingInterval() {
    assertEquals(createTimeline(2, 3, 2, 3, 4, 3, 4, 7, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(2, 7)));
  }

  @Test
  public void testBuildSubsetAfterOverlappingInBetweenInterval() {
    assertEquals(createTimeline(2.2, 3, 2, 3, 4, 3, 4, 7, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(2.2, 7)));
  }

  @Test
  public void testBuildSubsetAfterInterval() {
    assertEquals(createTimeline(11, 12, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(11, 12)));
  }

  @Test
  public void testBuildSubsetLargerThanDomainInterval() {
    assertEquals(createTimeline(-10, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 10, 0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(-10, 10)));
  }


  @Test
  public void testBuildSubsetEntireInterval() {
    assertEquals(prepopulatedExpanding.build(),
        prepopulatedExpanding.buildSubset(prepopulatedExpanding.getDomain()));
  }

  @Test
  public void testBuildSubsetTinyInterval() {
    assertEquals(createTimeline(1.2, 1.4, 1.0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(1.2, 1.4)));
  }

  @Test
  public void testBuildSubsetLargeOffsetInterval() {
    assertEquals(createTimeline(1.2, 2.0, 1.0, 2.0, 3.0, 2.0, 3.0, 3.5, 3.0),
        prepopulatedExpanding.buildSubset(new UnwritableInterval(1.2, 3.5)));
  }

  @Test
  public void testBuildSubsetBoundaryNonDefaultStateInteractionsSingleCoveringInterval() {

    StateTimeline.Builder<Integer> builder =
        StateTimeline.createExpanding(new UnwritableInterval(0, 1), -1);

    builder.add(0, 1, 0);

    assertEquals(createTimeline(-1, 0, -1, 0, 1, 0),
        builder.buildSubset(new UnwritableInterval(-1, 1)));

    assertEquals(createTimeline(0, 1, 0, 1, 2, -1),
        builder.buildSubset(new UnwritableInterval(0, 2)));

    assertEquals(createTimeline(0.1, 0.3, 0),
        builder.buildSubset(new UnwritableInterval(0.1, 0.3)));

    assertEquals(createTimeline(-1, 0, -1, 0, 0.3, 0),
        builder.buildSubset(new UnwritableInterval(-1, 0.3)));

    assertEquals(createTimeline(0.3, 1, 0, 1, 2, -1),
        builder.buildSubset(new UnwritableInterval(0.3, 2)));

    assertEquals(createTimeline(-1, 0, -1, 0, 1, 0, 1, 2, -1),
        builder.buildSubset(new UnwritableInterval(-1, 2)));

  }

  @Test
  public void testBuildSubsetBoundaryDefaultStateInteractionsSingleCoveringInterval() {

    StateTimeline.Builder<Integer> builder =
        StateTimeline.createExpanding(new UnwritableInterval(0, 1), 0);

    assertEquals(createTimeline(-1, 1, 0), builder.buildSubset(new UnwritableInterval(-1, 1)));

    assertEquals(createTimeline(0, 2, 0), builder.buildSubset(new UnwritableInterval(0, 2)));

    assertEquals(createTimeline(0.1, 0.3, 0),
        builder.buildSubset(new UnwritableInterval(0.1, 0.3)));

    assertEquals(createTimeline(-1, 0.3, 0), builder.buildSubset(new UnwritableInterval(-1, 0.3)));

    assertEquals(createTimeline(0.3, 2, 0), builder.buildSubset(new UnwritableInterval(0.3, 2)));

    assertEquals(createTimeline(-1, 2, 0), builder.buildSubset(new UnwritableInterval(-1, 2)));

  }


  /**
   * Simple utility to create a state timeline populated with integer values in the domain and an
   * integer state.
   * 
   * @param data values to populate the timeline contents: start, end, state, start, end, state...
   * 
   * @return newly create {@link StateTimeline} with {@link Integer} state and integral values for
   *         the domain intervals.
   * 
   * @throws IllegalArgumentException if the input data does not allow the proper construction of a
   *         timeline.
   */
  static StateTimeline<Integer> createTimeline(int... data) {

    checkArgument(data.length % 3 == 0, "Data must be divisible by 3.");

    ImmutableSortedMap.Builder<UnwritableInterval, Integer> builder =
        ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);
    for (int i = 0; i < data.length / 3; i++) {
      builder.put(new UnwritableInterval(data[3 * i], data[3 * i + 1]), data[3 * i + 2]);
    }

    return new StateTimeline<>(builder.build());
  }

  /**
   * Simple utility to create a state timeline populated with integer values in the domain and an
   * integer state.
   * 
   * @param data values to populate the timeline contents: start, end, state, start, end, state...
   * 
   * @return newly create {@link StateTimeline} with {@link Integer} state (derived from the
   *         supplied doubles using {@link RoundingMode#DOWN} and double values for the domain
   *         intervals.
   * 
   * @throws IllegalArgumentException if the input data does not allow the proper construction of a
   *         timeline.
   */
  static StateTimeline<Integer> createTimeline(double... data) {

    checkArgument(data.length % 3 == 0, "Data must be divisible by 3.");

    ImmutableSortedMap.Builder<UnwritableInterval, Integer> builder =
        ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);
    for (int i = 0; i < data.length / 3; i++) {
      builder.put(new UnwritableInterval(data[3 * i], data[3 * i + 1]),
          DoubleMath.roundToInt(data[3 * i + 2], RoundingMode.DOWN));
    }

    return new StateTimeline<>(builder.build());

  }

  @Test
  public void testJavadocExampleForAddIntervalWithPredicate() {
    StateTimeline.Builder<Integer> builder = StateTimeline.create(new UnwritableInterval(0, 10), 0);
    Integer newState = -1;
    UnwritableInterval interval = new UnwritableInterval(5, 7);
    assertFalse(builder.add(interval, newState, (s) -> newState <= s));
  }

  @Test
  public void testJavadocExampleForAddDoublesWithPredicate() {
    StateTimeline.Builder<Integer> builder = StateTimeline.create(new UnwritableInterval(0, 10), 0);
    Integer newState = -1;
    assertFalse(builder.add(5, 7, newState, (s) -> newState <= s));
  }

  @Test
  public void testJavadocExampleForIntervalSetWithPredicate() {
    StateTimeline.Builder<Integer> builder = StateTimeline.create(new UnwritableInterval(0, 10), 0);
    Integer newState = -1;
    IntervalSet intervalSet = IntervalSet.create(1, 4, 5, 8);
    assertFalse(builder.add(intervalSet, newState, (s) -> newState <= s));
  }

}
