package picante.timeline;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.LinkedHashMultiset;
import com.google.common.collect.Lists;
import com.google.common.collect.Multiset;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

public class StateTimelineTest {

  private static final int UNIQUE_SIZE = 20;
  private static final int REPEATING_SIZE = 20;
  private static final int REPEATING_MODULO = 3;

  private ImmutableSortedMap<UnwritableInterval, Integer> uniqueMap;
  private ImmutableSortedMap<UnwritableInterval, Integer> repeatingMap;

  private StateTimeline<Integer> unique;
  private StateTimeline<Integer> repeating;

  @Before
  public void setUp() {

    ImmutableSortedMap.Builder<UnwritableInterval, Integer> uniqueBuilder =
        ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      uniqueBuilder.put(new UnwritableInterval(i, i + 1), i);
    }
    uniqueMap = uniqueBuilder.build();

    unique = new StateTimeline<>(uniqueMap);

    ImmutableSortedMap.Builder<UnwritableInterval, Integer> repeatingBuilder =
        ImmutableSortedMap.orderedBy(Interval.BEGIN_COMPARATOR);
    for (int i = 0; i < REPEATING_SIZE; i++) {
      repeatingBuilder.put(new UnwritableInterval(i, i + 1), i % REPEATING_MODULO);
    }
    repeatingMap = repeatingBuilder.build();

    repeating = new StateTimeline<>(repeatingMap);
  }


  @Test
  public void testGetDomain() {
    assertSame(unique.getDomain(), unique.getDomain());
    assertSame(repeating.getDomain(), repeating.getDomain());
  }

  @Test
  public void testGetSupport() {
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      assertEquals(IntervalSet.create(i, i + 1), unique.getSupport(i));
    }
    for (int i = 0; i < REPEATING_MODULO; i++) {

      IntervalSet.Builder expectedBuilder = IntervalSet.builder();
      for (int j = 0; j < REPEATING_SIZE; j++) {
        if (j % REPEATING_MODULO == i) {
          expectedBuilder.add(j, j + 1);
        }
      }
      assertEquals(expectedBuilder.build(), repeating.getSupport(i));
    }
  }

  @Test
  public void testGetSupportingIntervalsPredicate() {
    Predicate<Integer> predicate = (input) -> {
      return input < 10;
    };

    ImmutableList<UnwritableInterval> result = unique.getSupportIntervals(predicate);
    assertEquals(10, result.size());
    for (int i = 0; i < 10; i++) {
      assertEquals(new UnwritableInterval(i, i + 1), result.get(i));
    }

  }

  @Test
  public void testGetSupportPredicate() {
    Predicate<Integer> predicate = new Predicate<Integer>() {

      @Override
      public boolean apply(Integer input) {
        return input.equals(0) || input.equals(1);
      }
    };

    IntervalSet.Builder expectedBuilder = IntervalSet.builder();
    for (int i = 0; i < REPEATING_SIZE; i++) {
      if (predicate.apply(i % REPEATING_MODULO)) {
        expectedBuilder.add(i, i + 1);
      }
    }

    assertEquals(expectedBuilder.build(), repeating.getSupport(predicate));
  }

  @Test
  public void testGetTransitionTimes() {
    List<Double> expected = Lists.newArrayList();
    for (int i = 1; i < UNIQUE_SIZE; i++) {
      expected.add((double) i);
    }
    assertEquals(expected, unique.getTransitionTimes());

    expected.clear();

    for (int i = 1; i < REPEATING_SIZE; i++) {
      expected.add((double) i);
    }
    assertEquals(expected, repeating.getTransitionTimes());
  }

  @Test
  public void testGetStates() {
    List<Integer> expected = Lists.newArrayList();
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      expected.add(i);
    }
    assertEquals(expected, unique.getStates());

    expected = Lists.newArrayList();
    for (int i = 0; i < REPEATING_SIZE; i++) {
      expected.add(i % REPEATING_MODULO);
    }
    assertEquals(expected, repeating.getStates());
  }

  @Test
  public void testGetStatesMultiset() {
    Multiset<Integer> expected = LinkedHashMultiset.create();
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      expected.add(i);
    }
    assertEquals(expected, unique.getStatesMultiset());

    expected = LinkedHashMultiset.create();
    for (int i = 0; i < REPEATING_SIZE; i++) {
      expected.add(i % REPEATING_MODULO);
    }
    assertEquals(expected, repeating.getStatesMultiset());
  }

  // @Test
  // public void testGetEntries() {
  // assertEquals(uniqueMap.entrySet(), unique.getEntries());
  // assertEquals(repeatingMap.entrySet(), repeating.getEntries());
  // assertEquals(uniqueMap.entrySet().asList(), unique.getEntries());
  // assertEquals(repeatingMap.entrySet().asList(), repeating.getEntries());
  // }


  @Test(expected = IllegalArgumentException.class)
  public void testSubsetBeforeIllegalArgumentException() {
    unique.subset(new UnwritableInterval(-10, -9));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetAfterIllegalArgumentException() {
    unique.subset(new UnwritableInterval(200, 201));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetBeginOverlapIllegalArgumentException() {
    unique.subset(new UnwritableInterval(-10, 5));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetEndOverlapIllegalArgumentException() {
    unique.subset(new UnwritableInterval(UNIQUE_SIZE - 5, UNIQUE_SIZE + 10));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetBeginTouchIllegalArgumentException() {
    unique.subset(new UnwritableInterval(-5, 0));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetEndTouchIllegalArgumentException() {
    unique.subset(new UnwritableInterval(UNIQUE_SIZE, UNIQUE_SIZE + 2));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetCoveringIllegalArgumentException() {
    unique.subset(new UnwritableInterval(-1, UNIQUE_SIZE + 2));
  }


  @Test
  public void testSubsetTinySingleInterval() {
    StateTimeline<Integer> expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(1.1, 1.2), Integer.valueOf(1)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(1.1, 1.2)));

    expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(0.0, 0.1), Integer.valueOf(0)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(0.0, 0.1)));

    expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(19.9, 20.0), Integer.valueOf(19)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(19.9, 20.0)));
  }

  @Test
  public void testSubsetExactInterval() {
    StateTimeline<Integer> expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(0.0, 1.0), Integer.valueOf(0)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(0.0, 1.0)));

    expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(5.0, 6.0), Integer.valueOf(5)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(5.0, 6.0)));

    expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(19.0, 20.0), Integer.valueOf(19)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(19, 20)));
  }

  @Test
  public void testSubsetDomainInterval() {
    assertEquals(unique, unique.subset(unique.getDomain()));
  }

  @Test
  public void testSubsetMultipleIntervals() {
    StateTimeline<Integer> expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(0.0, 1.0), Integer.valueOf(0))
            .put(new UnwritableInterval(1.0, 1.5), Integer.valueOf(1)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(0.0, 1.5)));

    expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(18.5, 19.0), Integer.valueOf(18))
            .put(new UnwritableInterval(19.0, 20.0), Integer.valueOf(19)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(18.5, 20.0)));

    expected = new StateTimeline<Integer>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(5.5, 6.0), Integer.valueOf(5))
            .put(new UnwritableInterval(6.0, 7.0), Integer.valueOf(6))
            .put(new UnwritableInterval(7.0, 7.4), Integer.valueOf(7)).build());
    assertEquals(expected, unique.subset(new UnwritableInterval(5.5, 7.4)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetLessThanBeforeStartPointIllegalArgumentException() {
    unique.subsetLessThan(-10.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetLessThanStartPointIllegalArgumentException() {
    unique.subsetLessThan(0.0);
  }

  @Test
  public void testSubsetLessThan() {
    StateTimeline<Integer> expected = new StateTimeline<>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(0.0, 1.0), Integer.valueOf(0))
            .put(new UnwritableInterval(1.0, 1.5), Integer.valueOf(1)).build());
    assertEquals(expected, unique.subsetLessThan(1.5));
  }

  @Test
  public void testSubsetLessThanEntireTimeline() {
    assertEquals(unique, unique.subsetLessThan(unique.getDomain().getEnd()));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetLessThanAfterEndIllegalArgumentException() {
    unique.subsetLessThan(Double.MAX_VALUE);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetGreaterThanAfterStartPointIllegalArgumentException() {
    unique.subsetGreaterThan(Double.MAX_VALUE);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetGreaterThanEndPointIllegalArgumentException() {
    unique.subsetGreaterThan(unique.getDomain().getEnd());
  }

  @Test
  public void testSubsetGreaterThan() {
    StateTimeline<Integer> expected = new StateTimeline<>(
        ImmutableSortedMap.<UnwritableInterval, Integer>orderedBy(Interval.BEGIN_COMPARATOR)
            .put(new UnwritableInterval(18.5, 19.0), Integer.valueOf(18))
            .put(new UnwritableInterval(19.0, 20.0), Integer.valueOf(19)).build());
    assertEquals(expected, unique.subsetGreaterThan(18.5));
  }

  @Test
  public void testSubsetGreaterThanEntireTimeline() {
    assertEquals(unique, unique.subsetGreaterThan(0.0));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSubsetGreaterThanBeforePointIllegalArgumentException() {
    unique.subsetGreaterThan(-10.0);
  }

  @Test
  public void testTransformOneToOneIntervalConversion() {
    StateTimeline.Builder<Double> b = StateTimeline.create(unique.getDomain(), 0.0);
    for (int i = 0; i < UNIQUE_SIZE; i++) {
      b.add(new UnwritableInterval(i, i + 1), (double) i);
    }
    StateTimeline<Double> expected = b.build();
    assertEquals(expected, unique.transform(i -> (double) i));
  }

  @Test
  public void testTransformTwoToOneIntervalConversion() {
    StateTimeline.Builder<Integer> b = StateTimeline.create(unique.getDomain(), 0);
    for (int i = 0; i < UNIQUE_SIZE / 2; i++) {
      b.add(new UnwritableInterval(2 * i, 2 * (i + 1)), i);
    }
    StateTimeline<Integer> expected = b.build();
    assertEquals(expected, unique.transform(i -> i / 2));
  }

}

