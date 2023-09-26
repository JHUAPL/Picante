package picante.math.intervals;

import static com.google.common.base.Preconditions.checkArgument;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.math.intervals.IntervalSet.EMPTY;
import static picante.math.intervals.IntervalSet.LINE;
import static picante.math.intervals.IntervalSet.builder;
import java.util.Collections;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import picante.math.intervals.IntervalSet.Builder;

public class IntervalSetTest {

  private IntervalSet singletons;
  private IntervalSet singleton;
  private IntervalSet intervals;
  private IntervalSet interval;
  private IntervalSet mixed;
  private IntervalSet empty;

  private ImmutableList<UnwritableInterval> intSingletons;
  private ImmutableList<UnwritableInterval> intSingleton;
  private ImmutableList<UnwritableInterval> intIntervals;
  private ImmutableList<UnwritableInterval> intInterval;
  private ImmutableList<UnwritableInterval> intMixed;
  private ImmutableList<UnwritableInterval> intEmpty;

  private UnwritableInterval a;
  private IntervalSet aSet;

  @Before
  public void setUp() throws Exception {

    Builder builder = new Builder();
    empty = builder.build();
    builder.add(0.0, 0.0);
    singleton = builder.build();
    builder.add(1.0, 1.0);
    builder.add(2.0, 2.0);
    builder.add(3.0, 3.0);
    singletons = builder.build();

    builder.empty();
    builder.add(0.0, 1.0);
    interval = builder.build();
    builder.add(2.0, 3.0);
    builder.add(4.0, 5.0);
    intervals = builder.build();

    builder.empty();
    builder.add(0.0, 0.0);
    builder.add(1.0, 2.0);
    builder.add(3.0, 3.0);
    builder.add(4.0, 6.0);
    mixed = builder.build();

    intSingleton = create(0.0, 0.0);
    intSingletons = create(0.0, 0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0);
    intInterval = create(0.0, 1.0);
    intIntervals = create(0.0, 1.0, 2.0, 3.0, 4.0, 5.0);
    intMixed = create(0.0, 0.0, 1.0, 2.0, 3.0, 3.0, 4.0, 6.0);
    intEmpty = ImmutableList.of();

    a = new UnwritableInterval(0.0, 3.0);
    aSet = IntervalSet.create(a);
  }

  private static ImmutableList<UnwritableInterval> create(double... startStops) {
    checkArgument(startStops.length % 2 == 0);

    List<UnwritableInterval> list = Lists.newArrayList();
    for (int i = 0; i < startStops.length; i += 2) {
      list.add(new UnwritableInterval(startStops[i], startStops[i + 1]));
    }

    return ImmutableList.copyOf(list);
  }

  @Test
  public void testStaticEmptyField() {
    assertEquals(0, EMPTY.size());
  }

  @Test
  public void testStaticLineField() {
    assertEquals(1, LINE.size());
    assertEquals(-Double.MAX_VALUE, LINE.get(0).getBegin(), 0.0);
    assertEquals(Double.MAX_VALUE, LINE.get(0).getEnd(), 0.0);
  }

  @Test
  public void testHashCode() {
    /*
     * Just assert that two distinct, but equal interval sets have the same hashCode
     */
    Builder builder = new Builder();
    builder.add(1.0, 2.0);
    builder.add(3.0, 5.0);
    IntervalSet test = builder.build();

    builder = new Builder();
    builder.add(3.0, 5.0);
    builder.add(1.0, 2.0);
    IntervalSet alternate = builder.build();

    assertEquals(test.hashCode(), alternate.hashCode());
  }

  @Test
  public void testSize() {
    assertEquals(0, empty.size());
    assertEquals(1, singleton.size());
    assertEquals(1, interval.size());
    assertEquals(4, singletons.size());
    assertEquals(3, intervals.size());
    assertEquals(4, mixed.size());
  }

  @Test
  public void testIsEmpty() {
    assertTrue(empty.isEmpty());
    assertFalse(singleton.isEmpty());
    assertFalse(singletons.isEmpty());
    assertFalse(interval.isEmpty());
    assertFalse(intervals.isEmpty());
    assertFalse(mixed.isEmpty());
  }

  @Test
  public void testGetIntervalBegins() {
    assertEquals(Collections.emptyList(), empty.getIntervalBegins());
    assertEquals(ImmutableList.of(0.0), singleton.getIntervalBegins());
    assertEquals(ImmutableList.of(0.0, 1.0, 2.0, 3.0), singletons.getIntervalBegins());
    assertEquals(ImmutableList.of(0.0), interval.getIntervalBegins());
    assertEquals(ImmutableList.of(0.0, 2.0, 4.0), intervals.getIntervalBegins());
    assertEquals(ImmutableList.of(0.0, 1.0, 3.0, 4.0), mixed.getIntervalBegins());
  }

  @Test
  public void testGetIntervalEnds() {
    assertEquals(Collections.emptyList(), empty.getIntervalEnds());
    assertEquals(ImmutableList.of(0.0), singleton.getIntervalEnds());
    assertEquals(ImmutableList.of(0.0, 1.0, 2.0, 3.0), singletons.getIntervalEnds());
    assertEquals(ImmutableList.of(1.0), interval.getIntervalEnds());
    assertEquals(ImmutableList.of(1.0, 3.0, 5.0), intervals.getIntervalEnds());
    assertEquals(ImmutableList.of(0.0, 2.0, 3.0, 6.0), mixed.getIntervalEnds());
  }

  @Test
  public void testContainsDouble() {

    assertFalse(empty.contains(0.0));

    assertFalse(singleton.contains(-1.0));
    assertTrue(singleton.contains(0.0));
    assertFalse(singleton.contains(-1.0));

    assertFalse(singletons.contains(-0.5));
    assertTrue(singletons.contains(0.0));
    assertFalse(singletons.contains(0.5));
    assertTrue(singletons.contains(1.0));
    assertFalse(singletons.contains(1.5));
    assertTrue(singletons.contains(2.0));
    assertFalse(singletons.contains(2.5));
    assertTrue(singletons.contains(3.0));
    assertFalse(singletons.contains(3.5));

    assertFalse(interval.contains(-0.5));
    assertTrue(interval.contains(0.0));
    assertTrue(interval.contains(0.5));
    assertTrue(interval.contains(1.0));
    assertFalse(interval.contains(1.5));

    assertFalse(intervals.contains(-0.5));
    assertTrue(intervals.contains(0.0));
    assertTrue(intervals.contains(0.5));
    assertTrue(intervals.contains(1.0));
    assertFalse(intervals.contains(1.5));
    assertTrue(intervals.contains(2.0));
    assertTrue(intervals.contains(2.5));
    assertTrue(intervals.contains(3.0));
    assertFalse(intervals.contains(3.5));
    assertTrue(intervals.contains(4.0));
    assertTrue(intervals.contains(4.5));
    assertTrue(intervals.contains(5.0));
    assertFalse(intervals.contains(5.5));

    assertFalse(mixed.contains(-0.5));
    assertTrue(mixed.contains(0.0));
    assertFalse(mixed.contains(0.5));
    assertTrue(mixed.contains(1.0));
    assertTrue(mixed.contains(1.5));
    assertTrue(mixed.contains(2.0));
    assertFalse(mixed.contains(2.5));
    assertTrue(mixed.contains(3.0));
    assertFalse(mixed.contains(3.5));
    assertTrue(mixed.contains(4.0));
    assertTrue(mixed.contains(5.0));
    assertTrue(mixed.contains(6.0));
    assertFalse(mixed.contains(6.5));
  }

  @Test
  public void testGetEnclosingIndexDouble() {

    assertEquals(0, singleton.getEnclosingIndex(0.0));

    assertEquals(0, singletons.getEnclosingIndex(0.0));
    assertEquals(1, singletons.getEnclosingIndex(1.0));
    assertEquals(2, singletons.getEnclosingIndex(2.0));
    assertEquals(3, singletons.getEnclosingIndex(3.0));

    assertEquals(0, interval.getEnclosingIndex(0.0));
    assertEquals(0, interval.getEnclosingIndex(0.5));
    assertEquals(0, interval.getEnclosingIndex(1.0));

    assertEquals(0, intervals.getEnclosingIndex(0.0));
    assertEquals(0, intervals.getEnclosingIndex(0.5));
    assertEquals(0, intervals.getEnclosingIndex(1.0));
    assertEquals(1, intervals.getEnclosingIndex(2.0));
    assertEquals(1, intervals.getEnclosingIndex(2.5));
    assertEquals(1, intervals.getEnclosingIndex(3.0));
    assertEquals(2, intervals.getEnclosingIndex(4.0));
    assertEquals(2, intervals.getEnclosingIndex(4.5));
    assertEquals(2, intervals.getEnclosingIndex(5.0));

    assertEquals(0, mixed.getEnclosingIndex(0.0));
    assertEquals(1, mixed.getEnclosingIndex(1.0));
    assertEquals(1, mixed.getEnclosingIndex(1.5));
    assertEquals(1, mixed.getEnclosingIndex(2.0));
    assertEquals(2, mixed.getEnclosingIndex(3.0));
    assertEquals(3, mixed.getEnclosingIndex(4.0));
    assertEquals(3, mixed.getEnclosingIndex(5.0));
    assertEquals(3, mixed.getEnclosingIndex(6.0));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleValuePreceedsIntervalsException() {
    singleton.getEnclosingIndex(-10000.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleValueOutsideIntervalException() {
    intervals.getEnclosingIndex(1.5);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleValueExceedsIntervalsException() {
    intervals.getEnclosingIndex(100000.0);
  }

  @Test
  public void testOnBoundary() {
    assertFalse(empty.onBoundary(0.0));

    assertFalse(singleton.onBoundary(-1.0));
    assertTrue(singleton.onBoundary(0.0));
    assertFalse(singleton.onBoundary(-1.0));

    assertFalse(singletons.onBoundary(-0.5));
    assertTrue(singletons.onBoundary(0.0));
    assertFalse(singletons.onBoundary(0.5));
    assertTrue(singletons.onBoundary(1.0));
    assertFalse(singletons.onBoundary(1.5));
    assertTrue(singletons.onBoundary(2.0));
    assertFalse(singletons.onBoundary(2.5));
    assertTrue(singletons.onBoundary(3.0));
    assertFalse(singletons.onBoundary(3.5));

    assertFalse(interval.onBoundary(-0.5));
    assertTrue(interval.onBoundary(0.0));
    assertFalse(interval.onBoundary(0.5));
    assertTrue(interval.onBoundary(1.0));
    assertFalse(interval.onBoundary(1.5));

    assertFalse(intervals.onBoundary(-0.5));
    assertTrue(intervals.onBoundary(0.0));
    assertFalse(intervals.onBoundary(0.5));
    assertTrue(intervals.onBoundary(1.0));
    assertFalse(intervals.onBoundary(1.5));
    assertTrue(intervals.onBoundary(2.0));
    assertFalse(intervals.onBoundary(2.5));
    assertTrue(intervals.onBoundary(3.0));
    assertFalse(intervals.onBoundary(3.5));
    assertTrue(intervals.onBoundary(4.0));
    assertFalse(intervals.onBoundary(4.5));
    assertTrue(intervals.onBoundary(5.0));
    assertFalse(intervals.onBoundary(5.5));

    assertFalse(mixed.onBoundary(-0.5));
    assertTrue(mixed.onBoundary(0.0));
    assertFalse(mixed.onBoundary(0.5));
    assertTrue(mixed.onBoundary(1.0));
    assertFalse(mixed.onBoundary(1.5));
    assertTrue(mixed.onBoundary(2.0));
    assertFalse(mixed.onBoundary(2.5));
    assertTrue(mixed.onBoundary(3.0));
    assertFalse(mixed.onBoundary(3.5));
    assertTrue(mixed.onBoundary(4.0));
    assertFalse(mixed.onBoundary(5.0));
    assertTrue(mixed.onBoundary(6.0));
    assertFalse(mixed.onBoundary(6.5));
  }

  @Test
  public void testInInterior() {

    assertFalse(empty.inInterior(0.0));

    assertFalse(singleton.inInterior(-1.0));
    assertFalse(singleton.inInterior(0.0));
    assertFalse(singleton.inInterior(-1.0));

    assertFalse(singletons.inInterior(-0.5));
    assertFalse(singletons.inInterior(0.0));
    assertFalse(singletons.inInterior(0.5));
    assertFalse(singletons.inInterior(1.0));
    assertFalse(singletons.inInterior(1.5));
    assertFalse(singletons.inInterior(2.0));
    assertFalse(singletons.inInterior(2.5));
    assertFalse(singletons.inInterior(3.0));
    assertFalse(singletons.inInterior(3.5));

    assertFalse(interval.inInterior(-0.5));
    assertFalse(interval.inInterior(0.0));
    assertTrue(interval.inInterior(0.5));
    assertFalse(interval.inInterior(1.0));
    assertFalse(interval.inInterior(1.5));

    assertFalse(intervals.inInterior(-0.5));
    assertFalse(intervals.inInterior(0.0));
    assertTrue(intervals.inInterior(0.5));
    assertFalse(intervals.inInterior(1.0));
    assertFalse(intervals.inInterior(1.5));
    assertFalse(intervals.inInterior(2.0));
    assertTrue(intervals.inInterior(2.5));
    assertFalse(intervals.inInterior(3.0));
    assertFalse(intervals.inInterior(3.5));
    assertFalse(intervals.inInterior(4.0));
    assertTrue(intervals.inInterior(4.5));
    assertFalse(intervals.inInterior(5.0));
    assertFalse(intervals.inInterior(5.5));

    assertFalse(mixed.inInterior(-0.5));
    assertFalse(mixed.inInterior(0.0));
    assertFalse(mixed.inInterior(0.5));
    assertFalse(mixed.inInterior(1.0));
    assertTrue(mixed.inInterior(1.5));
    assertFalse(mixed.inInterior(2.0));
    assertFalse(mixed.inInterior(2.5));
    assertFalse(mixed.inInterior(3.0));
    assertFalse(mixed.inInterior(3.5));
    assertFalse(mixed.inInterior(4.0));
    assertTrue(mixed.inInterior(5.0));
    assertFalse(mixed.inInterior(6.0));
    assertFalse(mixed.inInterior(6.5));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testContainsDoubleDoubleException() {
    singleton.contains(10, 5);
  }

  @Test
  public void testContainsDoubleDoubleSingletons() {
    assertFalse(empty.contains(0.0, 0.0));

    assertFalse(singleton.contains(-1.0, -1.0));
    assertTrue(singleton.contains(0.0, 0.0));
    assertFalse(singleton.contains(-1.0, -1.0));

    assertFalse(singletons.contains(-0.5, -0.5));
    assertTrue(singletons.contains(0.0, 0.0));
    assertFalse(singletons.contains(0.5, 0.5));
    assertTrue(singletons.contains(1.0, 1.0));
    assertFalse(singletons.contains(1.5, 1.5));
    assertTrue(singletons.contains(2.0, 2.0));
    assertFalse(singletons.contains(2.5, 2.5));
    assertTrue(singletons.contains(3.0, 3.0));
    assertFalse(singletons.contains(3.5, 3.5));

    assertFalse(interval.contains(-0.5, -0.5));
    assertTrue(interval.contains(0.0, 0.0));
    assertTrue(interval.contains(0.5, 0.5));
    assertTrue(interval.contains(1.0, 1.0));
    assertFalse(interval.contains(1.5, 1.5));

    assertFalse(intervals.contains(-0.5, -0.5));
    assertTrue(intervals.contains(0.0, 0.0));
    assertTrue(intervals.contains(0.5, 0.5));
    assertTrue(intervals.contains(1.0, 1.0));
    assertFalse(intervals.contains(1.5, 1.5));
    assertTrue(intervals.contains(2.0, 2.0));
    assertTrue(intervals.contains(2.5, 2.5));
    assertTrue(intervals.contains(3.0, 3.0));
    assertFalse(intervals.contains(3.5, 3.5));
    assertTrue(intervals.contains(4.0, 4.0));
    assertTrue(intervals.contains(4.5, 4.5));
    assertTrue(intervals.contains(5.0, 5.0));
    assertFalse(intervals.contains(5.5, 5.5));

    assertFalse(mixed.contains(-0.5, -0.5));
    assertTrue(mixed.contains(0.0, 0.0));
    assertFalse(mixed.contains(0.5, 0.5));
    assertTrue(mixed.contains(1.0, 1.0));
    assertTrue(mixed.contains(1.5, 1.5));
    assertTrue(mixed.contains(2.0, 2.0));
    assertFalse(mixed.contains(2.5, 2.5));
    assertTrue(mixed.contains(3.0, 3.0));
    assertFalse(mixed.contains(3.5, 3.5));
    assertTrue(mixed.contains(4.0, 4.0));
    assertTrue(mixed.contains(5.0, 5.0));
    assertTrue(mixed.contains(6.0, 6.0));
    assertFalse(mixed.contains(6.5, 6.5));
  }

  @Test
  public void testContainsDoubleDouble() {

    assertFalse(singleton.contains(-1.0, -1.0));
    assertTrue(singleton.contains(0.0, 0.0));
    assertFalse(singleton.contains(1.0, 1.0));

    assertFalse(singletons.contains(-0.5, -.5));
    assertTrue(singletons.contains(0.0, 0.0));
    assertFalse(singletons.contains(0.5, 0.5));
    assertTrue(singletons.contains(1.0, 1.0));
    assertFalse(singletons.contains(1.5, 1.5));
    assertTrue(singletons.contains(2.0, 2.0));
    assertFalse(singletons.contains(2.5, 2.5));
    assertTrue(singletons.contains(3.0, 3.0));
    assertFalse(singletons.contains(3.5, 3.5));

    assertFalse(interval.contains(-1.0, -0.5));
    assertFalse(interval.contains(-1.0, 0.0));
    assertFalse(interval.contains(-0.5, 0.5));
    assertTrue(interval.contains(0.0, 1.0));
    assertTrue(interval.contains(0.1, 0.9));
    assertFalse(interval.contains(0.5, 1.5));
    assertFalse(interval.contains(1.0, 1.5));
    assertFalse(interval.contains(1.5, 2.0));

    assertFalse(intervals.contains(-1.0, -0.5));
    assertFalse(intervals.contains(-1.0, 0.0));
    assertFalse(intervals.contains(-0.5, 0.5));
    assertTrue(intervals.contains(0.0, 1.0));
    assertTrue(intervals.contains(0.1, 0.9));
    assertFalse(intervals.contains(0.5, 1.5));
    assertFalse(intervals.contains(1.0, 1.5));
    assertFalse(intervals.contains(1.5, 1.9));
    assertFalse(intervals.contains(1.2, 1.7));
    assertFalse(intervals.contains(1.7, 2.0));
    assertFalse(intervals.contains(1.7, 2.5));
    assertTrue(intervals.contains(2.0, 3.0));
    assertTrue(intervals.contains(2.1, 2.9));
    assertFalse(intervals.contains(2.5, 3.5));
    assertFalse(intervals.contains(3.0, 3.5));
    assertFalse(intervals.contains(3.5, 3.9));
    assertFalse(intervals.contains(3.2, 3.7));
    assertFalse(intervals.contains(3.7, 4.0));
    assertFalse(intervals.contains(3.7, 4.5));
    assertTrue(intervals.contains(4.0, 5.0));
    assertTrue(intervals.contains(4.1, 4.9));
    assertFalse(intervals.contains(4.5, 5.5));
    assertFalse(intervals.contains(5.0, 5.5));
    assertFalse(intervals.contains(5.5, 5.9));
    assertFalse(intervals.contains(-10, 10));

  }

  @Test
  public void testGetEnclosingIndexDoubleDoubleSingletons() {

    assertEquals(0, singleton.getEnclosingIndex(0.0, 0.0));

    assertEquals(0, singletons.getEnclosingIndex(0.0, 0.0));
    assertEquals(1, singletons.getEnclosingIndex(1.0, 1.0));
    assertEquals(2, singletons.getEnclosingIndex(2.0, 2.0));
    assertEquals(3, singletons.getEnclosingIndex(3.0, 3.0));

    assertEquals(0, interval.getEnclosingIndex(0.0, 0.0));
    assertEquals(0, interval.getEnclosingIndex(0.5, 0.5));
    assertEquals(0, interval.getEnclosingIndex(1.0, 1.0));

    assertEquals(0, intervals.getEnclosingIndex(0.0, 0.0));
    assertEquals(0, intervals.getEnclosingIndex(0.5, 0.5));
    assertEquals(0, intervals.getEnclosingIndex(1.0, 1.0));
    assertEquals(1, intervals.getEnclosingIndex(2.0, 2.0));
    assertEquals(1, intervals.getEnclosingIndex(2.5, 2.5));
    assertEquals(1, intervals.getEnclosingIndex(3.0, 3.0));
    assertEquals(2, intervals.getEnclosingIndex(4.0, 4.0));
    assertEquals(2, intervals.getEnclosingIndex(4.5, 4.5));
    assertEquals(2, intervals.getEnclosingIndex(5.0, 5.0));

    assertEquals(0, mixed.getEnclosingIndex(0.0, 0.0));
    assertEquals(1, mixed.getEnclosingIndex(1.0, 1.0));
    assertEquals(1, mixed.getEnclosingIndex(1.5, 1.5));
    assertEquals(1, mixed.getEnclosingIndex(2.0, 2.0));
    assertEquals(2, mixed.getEnclosingIndex(3.0, 3.0));
    assertEquals(3, mixed.getEnclosingIndex(4.0, 4.0));
    assertEquals(3, mixed.getEnclosingIndex(5.0, 5.0));
    assertEquals(3, mixed.getEnclosingIndex(6.0, 6.0));

  }

  @Test
  public void testGetEnclosingIndexDoubleDouble() {

    assertEquals(0, interval.getEnclosingIndex(0.0, 1.0));
    assertEquals(0, interval.getEnclosingIndex(0.1, 1.0));
    assertEquals(0, interval.getEnclosingIndex(0.0, 0.9));
    assertEquals(0, interval.getEnclosingIndex(0.1, 0.9));

    assertEquals(0, intervals.getEnclosingIndex(0.0, 1.0));
    assertEquals(0, intervals.getEnclosingIndex(0.1, 1.0));
    assertEquals(0, intervals.getEnclosingIndex(0.0, 0.9));
    assertEquals(0, intervals.getEnclosingIndex(0.1, 0.9));
    assertEquals(1, intervals.getEnclosingIndex(2.0, 3.0));
    assertEquals(1, intervals.getEnclosingIndex(2.1, 3.0));
    assertEquals(1, intervals.getEnclosingIndex(2.0, 2.9));
    assertEquals(1, intervals.getEnclosingIndex(2.1, 2.9));
    assertEquals(2, intervals.getEnclosingIndex(4.0, 5.0));
    assertEquals(2, intervals.getEnclosingIndex(4.1, 5.0));
    assertEquals(2, intervals.getEnclosingIndex(4.0, 4.9));
    assertEquals(2, intervals.getEnclosingIndex(4.1, 4.9));

    assertEquals(0, mixed.getEnclosingIndex(0.0, 0.0));
    assertEquals(1, mixed.getEnclosingIndex(1.0, 2.0));
    assertEquals(1, mixed.getEnclosingIndex(1.1, 2.0));
    assertEquals(1, mixed.getEnclosingIndex(1.0, 1.9));
    assertEquals(1, mixed.getEnclosingIndex(1.1, 1.9));
    assertEquals(2, mixed.getEnclosingIndex(3.0, 3.0));
    assertEquals(3, mixed.getEnclosingIndex(4.0, 6.0));
    assertEquals(3, mixed.getEnclosingIndex(4.0, 5.9));
    assertEquals(3, mixed.getEnclosingIndex(4.1, 6.0));
    assertEquals(3, mixed.getEnclosingIndex(4.1, 5.9));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleDoubleIntervalPrecedesStart() {
    intervals.getEnclosingIndex(-1.0, 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleDoubleIntervalLiesEntirelyOutside() {
    intervals.getEnclosingIndex(1.2, 1.5);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleDoubleIntervalLiesPartiallyOutsideLow() {
    intervals.getEnclosingIndex(1.2, 2.4);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleDoubleIntervalLiesPartiallyOutsideHigh() {
    intervals.getEnclosingIndex(0.5, 1.4);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexDoubleDoubleIntervalExceedsEnd() {
    intervals.getEnclosingIndex(5.2, 5.6);
  }

  @Test
  public void testContainsUnwritableInterval() {

    assertFalse(singleton.contains(new UnwritableInterval(-1.0, -1.0)));
    assertTrue(singleton.contains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(singleton.contains(new UnwritableInterval(-1.0, -1.0)));

    assertFalse(singletons.contains(new UnwritableInterval(-0.5, -0.5)));
    assertTrue(singletons.contains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(singletons.contains(new UnwritableInterval(0.5, 0.5)));
    assertTrue(singletons.contains(new UnwritableInterval(1.0, 1.0)));
    assertFalse(singletons.contains(new UnwritableInterval(1.5, 1.5)));
    assertTrue(singletons.contains(new UnwritableInterval(2.0, 2.0)));
    assertFalse(singletons.contains(new UnwritableInterval(2.5, 2.5)));
    assertTrue(singletons.contains(new UnwritableInterval(3.0, 3.0)));
    assertFalse(singletons.contains(new UnwritableInterval(3.5, 3.5)));

    assertFalse(interval.contains(new UnwritableInterval(-0.5, -0.5)));
    assertTrue(interval.contains(new UnwritableInterval(0.0, 0.0)));
    assertTrue(interval.contains(new UnwritableInterval(0.5, 0.5)));
    assertTrue(interval.contains(new UnwritableInterval(1.0, 1.0)));
    assertFalse(interval.contains(new UnwritableInterval(1.5, 1.5)));

    assertFalse(intervals.contains(new UnwritableInterval(-0.5, -0.5)));
    assertTrue(intervals.contains(new UnwritableInterval(0.0, 0.0)));
    assertTrue(intervals.contains(new UnwritableInterval(0.5, 0.5)));
    assertTrue(intervals.contains(new UnwritableInterval(1.0, 1.0)));
    assertFalse(intervals.contains(new UnwritableInterval(1.5, 1.5)));
    assertTrue(intervals.contains(new UnwritableInterval(2.0, 2.0)));
    assertTrue(intervals.contains(new UnwritableInterval(2.5, 2.5)));
    assertTrue(intervals.contains(new UnwritableInterval(3.0, 3.0)));
    assertFalse(intervals.contains(new UnwritableInterval(3.5, 3.5)));
    assertTrue(intervals.contains(new UnwritableInterval(4.0, 4.0)));
    assertTrue(intervals.contains(new UnwritableInterval(4.5, 4.5)));
    assertTrue(intervals.contains(new UnwritableInterval(5.0, 5.0)));
    assertFalse(intervals.contains(new UnwritableInterval(5.5, 5.5)));

    assertFalse(mixed.contains(new UnwritableInterval(-0.5, -0.5)));
    assertTrue(mixed.contains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(mixed.contains(new UnwritableInterval(0.5, 0.5)));
    assertTrue(mixed.contains(new UnwritableInterval(1.0, 1.0)));
    assertTrue(mixed.contains(new UnwritableInterval(1.5, 1.5)));
    assertTrue(mixed.contains(new UnwritableInterval(2.0, 2.0)));
    assertFalse(mixed.contains(new UnwritableInterval(2.5, 2.5)));
    assertTrue(mixed.contains(new UnwritableInterval(3.0, 3.0)));
    assertFalse(mixed.contains(new UnwritableInterval(3.5, 3.5)));
    assertTrue(mixed.contains(new UnwritableInterval(4.0, 4.0)));
    assertTrue(mixed.contains(new UnwritableInterval(5.0, 5.0)));
    assertTrue(mixed.contains(new UnwritableInterval(6.0, 6.0)));
    assertFalse(mixed.contains(new UnwritableInterval(6.5, 6.5)));

    assertFalse(singleton.contains(new UnwritableInterval(-1.0, -1.0)));
    assertTrue(singleton.contains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(singleton.contains(new UnwritableInterval(1.0, 1.0)));

    assertFalse(singletons.contains(new UnwritableInterval(-0.5, -.5)));
    assertTrue(singletons.contains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(singletons.contains(new UnwritableInterval(0.5, 0.5)));
    assertTrue(singletons.contains(new UnwritableInterval(1.0, 1.0)));
    assertFalse(singletons.contains(new UnwritableInterval(1.5, 1.5)));
    assertTrue(singletons.contains(new UnwritableInterval(2.0, 2.0)));
    assertFalse(singletons.contains(new UnwritableInterval(2.5, 2.5)));
    assertTrue(singletons.contains(new UnwritableInterval(3.0, 3.0)));
    assertFalse(singletons.contains(new UnwritableInterval(3.5, 3.5)));

    assertFalse(interval.contains(new UnwritableInterval(-1.0, -0.5)));
    assertFalse(interval.contains(new UnwritableInterval(-1.0, 0.0)));
    assertFalse(interval.contains(new UnwritableInterval(-0.5, 0.5)));
    assertTrue(interval.contains(new UnwritableInterval(0.0, 1.0)));
    assertTrue(interval.contains(new UnwritableInterval(0.1, 0.9)));
    assertFalse(interval.contains(new UnwritableInterval(0.5, 1.5)));
    assertFalse(interval.contains(new UnwritableInterval(1.0, 1.5)));
    assertFalse(interval.contains(new UnwritableInterval(1.5, 2.0)));

    assertFalse(intervals.contains(new UnwritableInterval(-1.0, -0.5)));
    assertFalse(intervals.contains(new UnwritableInterval(-1.0, 0.0)));
    assertFalse(intervals.contains(new UnwritableInterval(-0.5, 0.5)));
    assertTrue(intervals.contains(new UnwritableInterval(0.0, 1.0)));
    assertTrue(intervals.contains(new UnwritableInterval(0.1, 0.9)));
    assertFalse(intervals.contains(new UnwritableInterval(0.5, 1.5)));
    assertFalse(intervals.contains(new UnwritableInterval(1.0, 1.5)));
    assertFalse(intervals.contains(new UnwritableInterval(1.5, 1.9)));
    assertFalse(intervals.contains(new UnwritableInterval(1.2, 1.7)));
    assertFalse(intervals.contains(new UnwritableInterval(1.7, 2.0)));
    assertFalse(intervals.contains(new UnwritableInterval(1.7, 2.5)));
    assertTrue(intervals.contains(new UnwritableInterval(2.0, 3.0)));
    assertTrue(intervals.contains(new UnwritableInterval(2.1, 2.9)));
    assertFalse(intervals.contains(new UnwritableInterval(2.5, 3.5)));
    assertFalse(intervals.contains(new UnwritableInterval(3.0, 3.5)));
    assertFalse(intervals.contains(new UnwritableInterval(3.5, 3.9)));
    assertFalse(intervals.contains(new UnwritableInterval(3.2, 3.7)));
    assertFalse(intervals.contains(new UnwritableInterval(3.7, 4.0)));
    assertFalse(intervals.contains(new UnwritableInterval(3.7, 4.5)));
    assertTrue(intervals.contains(new UnwritableInterval(4.0, 5.0)));
    assertTrue(intervals.contains(new UnwritableInterval(4.1, 4.9)));
    assertFalse(intervals.contains(new UnwritableInterval(4.5, 5.5)));
    assertFalse(intervals.contains(new UnwritableInterval(5.0, 5.5)));
    assertFalse(intervals.contains(new UnwritableInterval(5.5, 5.9)));
    assertFalse(intervals.contains(new UnwritableInterval(-10, 10)));

  }

  @Test
  public void testGetEnclosingIndexUnwritableIntervalSingletons() {

    assertEquals(0, singleton.getEnclosingIndex(new UnwritableInterval(0.0, 0.0)));

    assertEquals(0, singletons.getEnclosingIndex(new UnwritableInterval(0.0, 0.0)));
    assertEquals(1, singletons.getEnclosingIndex(new UnwritableInterval(1.0, 1.0)));
    assertEquals(2, singletons.getEnclosingIndex(new UnwritableInterval(2.0, 2.0)));
    assertEquals(3, singletons.getEnclosingIndex(new UnwritableInterval(3.0, 3.0)));

    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(0.0, 0.0)));
    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(0.5, 0.5)));
    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(1.0, 1.0)));

    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(0.0, 0.0)));
    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(0.5, 0.5)));
    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(1.0, 1.0)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(2.0, 2.0)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(2.5, 2.5)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(3.0, 3.0)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(4.0, 4.0)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(4.5, 4.5)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(5.0, 5.0)));

    assertEquals(0, mixed.getEnclosingIndex(new UnwritableInterval(0.0, 0.0)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(1.0, 1.0)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(1.5, 1.5)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(2.0, 2.0)));
    assertEquals(2, mixed.getEnclosingIndex(new UnwritableInterval(3.0, 3.0)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(4.0, 4.0)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(5.0, 5.0)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(6.0, 6.0)));

  }

  @Test
  public void testGetEnclosingIndexUnwritableInterval() {

    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(0.0, 1.0)));
    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(0.1, 1.0)));
    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(0.0, 0.9)));
    assertEquals(0, interval.getEnclosingIndex(new UnwritableInterval(0.1, 0.9)));

    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(0.0, 1.0)));
    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(0.1, 1.0)));
    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(0.0, 0.9)));
    assertEquals(0, intervals.getEnclosingIndex(new UnwritableInterval(0.1, 0.9)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(2.0, 3.0)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(2.1, 3.0)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(2.0, 2.9)));
    assertEquals(1, intervals.getEnclosingIndex(new UnwritableInterval(2.1, 2.9)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(4.0, 5.0)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(4.1, 5.0)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(4.0, 4.9)));
    assertEquals(2, intervals.getEnclosingIndex(new UnwritableInterval(4.1, 4.9)));

    assertEquals(0, mixed.getEnclosingIndex(new UnwritableInterval(0.0, 0.0)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(1.0, 2.0)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(1.1, 2.0)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(1.0, 1.9)));
    assertEquals(1, mixed.getEnclosingIndex(new UnwritableInterval(1.1, 1.9)));
    assertEquals(2, mixed.getEnclosingIndex(new UnwritableInterval(3.0, 3.0)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(4.0, 6.0)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(4.0, 5.9)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(4.1, 6.0)));
    assertEquals(3, mixed.getEnclosingIndex(new UnwritableInterval(4.1, 5.9)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexUnwritableIntervalIntervalPrecedesStart() {
    intervals.getEnclosingIndex(new UnwritableInterval(-1.0, 0.0));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexUnwritableIntervalIntervalLiesEntirelyOutside() {
    intervals.getEnclosingIndex(new UnwritableInterval(1.2, 1.5));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexUnwritableIntervalIntervalLiesPartiallyOutsideLow() {
    intervals.getEnclosingIndex(new UnwritableInterval(1.2, 2.4));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexUnwritableIntervalIntervalLiesPartiallyOutsideHigh() {
    intervals.getEnclosingIndex(new UnwritableInterval(0.5, 1.4));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetEnclosingIndexUnwritableIntervalIntervalExceedsEnd() {
    intervals.getEnclosingIndex(new UnwritableInterval(5.2, 5.6));
  }

  @Test
  public void testIterator() {
    assertTrue(Iterators.elementsEqual(intEmpty.iterator(), empty.iterator()));
    assertTrue(Iterators.elementsEqual(intSingleton.iterator(), singleton.iterator()));
    assertTrue(Iterators.elementsEqual(intSingletons.iterator(), singletons.iterator()));
    assertTrue(Iterators.elementsEqual(intInterval.iterator(), interval.iterator()));
    assertTrue(Iterators.elementsEqual(intIntervals.iterator(), intervals.iterator()));
    assertTrue(Iterators.elementsEqual(intMixed.iterator(), mixed.iterator()));
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetLowerIndexException() {
    singleton.get(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetUpperIndexException() {
    singleton.get(1);
  }

  @Test
  public void testGet() {

    assertEquals(intSingleton.get(0), singleton.get(0));

    for (int i = 0; i < singletons.size(); i++) {
      assertEquals(intSingletons.get(i), singletons.get(i));
    }

    assertEquals(intInterval.get(0), intervals.get(0));

    for (int i = 0; i < intervals.size(); i++) {
      assertEquals(intIntervals.get(0), intervals.get(0));
    }

    for (int i = 0; i < mixed.size(); i++) {
      assertEquals(intMixed.get(0), mixed.get(0));
    }

  }

  @Test
  public void testContainsIntervalSet() {
    assertTrue(singleton.contains(singleton));
    assertFalse(singleton.contains(singletons));
    assertFalse(singleton.contains(interval));
    assertFalse(singleton.contains(intervals));
    assertFalse(singleton.contains(mixed));

    assertTrue(singletons.contains(singleton));
    assertTrue(singletons.contains(singletons));
    assertFalse(singletons.contains(interval));
    assertFalse(singletons.contains(intervals));
    assertFalse(singletons.contains(mixed));

    assertTrue(interval.contains(singleton));
    assertFalse(interval.contains(singletons));
    assertTrue(interval.contains(interval));
    assertFalse(interval.contains(intervals));
    assertFalse(interval.contains(mixed));

    assertTrue(intervals.contains(singleton));
    assertTrue(intervals.contains(singletons));
    assertTrue(intervals.contains(interval));
    assertTrue(intervals.contains(intervals));
    assertFalse(intervals.contains(mixed));

    assertTrue(mixed.contains(singleton));
    assertTrue(mixed.contains(singletons));
    assertFalse(mixed.contains(interval));
    assertFalse(mixed.contains(intervals));
    assertTrue(mixed.contains(mixed));
  }

  @Test
  public void testProperContains() {
    assertFalse(singleton.properContains(singleton));
    assertFalse(singleton.properContains(singletons));
    assertFalse(singleton.properContains(interval));
    assertFalse(singleton.properContains(intervals));
    assertFalse(singleton.properContains(mixed));

    assertTrue(singletons.properContains(singleton));
    assertFalse(singletons.properContains(singletons));
    assertFalse(singletons.properContains(interval));
    assertFalse(singletons.properContains(intervals));
    assertFalse(singletons.properContains(mixed));

    assertTrue(interval.properContains(singleton));
    assertFalse(interval.properContains(singletons));
    assertFalse(interval.properContains(interval));
    assertFalse(interval.properContains(intervals));
    assertFalse(interval.properContains(mixed));

    assertTrue(intervals.properContains(singleton));
    assertTrue(intervals.properContains(singletons));
    assertTrue(intervals.properContains(interval));
    assertFalse(intervals.properContains(intervals));
    assertFalse(intervals.properContains(mixed));

    assertTrue(mixed.properContains(singleton));
    assertTrue(mixed.properContains(singletons));
    assertFalse(mixed.properContains(interval));
    assertFalse(mixed.properContains(intervals));
    assertFalse(mixed.properContains(mixed));
  }

  @Test
  public void testGetLengthTotalEmptySet() {
    assertEquals(0.0, empty.getLengthTotal(), 0.0);
  }

  @Test
  public void testGetLengthTotal() {
    assertEquals(0, singleton.getLengthTotal(), 0.0);
    assertEquals(0, singletons.getLengthTotal(), 0.0);
    assertEquals(1, interval.getLengthTotal(), 0.0);
    assertEquals(3, intervals.getLengthTotal(), 0.0);
    assertEquals(3, mixed.getLengthTotal(), 0.0);
  }

  @Test(expected = IllegalStateException.class)
  public void testGetLengthAverageException() {
    empty.getLengthAverage();
  }

  @Test
  public void testGetLengthAverage() {
    assertEquals(0, singleton.getLengthAverage(), 0.0);
    assertEquals(0, singletons.getLengthAverage(), 0.0);
    assertEquals(1, interval.getLengthAverage(), 0.0);
    assertEquals(1, intervals.getLengthAverage(), 0.0);
    assertEquals(0.75, mixed.getLengthAverage(), 0.0);
  }

  @Test(expected = IllegalStateException.class)
  public void testGetLengthStandardDeviationException() {
    empty.getLengthStandardDeviation();
  }

  @Test
  public void testGetLengthStandardDeviation() {
    assertEquals(0, singleton.getLengthStandardDeviation(), 0.0);
    assertEquals(0, singletons.getLengthStandardDeviation(), 0.0);
    assertEquals(0, interval.getLengthStandardDeviation(), 0.0);
    assertEquals(0, intervals.getLengthStandardDeviation(), 0.0);
    assertEquals(0.8291561975888498, mixed.getLengthStandardDeviation(), 0.0);
  }

  @Test(expected = IllegalStateException.class)
  public void testGetLengthMinimumIndexException() {
    empty.getLengthMinimumIndex();
  }

  @Test
  public void testGetLengthMinimumIndex() {
    assertEquals(0, singleton.getLengthMinimumIndex());
    assertEquals(0, singletons.getLengthMinimumIndex());
    assertEquals(0, interval.getLengthMinimumIndex());
    assertEquals(0, intervals.getLengthMinimumIndex());
    assertEquals(0, mixed.getLengthMinimumIndex());
  }

  @Test(expected = IllegalStateException.class)
  public void testGetLengthMaximumIndexException() {
    empty.getLengthMaximumIndex();
  }

  @Test
  public void testGetLengthMaximumIndex() {
    assertEquals(0, singleton.getLengthMaximumIndex());
    assertEquals(0, singletons.getLengthMaximumIndex());
    assertEquals(0, interval.getLengthMaximumIndex());
    assertEquals(0, intervals.getLengthMaximumIndex());
    assertEquals(3, mixed.getLengthMaximumIndex());
  }

  @Test
  public void testGetLengthMinimumEmptySet() {
    assertEquals(0, empty.getLengthMinimum(), 0.0);
  }

  @Test
  public void testGetLengthMinimum() {
    assertEquals(0, singleton.getLengthMinimum(), 0.0);
    assertEquals(0, singletons.getLengthMinimum(), 0.0);
    assertEquals(1, interval.getLengthMinimum(), 0.0);
    assertEquals(1, intervals.getLengthMinimum(), 0.0);
    assertEquals(0, mixed.getLengthMinimum(), 0.0);
  }

  @Test
  public void testGetLengthMaximumEmptySet() {
    assertEquals(0, empty.getLengthMaximum(), 0.0);
  }

  @Test
  public void testGetLengthMaximum() {
    assertEquals(0, singleton.getLengthMaximum(), 0.0);
    assertEquals(0, singletons.getLengthMaximum(), 0.0);
    assertEquals(1, interval.getLengthMaximum(), 0.0);
    assertEquals(1, intervals.getLengthMaximum(), 0.0);
    assertEquals(2, mixed.getLengthMaximum(), 0.0);
  }

  @Test
  public void testGetBoundingInterval() {
    assertEquals(new UnwritableInterval(0.0, 0.0), singleton.getBoundingInterval());
    assertEquals(new UnwritableInterval(0.0, 3.0), singletons.getBoundingInterval());
    assertEquals(new UnwritableInterval(0.0, 1.0), interval.getBoundingInterval());
    assertEquals(new UnwritableInterval(0.0, 5.0), intervals.getBoundingInterval());
    assertEquals(new UnwritableInterval(0.0, 6.0), mixed.getBoundingInterval());
  }

  @Test(expected = IllegalStateException.class)
  public void testGetBoundingIntervalEmptySetStateException() {
    empty.getBoundingInterval();
  }

  @Test
  public void testToString() {
    /*
     * We're just dumping out the contents of the list internally for now, just check that is
     * exactly what's going on here.
     */
    assertEquals(intMixed.toString(), mixed.toString());
  }

  @Test
  public void testEqualsObject() {
    assertFalse(mixed.equals("String"));
    assertFalse(mixed.equals(null));
    assertTrue(mixed.equals(IntervalSet.create(intMixed)));
    assertFalse(mixed.equals(empty));
    assertFalse(mixed.equals(singleton));
    assertFalse(mixed.equals(singletons));
    assertFalse(mixed.equals(interval));
    assertFalse(mixed.equals(intervals));
  }

  @Test
  public void testBuilder() {
    /*
     * Just check that the builder is empty.
     */
    assertEquals(EMPTY, builder().build());
  }

  @Test
  public void testBuilderInt() {
    /*
     * Just check that the builder is empty.
     */
    assertEquals(EMPTY, builder(10).build());
  }

  @Test
  public void testBuilderIntervalSet() {
    /*
     * Check that the builder produces the appropriate state.
     */
    IntervalSet actual = builder(mixed).build();
    assertNotSame(mixed, actual);
    assertEquals(mixed, actual);
  }

  @Test
  public void testCreateUnwritableInterval() {

    Interval test = new Interval(interval.get(0));
    IntervalSet actual = IntervalSet.create(test);
    assertEquals(interval.get(0), test);
    assertEquals(interval, actual);

    test.set(-10, 10);
    assertEquals(interval, actual);
  }

  @Test
  public void testCreateIterableOfQextendsUnwritableInterval() {
    IntervalSet actual = IntervalSet.create(intEmpty);
    assertSame(actual, empty);

    actual = IntervalSet.create(intSingleton);
    assertNotSame(actual, singleton);
    assertEquals(actual, singleton);

    actual = IntervalSet.create(intSingletons);
    assertNotSame(actual, singletons);
    assertEquals(actual, singletons);

    actual = IntervalSet.create(intInterval);
    assertNotSame(actual, interval);
    assertEquals(actual, interval);

    actual = IntervalSet.create(intIntervals);
    assertNotSame(actual, intervals);
    assertEquals(actual, intervals);

    actual = IntervalSet.create(intMixed);
    assertNotSame(actual, mixed);
    assertEquals(actual, mixed);
  }

  @Test
  public void testCreateDoubleArray() {
    IntervalSet actual = IntervalSet.create(0.0, 0.0, 1.0, 1.0, 2.0, 2.0, 3.0, 3.0);
    assertNotSame(actual, singletons);
    assertEquals(actual, singletons);
  }

  @Test
  public void testCreateUnwritableIntervalArray() {
    IntervalSet actual =
        IntervalSet.create(new UnwritableInterval(0.0, 0.0), new UnwritableInterval(1.0, 1.0),
            new UnwritableInterval(2.0, 2.0), new UnwritableInterval(3.0, 3.0));
    assertNotSame(actual, singletons);
    assertEquals(actual, singletons);
  }

  /*
   * For the remaining convenience methods (that are internally using the already tested builder)
   * just kick the tires.
   */

  @Test
  public void testUnion() {
    assertEquals(mixed.union(intervals), builder(intervals).union(mixed).build());
  }

  @Test
  public void testUnionInterval() {
    assertEquals(mixed.union(a), builder(mixed).union(aSet).build());
  }

  @Test
  public void testIntersect() {
    assertEquals(intervals.intersect(interval), builder(interval).intersect(intervals).build());
  }

  @Test
  public void testIntersectInterval() {
    assertEquals(mixed.intersect(a), builder(mixed).intersect(aSet).build());
  }

  @Test
  public void testDifference() {
    assertEquals(mixed.difference(intervals), builder(mixed).difference(intervals).build());
  }

  @Test
  public void testDifferenceInterval() {
    assertEquals(mixed.difference(a), builder(mixed).difference(aSet).build());
  }

  @Test
  public void testFilter() {
    Predicate<UnwritableInterval> filter = Predicates.alwaysFalse();
    assertEquals(mixed.filter(filter), EMPTY);
  }

  @Test
  public void testComplement() {
    assertEquals(mixed.complement(), builder(mixed).complement().build());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testComplementAgainstDoubleDoubleException() {
    mixed.complementAgainst(10, 5);
  }

  @Test
  public void testComplementAgainstDoubleDouble() {
    assertEquals(mixed.complementAgainst(-20, 20),
        builder(mixed).complementAgainst(-20, 20).build());
  }

  @Test
  public void testComplementAgainstUnwritableInterval() {
    assertEquals(mixed.complementAgainst(new UnwritableInterval(-20, 20)),
        builder(mixed).complementAgainst(-20, 20).build());
  }

  @Test
  public void testExpandDouble() {
    assertEquals(mixed.expand(0.5), builder(mixed).expand(0.5).build());
  }

  @Test
  public void testExpandDoubleDouble() {
    assertEquals(mixed.expand(0.5, 0), builder(mixed).expand(0.5, 0).build());
  }

  @Test
  public void testContractDouble() {
    assertEquals(mixed.contract(0.5), builder(mixed).contract(0.5).build());
  }

  @Test
  public void testContractDoubleDouble() {
    assertEquals(mixed.contract(0, 0.5), builder(mixed).contract(0, 0.5).build());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testFillGapsException() {
    mixed.fillGaps(-1.0);
  }

  @Test
  public void testFillGaps() {
    assertEquals(mixed.fillGaps(1.0), builder(mixed).fillGaps(1.0).build());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRemoveIntervalsException() {
    mixed.removeIntervals(-1.0);
  }

  @Test
  public void testRemoveIntervals() {
    assertEquals(mixed.removeIntervals(1.0), builder().add(4, 6).build());
  }

  @Test
  public void testAddUnwritableInterval() {
    assertEquals(mixed.add(new UnwritableInterval(2, 3)), builder(mixed).add(2, 3).build());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAddDoubleDoubleException() {
    mixed.add(10, 5);
  }

  @Test
  public void testAddDoubleDouble() {
    assertEquals(mixed.add(2, 3), builder(mixed).add(2, 3).build());
  }

  @Test
  public void testGetLength() {
    assertEquals(intIntervals.size(), intervals.size());
    assertEquals(intervals.size(), intervals.size());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordNegativeIndexException() {
    intervals.get(-1, new Interval());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordGreaterThanEndException() {
    intervals.get(intervals.size(), new Interval());
  }

  @Test
  public void testGetRecord() {
    int index = 0;
    for (UnwritableInterval interval : intIntervals) {
      Interval buffer = new Interval();
      Interval result = intervals.get(index++, buffer);
      assertSame(result, buffer);
      assertEquals(interval, result);
    }
  }
}
