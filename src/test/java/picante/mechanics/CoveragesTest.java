package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.List;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import picante.junit.AssertTools;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

public class CoveragesTest {

  private static double FUZZY_TOL = 1e-10;

  @Before
  public void setUp() throws Exception {}

  @Test(expected = IllegalArgumentException.class)
  public void testCreateIntervalSetEmptySetException() {
    Coverages.create(IntervalSet.EMPTY);
  }

  @Test
  public void testCreateIntervalSet() {
    Coverage coverage = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60));

    Interval buffer = new Interval();

    Interval result = coverage.getBoundingInterval(buffer);
    assertSame(result, buffer);
    assertTrue(result.closedContains(0, 60));

    result = coverage.getBracketingInterval(0.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(0, 10), result);

    result = coverage.getBracketingInterval(5.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(0, 10), result);

    result = coverage.getBracketingInterval(10.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(0, 10), result);

    result = coverage.getBracketingInterval(20.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(20, 30), result);

    result = coverage.getBracketingInterval(25.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(20, 30), result);

    result = coverage.getBracketingInterval(30.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(20, 30), result);

    result = coverage.getBracketingInterval(40, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(40, 40), buffer);

    result = coverage.getBracketingInterval(50.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(50, 60), result);

    result = coverage.getBracketingInterval(55.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(50, 60), result);

    result = coverage.getBracketingInterval(60.0, buffer);
    assertSame(result, buffer);
    assertEquals(new UnwritableInterval(50, 60), result);

  }

  @Test
  public void testTightBoundary() {

    Coverage coverage = Coverages.create(0, 100);
    assertEquals(new UnwritableInterval(0, 100), Coverages.tightBoundary(coverage, new Interval()));
    Coverage delegate =
        Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60, 70, 100));

    coverage = new BoundaryDelegate(delegate, -10, 100);
    assertEquals(new UnwritableInterval(0, 100), Coverages.tightBoundary(coverage, new Interval()));

    coverage = new BoundaryDelegate(delegate, 0, 110);
    assertEquals(new UnwritableInterval(0, 100), Coverages.tightBoundary(coverage, new Interval()));

    coverage = new BoundaryDelegate(delegate, -10, 110);
    assertEquals(new UnwritableInterval(0, 100), Coverages.tightBoundary(coverage, new Interval()));

  }

  @Test
  public void testSnapshotToIntervalSetSnapshotCoverageIntervalSetRetrieval() {
    IntervalSet set = IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60);
    Coverage setCoverage = new IntervalSetCoverage(set);

    assertSame(set, Coverages.snapshotToIntervalSet(setCoverage));
  }

  @Test
  public void testSnapshotToIntervalSetSnapshotCoverage() {

    /*
     * This will prevent the instanceof code from detecting that the class is in fact an interval
     * set coverage instance and force the usage of the builder.
     */
    IntervalSet set = IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60);
    Coverage coverage = new BoundaryDelegate(Coverages.create(set), -10, 70);

    IntervalSet actual = Coverages.snapshotToIntervalSet(coverage);

    assertNotSame(set, actual);
    assertEquals(set, actual);

  }

  @Test
  public void testCreateSnapshotUnwritableInterval() {

    Interval interval = new Interval(0, 10);

    Coverage coverage = Coverages.createSnapshot(interval);

    assertTrue(coverage.getBoundingInterval(new Interval()).closedContains(interval));

    assertEquals(interval, coverage.getBracketingInterval(0.0, new Interval()));
    assertEquals(interval, coverage.getBracketingInterval(5.0, new Interval()));
    assertEquals(interval, coverage.getBracketingInterval(10.0, new Interval()));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateDoubleDoubleException() {
    Coverages.create(10, 0);
  }

  @Test
  public void testCreateDoubleDouble() {
    Interval interval = new Interval(0, 10);

    Coverage coverage = Coverages.create(interval.getBegin(), interval.getEnd());

    assertTrue(coverage.getBoundingInterval(new Interval()).closedContains(interval));

    assertEquals(interval, coverage.getBracketingInterval(0.0, new Interval()));
    assertEquals(interval, coverage.getBracketingInterval(5.0, new Interval()));
    assertEquals(interval, coverage.getBracketingInterval(10.0, new Interval()));
  }

  @Test
  public void testIsSingleInterval() {
    assertTrue(Coverages.isSingleInterval(Coverages.create(-Double.MAX_VALUE, Double.MAX_VALUE)));
    assertTrue(Coverages.isSingleInterval(Coverages.create(10, 10)));
    assertTrue(Coverages.isSingleInterval(Coverages.create(IntervalSet.create(0, 10))));
    assertTrue(Coverages.isSingleInterval(Coverages.create(0, 10)));
    assertFalse(Coverages.isSingleInterval(Coverages.create(IntervalSet.create(0, 10, 20, 20))));
  }

  @Test
  public void testIterableCoverageIntervalSetRetrieval() {
    IntervalSet set = IntervalSet.create(0, 10, 20, 20, 30, 40, 50, 60);
    assertSame(set, Coverages.iterable(new IntervalSetCoverage(set)));
  }

  @Test
  public void testIterableCoverage() {
    IntervalSet set = IntervalSet.create(0, 10, 20, 20, 30, 40, 50, 60);
    Coverage coverage = new BoundaryDelegate(Coverages.create(set), 0, 60);

    Set<UnwritableInterval> intervals = Sets.newHashSet(set);

    for (UnwritableInterval interval : Coverages.iterable(coverage)) {
      assertTrue(intervals.contains(interval));
    }
  }

  @Test
  public void testContainsIntervalCoverageUnwritableInterval() {
    Coverage coverage = Coverages.create(IntervalSet.create(0, 10, 20, 20, 30, 40));

    assertTrue(Coverages.containsInterval(coverage, new Interval(0, 10)));
    assertTrue(Coverages.containsInterval(coverage, new Interval(20, 20)));
    assertTrue(Coverages.containsInterval(coverage, new Interval(30, 40)));

    assertTrue(Coverages.containsInterval(coverage, new Interval(0, 5)));
    assertTrue(Coverages.containsInterval(coverage, new Interval(30, 31)));

    assertTrue(Coverages.containsInterval(coverage, new Interval(5, 10)));
    assertTrue(Coverages.containsInterval(coverage, new Interval(35, 40)));

    assertTrue(Coverages.containsInterval(coverage, new Interval(5, 7)));
    assertTrue(Coverages.containsInterval(coverage, new Interval(35, 37)));

    assertFalse(Coverages.containsInterval(coverage, new Interval(-10, -5)));
    assertFalse(Coverages.containsInterval(coverage, new Interval(45, 50)));

    assertFalse(Coverages.containsInterval(coverage, new Interval(-5, 0)));
    assertFalse(Coverages.containsInterval(coverage, new Interval(17, 20)));
    assertFalse(Coverages.containsInterval(coverage, new Interval(25, 30)));

    assertFalse(Coverages.containsInterval(coverage, new Interval(10, 15)));
    assertFalse(Coverages.containsInterval(coverage, new Interval(20, 23)));
    assertFalse(Coverages.containsInterval(coverage, new Interval(40, 45)));

    assertFalse(Coverages.containsInterval(coverage, new Interval(5, 35)));
    assertFalse(Coverages.containsInterval(coverage, new Interval(17, 19)));

  }

  @Test
  public void testContainsIntervalCoverageUnwritableIntervalInterval() {
    Coverage coverage = Coverages.create(IntervalSet.create(0, 10, 20, 20, 30, 40));

    assertTrue(Coverages.containsInterval(coverage, new Interval(0, 10), new Interval()));
    assertTrue(Coverages.containsInterval(coverage, new Interval(20, 20), new Interval()));
    assertTrue(Coverages.containsInterval(coverage, new Interval(30, 40), new Interval()));

    assertTrue(Coverages.containsInterval(coverage, new Interval(0, 5), new Interval()));
    assertTrue(Coverages.containsInterval(coverage, new Interval(30, 31), new Interval()));

    assertTrue(Coverages.containsInterval(coverage, new Interval(5, 10), new Interval()));
    assertTrue(Coverages.containsInterval(coverage, new Interval(35, 40), new Interval()));

    assertTrue(Coverages.containsInterval(coverage, new Interval(5, 7), new Interval()));
    assertTrue(Coverages.containsInterval(coverage, new Interval(35, 37), new Interval()));

    assertFalse(Coverages.containsInterval(coverage, new Interval(-10, -5), new Interval()));
    assertFalse(Coverages.containsInterval(coverage, new Interval(45, 50), new Interval()));

    assertFalse(Coverages.containsInterval(coverage, new Interval(-5, 0), new Interval()));
    assertFalse(Coverages.containsInterval(coverage, new Interval(17, 20), new Interval()));
    assertFalse(Coverages.containsInterval(coverage, new Interval(25, 30), new Interval()));

    assertFalse(Coverages.containsInterval(coverage, new Interval(10, 15), new Interval()));
    assertFalse(Coverages.containsInterval(coverage, new Interval(20, 23), new Interval()));
    assertFalse(Coverages.containsInterval(coverage, new Interval(40, 45), new Interval()));

    assertFalse(Coverages.containsInterval(coverage, new Interval(5, 35), new Interval()));
    assertFalse(Coverages.containsInterval(coverage, new Interval(17, 19), new Interval()));

  }

  @Test
  public void testContainsIntervalCoverageDoubleDouble() {
    Coverage coverage = Coverages.create(IntervalSet.create(0, 10, 20, 20, 30, 40));

    assertTrue(Coverages.containsInterval(coverage, 0, 10));
    assertTrue(Coverages.containsInterval(coverage, 20, 20));
    assertTrue(Coverages.containsInterval(coverage, 30, 40));

    assertTrue(Coverages.containsInterval(coverage, 0, 5));
    assertTrue(Coverages.containsInterval(coverage, 30, 31));

    assertTrue(Coverages.containsInterval(coverage, 5, 10));
    assertTrue(Coverages.containsInterval(coverage, 35, 40));

    assertTrue(Coverages.containsInterval(coverage, 5, 7));
    assertTrue(Coverages.containsInterval(coverage, 35, 37));

    assertFalse(Coverages.containsInterval(coverage, -10, -5));
    assertFalse(Coverages.containsInterval(coverage, 45, 50));

    assertFalse(Coverages.containsInterval(coverage, -5, 0));
    assertFalse(Coverages.containsInterval(coverage, 17, 20));
    assertFalse(Coverages.containsInterval(coverage, 25, 30));

    assertFalse(Coverages.containsInterval(coverage, 10, 15));
    assertFalse(Coverages.containsInterval(coverage, 20, 23));
    assertFalse(Coverages.containsInterval(coverage, 40, 45));

    assertFalse(Coverages.containsInterval(coverage, 5, 35));
    assertFalse(Coverages.containsInterval(coverage, 17, 19));

  }

  @Test
  public void testContainsAll() {

    Coverage a = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60, 70, 100));
    Coverage b = Coverages.create(0, 100);
    Coverage c = Coverages.create(-10, 110);
    Coverage d = Coverages.create(IntervalSet.create(0, 5, 20, 30, 40, 40));
    Coverage dprime = Coverages.create(IntervalSet.create(0, 5, 19, 30, 40, 40));
    Coverage e = Coverages.create(IntervalSet.create(2, 3, 22, 25, 55, 55, 80, 90));
    Coverage copyA = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60, 70, 100));

    assertTrue(Coverages.containsAll(a, a));
    assertFalse(Coverages.containsAll(a, b));
    assertFalse(Coverages.containsAll(a, c));
    assertTrue(Coverages.containsAll(a, d));
    assertFalse(Coverages.containsAll(a, dprime));
    assertTrue(Coverages.containsAll(a, e));
    assertTrue(Coverages.containsAll(a, copyA));

    assertTrue(Coverages.containsAll(b, a));
    assertTrue(Coverages.containsAll(b, b));
    assertFalse(Coverages.containsAll(b, c));
    assertTrue(Coverages.containsAll(b, d));
    assertTrue(Coverages.containsAll(b, dprime));
    assertTrue(Coverages.containsAll(b, e));
    assertTrue(Coverages.containsAll(b, copyA));

    assertTrue(Coverages.containsAll(c, a));
    assertTrue(Coverages.containsAll(c, b));
    assertTrue(Coverages.containsAll(c, c));
    assertTrue(Coverages.containsAll(c, d));
    assertTrue(Coverages.containsAll(c, dprime));
    assertTrue(Coverages.containsAll(c, e));
    assertTrue(Coverages.containsAll(c, copyA));

    assertFalse(Coverages.containsAll(d, a));
    assertFalse(Coverages.containsAll(d, b));
    assertFalse(Coverages.containsAll(d, c));
    assertTrue(Coverages.containsAll(d, d));
    assertFalse(Coverages.containsAll(d, dprime));
    assertFalse(Coverages.containsAll(d, e));
    assertFalse(Coverages.containsAll(d, copyA));

    assertFalse(Coverages.containsAll(dprime, a));
    assertFalse(Coverages.containsAll(dprime, b));
    assertFalse(Coverages.containsAll(dprime, c));
    assertTrue(Coverages.containsAll(dprime, d));
    assertTrue(Coverages.containsAll(dprime, dprime));
    assertFalse(Coverages.containsAll(dprime, e));
    assertFalse(Coverages.containsAll(dprime, copyA));

    assertFalse(Coverages.containsAll(e, a));
    assertFalse(Coverages.containsAll(e, b));
    assertFalse(Coverages.containsAll(e, c));
    assertFalse(Coverages.containsAll(e, d));
    assertFalse(Coverages.containsAll(e, dprime));
    assertTrue(Coverages.containsAll(e, e));
    assertFalse(Coverages.containsAll(e, copyA));

  }

  @Test
  public void testContainsAllWithin() {

    Coverage a = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60, 70, 100));
    Coverage b = Coverages.create(0, 100);
    Coverage c = Coverages.create(-10, 110);
    Coverage d = Coverages.create(IntervalSet.create(0, 5, 20, 30, 40, 40));
    Coverage dprime = Coverages.create(IntervalSet.create(0, 5, 19, 30, 40, 40));
    Coverage e = Coverages.create(IntervalSet.create(2, 3, 22, 25, 55, 55, 80, 90));
    Coverage copyA = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60, 70, 100));

    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, a));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, b));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, c));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, d));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, dprime));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, e));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, a, copyA));

    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, a));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, b));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, c));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, d));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, dprime));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, e));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, b, copyA));

    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, a));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, b));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, c));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, d));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, dprime));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, e));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, c, copyA));

    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, a));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, b));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, c));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, d));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, dprime));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, e));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, d, copyA));

    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, a));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, b));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, c));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, d));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, dprime));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, e));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, dprime, copyA));

    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, a));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, b));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, c));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, d));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, dprime));
    assertTrue(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, e));
    assertFalse(Coverages.containsAllWithin(Interval.ALL_DOUBLES, e, copyA));

    /*
     * Restrict it to 0, 100 interval-wise next. All that should change is that b contains c subject
     * to the bounds.
     */
    Interval interval = new Interval(0, 100);

    assertTrue(Coverages.containsAllWithin(interval, a, a));
    assertFalse(Coverages.containsAllWithin(interval, a, b));
    assertFalse(Coverages.containsAllWithin(interval, a, c));
    assertTrue(Coverages.containsAllWithin(interval, a, d));
    assertFalse(Coverages.containsAllWithin(interval, a, dprime));
    assertTrue(Coverages.containsAllWithin(interval, a, e));
    assertTrue(Coverages.containsAllWithin(interval, a, copyA));

    assertTrue(Coverages.containsAllWithin(interval, b, a));
    assertTrue(Coverages.containsAllWithin(interval, b, b));
    assertTrue(Coverages.containsAllWithin(interval, b, c));
    assertTrue(Coverages.containsAllWithin(interval, b, d));
    assertTrue(Coverages.containsAllWithin(interval, b, dprime));
    assertTrue(Coverages.containsAllWithin(interval, b, e));
    assertTrue(Coverages.containsAllWithin(interval, b, copyA));

    assertTrue(Coverages.containsAllWithin(interval, c, a));
    assertTrue(Coverages.containsAllWithin(interval, c, b));
    assertTrue(Coverages.containsAllWithin(interval, c, c));
    assertTrue(Coverages.containsAllWithin(interval, c, d));
    assertTrue(Coverages.containsAllWithin(interval, c, dprime));
    assertTrue(Coverages.containsAllWithin(interval, c, e));
    assertTrue(Coverages.containsAllWithin(interval, c, copyA));

    assertFalse(Coverages.containsAllWithin(interval, d, a));
    assertFalse(Coverages.containsAllWithin(interval, d, b));
    assertFalse(Coverages.containsAllWithin(interval, d, c));
    assertTrue(Coverages.containsAllWithin(interval, d, d));
    assertFalse(Coverages.containsAllWithin(interval, d, dprime));
    assertFalse(Coverages.containsAllWithin(interval, d, e));
    assertFalse(Coverages.containsAllWithin(interval, d, copyA));

    assertFalse(Coverages.containsAllWithin(interval, dprime, a));
    assertFalse(Coverages.containsAllWithin(interval, dprime, b));
    assertFalse(Coverages.containsAllWithin(interval, dprime, c));
    assertTrue(Coverages.containsAllWithin(interval, dprime, d));
    assertTrue(Coverages.containsAllWithin(interval, dprime, dprime));
    assertFalse(Coverages.containsAllWithin(interval, dprime, e));
    assertFalse(Coverages.containsAllWithin(interval, dprime, copyA));

    assertFalse(Coverages.containsAllWithin(interval, e, a));
    assertFalse(Coverages.containsAllWithin(interval, e, b));
    assertFalse(Coverages.containsAllWithin(interval, e, c));
    assertFalse(Coverages.containsAllWithin(interval, e, d));
    assertFalse(Coverages.containsAllWithin(interval, e, dprime));
    assertTrue(Coverages.containsAllWithin(interval, e, e));
    assertFalse(Coverages.containsAllWithin(interval, e, copyA));

    /*
     * Now set interval to a singleton and try again. The only coverage not to include 40,40 is e.
     */
    interval = new Interval(40, 40);

    assertTrue(Coverages.containsAllWithin(interval, a, a));
    assertTrue(Coverages.containsAllWithin(interval, a, b));
    assertTrue(Coverages.containsAllWithin(interval, a, c));
    assertTrue(Coverages.containsAllWithin(interval, a, d));
    assertTrue(Coverages.containsAllWithin(interval, a, dprime));
    assertTrue(Coverages.containsAllWithin(interval, a, e));
    assertTrue(Coverages.containsAllWithin(interval, a, copyA));

    assertTrue(Coverages.containsAllWithin(interval, b, a));
    assertTrue(Coverages.containsAllWithin(interval, b, b));
    assertTrue(Coverages.containsAllWithin(interval, b, c));
    assertTrue(Coverages.containsAllWithin(interval, b, d));
    assertTrue(Coverages.containsAllWithin(interval, b, dprime));
    assertTrue(Coverages.containsAllWithin(interval, b, e));
    assertTrue(Coverages.containsAllWithin(interval, b, copyA));

    assertTrue(Coverages.containsAllWithin(interval, c, a));
    assertTrue(Coverages.containsAllWithin(interval, c, b));
    assertTrue(Coverages.containsAllWithin(interval, c, c));
    assertTrue(Coverages.containsAllWithin(interval, c, d));
    assertTrue(Coverages.containsAllWithin(interval, c, dprime));
    assertTrue(Coverages.containsAllWithin(interval, c, e));
    assertTrue(Coverages.containsAllWithin(interval, c, copyA));

    assertTrue(Coverages.containsAllWithin(interval, d, a));
    assertTrue(Coverages.containsAllWithin(interval, d, b));
    assertTrue(Coverages.containsAllWithin(interval, d, c));
    assertTrue(Coverages.containsAllWithin(interval, d, d));
    assertTrue(Coverages.containsAllWithin(interval, d, dprime));
    assertTrue(Coverages.containsAllWithin(interval, d, e));
    assertTrue(Coverages.containsAllWithin(interval, d, copyA));

    assertTrue(Coverages.containsAllWithin(interval, dprime, a));
    assertTrue(Coverages.containsAllWithin(interval, dprime, b));
    assertTrue(Coverages.containsAllWithin(interval, dprime, c));
    assertTrue(Coverages.containsAllWithin(interval, dprime, d));
    assertTrue(Coverages.containsAllWithin(interval, dprime, dprime));
    assertTrue(Coverages.containsAllWithin(interval, dprime, e));
    assertTrue(Coverages.containsAllWithin(interval, dprime, copyA));

    assertFalse(Coverages.containsAllWithin(interval, e, a));
    assertFalse(Coverages.containsAllWithin(interval, e, b));
    assertFalse(Coverages.containsAllWithin(interval, e, c));
    assertFalse(Coverages.containsAllWithin(interval, e, d));
    assertFalse(Coverages.containsAllWithin(interval, e, dprime));
    /*
     * e contains itself over [40,40], because it has no elements there. This is analogous to empty
     * set being equivalent to itself.
     */
    assertTrue(Coverages.containsAllWithin(interval, e, e));
    assertFalse(Coverages.containsAllWithin(interval, e, copyA));

    /*
     * And lastly, the non-trivial [5,25].
     */
    interval = new Interval(5, 25);

    assertTrue(Coverages.containsAllWithin(interval, a, a));
    assertFalse(Coverages.containsAllWithin(interval, a, b));
    assertFalse(Coverages.containsAllWithin(interval, a, c));
    assertTrue(Coverages.containsAllWithin(interval, a, d));
    assertFalse(Coverages.containsAllWithin(interval, a, dprime));
    assertTrue(Coverages.containsAllWithin(interval, a, e));
    assertTrue(Coverages.containsAllWithin(interval, a, copyA));

    assertTrue(Coverages.containsAllWithin(interval, b, a));
    assertTrue(Coverages.containsAllWithin(interval, b, b));
    assertTrue(Coverages.containsAllWithin(interval, b, c));
    assertTrue(Coverages.containsAllWithin(interval, b, d));
    assertTrue(Coverages.containsAllWithin(interval, b, dprime));
    assertTrue(Coverages.containsAllWithin(interval, b, e));
    assertTrue(Coverages.containsAllWithin(interval, b, copyA));

    assertTrue(Coverages.containsAllWithin(interval, c, a));
    assertTrue(Coverages.containsAllWithin(interval, c, b));
    assertTrue(Coverages.containsAllWithin(interval, c, c));
    assertTrue(Coverages.containsAllWithin(interval, c, d));
    assertTrue(Coverages.containsAllWithin(interval, c, dprime));
    assertTrue(Coverages.containsAllWithin(interval, c, e));
    assertTrue(Coverages.containsAllWithin(interval, c, copyA));

    assertFalse(Coverages.containsAllWithin(interval, d, a));
    assertFalse(Coverages.containsAllWithin(interval, d, b));
    assertFalse(Coverages.containsAllWithin(interval, d, c));
    assertTrue(Coverages.containsAllWithin(interval, d, d));
    assertFalse(Coverages.containsAllWithin(interval, d, dprime));
    assertTrue(Coverages.containsAllWithin(interval, d, e));
    assertFalse(Coverages.containsAllWithin(interval, d, copyA));

    assertFalse(Coverages.containsAllWithin(interval, dprime, a));
    assertFalse(Coverages.containsAllWithin(interval, dprime, b));
    assertFalse(Coverages.containsAllWithin(interval, dprime, c));
    assertTrue(Coverages.containsAllWithin(interval, dprime, d));
    assertTrue(Coverages.containsAllWithin(interval, dprime, dprime));
    assertTrue(Coverages.containsAllWithin(interval, dprime, e));
    assertFalse(Coverages.containsAllWithin(interval, dprime, copyA));

    assertFalse(Coverages.containsAllWithin(interval, e, a));
    assertFalse(Coverages.containsAllWithin(interval, e, b));
    assertFalse(Coverages.containsAllWithin(interval, e, c));
    assertFalse(Coverages.containsAllWithin(interval, e, d));
    assertFalse(Coverages.containsAllWithin(interval, e, dprime));
    assertTrue(Coverages.containsAllWithin(interval, e, e));
    assertFalse(Coverages.containsAllWithin(interval, e, copyA));

  }

  private static IntervalSet createSet(double... startStops) {
    checkArgument(startStops.length % 2 == 0);

    List<UnwritableInterval> list = Lists.newArrayList();
    for (int i = 0; i < startStops.length; i += 2) {
      list.add(new UnwritableInterval(startStops[i], startStops[i + 1]));
    }

    return IntervalSet.builder().addAll(list).build();
  }

  private static Coverage create(double... startStops) {
    return Coverages.create(createSet(startStops));
  }

  @Test
  public void testUnionCoverageCoverage() {

    Coverage c = Coverages.union(create(0, 100), create(1, 2, 3, 4, 5, 6, 7, 8));
    assertEquals(create(0, 100), c);

    c = Coverages.union(create(1, 2, 3, 4, 5, 6, 7, 8), create(0, 100));
    assertEquals(create(0, 100), c);

    c = Coverages.union(create(0, 10, 90, 100), create(20, 30, 40, 50));
    assertEquals(create(0, 10, 20, 30, 40, 50, 90, 100), c);

    c = Coverages.union(create(0, 10), create(-5, 1, 9, 15));
    assertEquals(create(-5, 15), c);

    c = Coverages.union(create(-5, 1, 9, 15), create(0, 10));
    assertEquals(create(-5, 15), c);

    assertTrue(c.contains(-5));
    assertTrue(c.contains(7.5));
    assertTrue(c.contains(15));

  }

  @Test
  public void testUnionCoverageVarargs() {

    Coverage c = Coverages.union(create(0, 100), create(1, 2, 3, 4, 5, 6, 7, 8), create(110, 120));
    assertEquals(create(0, 100, 110, 120), c);

    c = Coverages.union(create(1, 2, 3, 4, 5, 6, 7, 8), create(110, 120), create(0, 100));
    assertEquals(create(0, 100, 110, 120), c);

    c = Coverages.union(create(0, 10, 90, 100), create(20, 30, 40, 50), create(20, 50));
    assertEquals(create(0, 10, 20, 50, 90, 100), c);

    c = Coverages.union(create(0, 10), create(-5, 1, 9, 15), create(15, 20));
    assertEquals(create(-5, 20), c);

    c = Coverages.union(create(-5, 1, 9, 15), create(15, 20), create(0, 10));
    assertEquals(create(-5, 20), c);

    assertTrue(c.contains(-5));
    assertTrue(c.contains(7.5));
    assertTrue(c.contains(20));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testIntersectCoverageCoverageEmptyIntersectionException() {
    Coverages.intersect(create(10, 20), create(30, 40));
  }

  @Test
  public void testIntersectCoverageCoverage() {

    Coverage c = Coverages.intersect(create(0, 100), create(1, 2, 3, 4, 5, 6, 7, 8));
    assertEquals(create(1, 2, 3, 4, 5, 6, 7, 8), c);

    c = Coverages.intersect(create(1, 2, 3, 4, 5, 6, 7, 8), create(0, 100));
    assertEquals(create(1, 2, 3, 4, 5, 6, 7, 8), c);

    c = Coverages.intersect(create(0, 10), create(-5, 1, 9, 15));
    assertEquals(create(0, 1, 9, 10), c);

    c = Coverages.intersect(create(-5, 1, 9, 15), create(0, 10));
    assertEquals(create(0, 1, 9, 10), c);

    assertTrue(c.contains(0.0));
    assertTrue(c.contains(0.5));
    assertTrue(c.contains(1.0));
    assertTrue(c.contains(9.0));
    assertTrue(c.contains(9.5));
    assertTrue(c.contains(10.0));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testIntersectCoverageVarargsEmptyIntersectionException() {
    Coverages.intersect(create(10, 20), create(30, 40), create(50, 60));
  }

  @Test
  public void testIntersectCoverageVarargs() {

    Coverage c = Coverages.intersect(create(0, 100), create(1, 2, 3, 4, 5, 6, 7, 8),
        create(1, 8, 9, 10, 11, 12));
    assertEquals(create(1, 2, 3, 4, 5, 6, 7, 8), c);

    c = Coverages.intersect(create(1, 2, 3, 4, 5, 6, 7, 8), create(1, 8, 9, 10, 11, 12),
        create(0, 100));
    assertEquals(create(1, 2, 3, 4, 5, 6, 7, 8), c);

    c = Coverages.intersect(create(0, 10), create(-5, 1, 9, 15), create(0, 16), create(0.5, 20.0));
    assertEquals(create(0.5, 1, 9, 10), c);

    c = Coverages.intersect(create(-5, 1, 9, 15), create(0, 10), create(0.5, 20.0),
        create(0, 1, 9, 10));
    assertEquals(create(0.5, 1, 9, 10), c);

    assertFalse(c.contains(0.0));
    assertTrue(c.contains(0.5));
    assertTrue(c.contains(1.0));
    assertTrue(c.contains(9.0));
    assertTrue(c.contains(9.5));
    assertTrue(c.contains(10.0));

  }

  @Test
  public void testIntersectsCoverageVarargs() {

    assertTrue(Coverages.intersects(create(1, 2, 3, 4), create(0.5, 3.5), create(0.5, 1.0)));
    assertTrue(Coverages.intersects(create(1, 2, 3, 4), create(0.5, 1.0), create(0.5, 3.5)));
    assertTrue(Coverages.intersects(create(0.5, 1.0), create(1, 2, 3, 4), create(0.5, 3.5)));

    assertFalse(Coverages.intersects(create(3, 4, 10, 12), create(0.5, 3.5), create(0.5, 1.0)));
    assertFalse(Coverages.intersects(create(3, 4, 10, 12), create(0.5, 1.0), create(0.5, 3.5)));
    assertFalse(Coverages.intersects(create(0.5, 1.0), create(3, 4, 10, 12), create(0.5, 3.5)));

  }

  @Test
  public void testIntersectsCoverageCoverage() {

    assertTrue(Coverages.intersects(create(1, 2, 3, 4), create(0.5, 3.5)));
    assertTrue(Coverages.intersects(create(0.5, 3.5), create(1, 2, 3, 4)));

    assertFalse(Coverages.intersects(create(0, 1), create(2, 3)));
    assertFalse(Coverages.intersects(create(2, 3), create(0, 1)));
  }

  @Test
  public void testEqualsImplementation() {

    Coverage a = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60));
    Coverage b = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40));
    Coverage dA = new BoundaryDelegate(a, -10, 70);
    Coverage c = Coverages.create(0, 10);
    Coverage d = Coverages.create(IntervalSet.create(0, 10));
    Coverage e = Coverages.create(IntervalSet.create(0, 10, 20, 30));
    Coverage copyA = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60));

    assertTrue(a.equals(a));
    assertFalse(a.equals(b));
    assertTrue(a.equals(dA));
    assertFalse(a.equals(c));
    assertFalse(a.equals(d));
    assertFalse(a.equals(e));
    assertTrue(a.equals(copyA));
    assertFalse(a.equals(""));
    assertFalse(a.equals(null));

    assertFalse(b.equals(a));
    assertTrue(b.equals(b));
    assertFalse(b.equals(dA));
    assertFalse(b.equals(c));
    assertFalse(b.equals(d));
    assertFalse(b.equals(e));
    assertFalse(b.equals(copyA));
    assertFalse(b.equals(""));
    assertFalse(b.equals(null));

    assertTrue(dA.equals(a));
    assertFalse(dA.equals(b));
    assertTrue(dA.equals(dA));
    assertFalse(dA.equals(c));
    assertFalse(dA.equals(d));
    assertFalse(dA.equals(e));
    assertTrue(dA.equals(copyA));
    assertFalse(dA.equals(""));
    assertFalse(dA.equals(null));

    assertFalse(c.equals(a));
    assertFalse(c.equals(b));
    assertFalse(c.equals(dA));
    assertTrue(c.equals(c));
    assertTrue(c.equals(d));
    assertFalse(c.equals(e));
    assertFalse(c.equals(copyA));
    assertFalse(c.equals(""));
    assertFalse(c.equals(null));

    assertFalse(d.equals(a));
    assertFalse(d.equals(b));
    assertFalse(d.equals(dA));
    assertTrue(d.equals(c));
    assertTrue(d.equals(d));
    assertFalse(d.equals(e));
    assertFalse(d.equals(copyA));
    assertFalse(d.equals(""));
    assertFalse(d.equals(null));

    assertFalse(e.equals(a));
    assertFalse(e.equals(b));
    assertFalse(e.equals(dA));
    assertFalse(e.equals(c));
    assertFalse(e.equals(d));
    assertTrue(e.equals(e));
    assertFalse(e.equals(copyA));
    assertFalse(e.equals(""));
    assertFalse(e.equals(null));

  }

  @Test
  public void testHashCodeImplementation() {

    Coverage a = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60));
    Coverage dA = new BoundaryDelegate(a, -10, 70);
    Coverage copyA = Coverages.create(IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60));

    assertEquals(a.hashCode(), dA.hashCode());
    assertEquals(a.hashCode(), copyA.hashCode());

  }

  @Test
  public void testTicket40Bug() {

    /*
     * Trigger the Math.ulp() issue.
     */
    IntervalSet expected = IntervalSet.create(0, 10, 20, 30, 40, 40, 50, 60);
    IntervalSet delegatingSet = IntervalSet.create(1e25, 1e25 + 1e20, 1e25 + 2e20, 1e25 + 3e20,
        1e25 + 4e20, 1e25 + 4e20, 1e25 + 5e20, 1e25 + 6e20);

    ConversionDelegate delegate =
        new ConversionDelegate(Coverages.create(delegatingSet), 1e25, 1e19);

    IntervalSet actual = Coverages.snapshotToIntervalSet(delegate);

    /*
     * We need to use fuzzy comparison logic due to the magnitude of the conversion factors.
     */
    assertEquals("Interval sets are not of the same length", expected.size(), actual.size());

    for (int i = 0; i < expected.size(); i++) {
      AssertTools.assertEqualInterval(expected.get(i), actual.get(i), FUZZY_TOL);
    }

  }

  /**
   * Simple private class used to capture a coverage instance that applies a basic linear conversion
   * for times prior to delegating to a supplied coverage.
   */
  private static class ConversionDelegate implements Coverage {

    private final Coverage delegate;
    private final double offset;
    private final double multiplier;

    public ConversionDelegate(Coverage delegate, double offset, double multiplier) {
      super();
      this.delegate = delegate;
      this.offset = offset;
      this.multiplier = multiplier;
    }

    @Override
    public boolean contains(double time) {
      return delegate.contains(convertToCoverageBase(time));
    }

    @Override
    public Interval getBoundingInterval(Interval buffer) {
      return convertInPlaceFromCoverageBase(delegate.getBoundingInterval(buffer));
    }

    @Override
    public Interval getBracketingInterval(double time, Interval buffer) {
      return convertInPlaceFromCoverageBase(
          delegate.getBracketingInterval(convertToCoverageBase(time), buffer));
    }

    @Override
    public boolean hasNextInterval(double time) {
      return delegate.hasNextInterval(convertToCoverageBase(time));
    }

    @Override
    public Interval getNextInterval(double time, Interval buffer) {
      return convertInPlaceFromCoverageBase(
          delegate.getNextInterval(convertToCoverageBase(time), buffer));
    }

    private double convertToCoverageBase(double time) {
      return time * multiplier + offset;
    }

    private double convertFromCoverageBase(double baseTime) {
      return (baseTime - offset) / multiplier;
    }

    private Interval convertInPlaceFromCoverageBase(Interval buffer) {
      buffer.set(convertFromCoverageBase(buffer.getBegin()),
          convertFromCoverageBase(buffer.getEnd()));
      return buffer;
    }
  }

  /**
   * Simple private class used to capture a coverage instance that delegates its boundary
   * specification to the supplied start and end for test purposes.
   */
  private static class BoundaryDelegate implements Coverage {

    private final Coverage delegate;
    private final double boundaryStart;
    private final double boundaryEnd;

    public BoundaryDelegate(Coverage delegate, double boundaryStart, double boundaryEnd) {
      this.delegate = delegate;
      this.boundaryStart = boundaryStart;
      this.boundaryEnd = boundaryEnd;
    }

    @Override
    public boolean contains(double time) {
      return delegate.contains(time);
    }

    @Override
    public Interval getBoundingInterval(Interval buffer) {
      buffer.set(boundaryStart, boundaryEnd);
      return buffer;
    }

    @Override
    public Interval getBracketingInterval(double time, Interval buffer) {
      return delegate.getBracketingInterval(time, buffer);
    }

    @Override
    public boolean hasNextInterval(double time) {
      return delegate.hasNextInterval(time);
    }

    @Override
    public Interval getNextInterval(double time, Interval buffer) {
      return delegate.getNextInterval(time, buffer);
    }

    @Override
    public boolean equals(Object object) {
      return delegate.equals(object);
    }

    @Override
    public int hashCode() {
      return delegate.hashCode();
    }

  }

}
