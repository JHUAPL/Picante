package picante.mechanics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

public class IntervalCoverageTest {

  private IntervalCoverage coverage;

  @Before
  public void setUp() throws Exception {
    coverage = new IntervalCoverage(-10, 25);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIntervalCoverageIllegalArgumentException() {
    new IntervalCoverage(15, -10);
  }

  @Test
  public void testContains() {
    assertTrue(coverage.contains(-10));
    assertTrue(coverage.contains(15));
    assertTrue(coverage.contains(25));
    assertFalse(coverage.contains(-10.1));
    assertFalse(coverage.contains(25.1));

  }

  @Test
  public void testGetBoundingInterval() {
    Interval interval = new Interval();
    Interval bound = coverage.getBoundingInterval(interval);
    assertSame(bound, interval);
    assertEquals(-10, bound.getBegin(), 0.0);
    assertEquals(25, bound.getEnd(), 0.0);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetBracketingIntervalLowException() {
    coverage.getBracketingInterval(-10.5, new Interval());
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetBracketingIntervalHighException() {
    coverage.getBracketingInterval(25.5, new Interval());
  }

  @Test
  public void testGetBracketingInterval() {
    Interval interval = new Interval();

    Interval bracket = coverage.getBracketingInterval(0, interval);
    assertSame(bracket, interval);
    assertEquals(-10, bracket.getBegin(), 0.0);
    assertEquals(25, bracket.getEnd(), 0.0);

    bracket = coverage.getBracketingInterval(-10, interval);
    assertSame(bracket, interval);
    assertEquals(-10, bracket.getBegin(), 0.0);
    assertEquals(25, bracket.getEnd(), 0.0);

    bracket = coverage.getBracketingInterval(25, interval);
    assertSame(bracket, interval);
    assertEquals(-10, bracket.getBegin(), 0.0);
    assertEquals(25, bracket.getEnd(), 0.0);

  }

  @Test
  public void testHasNextInterval() {
    assertTrue(coverage.hasNextInterval(-300));
    assertTrue(coverage.hasNextInterval(-10 - Math.ulp(-10)));
    assertFalse(coverage.hasNextInterval(-10));
    assertFalse(coverage.hasNextInterval(20));
    assertFalse(coverage.hasNextInterval(25));
    assertFalse(coverage.hasNextInterval(30));
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetNextIntervalExceptionAtLastCoverageStart() {
    coverage.getNextInterval(-10.0, new Interval());
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetNextIntervalExceptionInLastCoverage() {
    coverage.getNextInterval(0.0, new Interval());
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetNextIntervalExceptionAtLastCoverageStop() {
    coverage.getNextInterval(25.0, new Interval());
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetNextIntervalExceptionAfterLastCoverage() {
    coverage.getNextInterval(30.0, new Interval());
  }

  @Test
  public void testGetNextInterval() {
    Interval interval = new Interval();
    Interval next = coverage.getNextInterval(-11.0, interval);
    assertSame(next, interval);
    assertEquals(-10, next.getBegin(), 0.0);
    assertEquals(25.0, next.getEnd(), 0.0);
  }

  @Test
  public void testGetInterval() {
    IntervalCoverage a = new IntervalCoverage(10, 20);
    UnwritableInterval input = new UnwritableInterval(10, 20);
    IntervalCoverage b = new IntervalCoverage(input);

    UnwritableInterval aInt = a.getInterval();
    UnwritableInterval bInt = b.getInterval();

    assertEquals(aInt, input);
    assertEquals(bInt, input);
  }

  @Test
  public void testEquals() {

    IntervalCoverage a = new IntervalCoverage(10, 20);
    IntervalSetCoverage b = new IntervalSetCoverage(IntervalSet.create(10, 20));

    IntervalSetCoverage c = new IntervalSetCoverage(IntervalSet.create(10, 20, 30, 40));

    assertFalse(a.equals("STRING"));
    assertFalse(a.equals(null));
    assertFalse(a.equals(c));
    assertTrue(a.equals(b));

  }

  @Test
  public void testHashCode() {
    /*
     * Just verify that two interval coverages have the same hashCode.
     */
    IntervalCoverage a = new IntervalCoverage(10, 20);

    IntervalSetCoverage b = new IntervalSetCoverage(IntervalSet.builder().add(10, 20).build());

    assertNotSame(a, b);
    assertEquals(a.hashCode(), b.hashCode());

  }
}
