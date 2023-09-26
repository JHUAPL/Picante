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

public class IntervalSetCoverageTest {

  private IntervalSet setCoverage;
  private IntervalSet setDuplicate;
  private IntervalSet setAlternate;
  private IntervalSetCoverage coverage;
  private IntervalSetCoverage duplicate;
  private IntervalSetCoverage alternate;

  @Before
  public void setUp() throws Exception {
    setCoverage = IntervalSet.create(0.0, 10.0, 20.0, 20.0, 30.0, 40.0, 50.0, 60.0);
    coverage = new IntervalSetCoverage(setCoverage);
    setDuplicate = IntervalSet.builder().addAll(coverage.getSet()).build();
    duplicate = new IntervalSetCoverage(setDuplicate);
    setAlternate = IntervalSet.create(10.0, 20.0, 30.0, 40.0);
    alternate = new IntervalSetCoverage(setAlternate);
  }

  @Test
  public void testIntervalSetCoverage() {
    assertSame(setCoverage, coverage.getSet());
    assertSame(setDuplicate, duplicate.getSet());
    assertSame(setAlternate, alternate.getSet());
  }

  @Test
  public void testContains() {
    assertFalse(coverage.contains(-Double.MAX_VALUE));
    assertFalse(coverage.contains(-1.0));
    assertTrue(coverage.contains(0.0));
    assertTrue(coverage.contains(5.0));
    assertTrue(coverage.contains(10.0));
    assertFalse(coverage.contains(15.0));
    assertTrue(coverage.contains(20.0));
    assertFalse(coverage.contains(25.0));
    assertTrue(coverage.contains(30.0));
    assertTrue(coverage.contains(35.0));
    assertTrue(coverage.contains(40.0));
    assertFalse(coverage.contains(45.0));
    assertTrue(coverage.contains(50.0));
    assertTrue(coverage.contains(55.0));
    assertTrue(coverage.contains(60.0));
    assertFalse(coverage.contains(65.0));
    assertFalse(coverage.contains(Double.MAX_VALUE));
  }

  @Test
  public void testGetBoundingInterval() {
    assertEquals(new UnwritableInterval(0.0, 60.0), coverage.getBoundingInterval(new Interval()));
    assertEquals(new UnwritableInterval(10.0, 40.0), alternate.getBoundingInterval(new Interval()));
  }

  @Test
  public void testGetBracketingInterval() {

    for (UnwritableInterval interval : setCoverage) {
      assertEquals(interval, coverage.getBracketingInterval(interval.getBegin(), new Interval()));
      assertEquals(interval, coverage.getBracketingInterval(interval.getMiddle(), new Interval()));
      assertEquals(interval, coverage.getBracketingInterval(interval.getEnd(), new Interval()));
    }

  }

  @Test
  public void testHasNextInterval() {
    assertTrue(coverage.hasNextInterval(-Double.MAX_VALUE));
    assertTrue(coverage.hasNextInterval(0.0));
    assertTrue(coverage.hasNextInterval(5.0));
    assertTrue(coverage.hasNextInterval(10.0));
    assertTrue(coverage.hasNextInterval(15.0));
    assertTrue(coverage.hasNextInterval(20.0));
    assertTrue(coverage.hasNextInterval(25.0));
    assertTrue(coverage.hasNextInterval(30.0));
    assertTrue(coverage.hasNextInterval(35.0));
    assertTrue(coverage.hasNextInterval(40.0));
    assertTrue(coverage.hasNextInterval(45.0));
    assertFalse(coverage.hasNextInterval(50.0));
    assertFalse(coverage.hasNextInterval(55.0));
    assertFalse(coverage.hasNextInterval(60.0));
    assertFalse(coverage.hasNextInterval(65.0));

  }

  @Test
  public void testGetNextInterval() {

    double time = -Double.MAX_VALUE;
    double midTime = -Double.MAX_VALUE;
    double endTime = -Double.MAX_VALUE;
    for (UnwritableInterval interval : setCoverage) {
      assertEquals(interval, coverage.getNextInterval(time, new Interval()));
      assertEquals(interval, coverage.getNextInterval(midTime, new Interval()));
      assertEquals(interval, coverage.getNextInterval(endTime, new Interval()));
      time = interval.getBegin();
      midTime = interval.getMiddle();
      endTime = interval.getEnd();
    }
  }

  @Test
  public void testEquals() {

    assertFalse(coverage.equals(null));
    assertFalse(coverage.equals("String"));
    assertFalse(coverage.equals(alternate));
    assertNotSame(duplicate, coverage);
    assertTrue(coverage.equals(duplicate));
    assertTrue(duplicate.equals(coverage));
    assertFalse(alternate.equals(coverage));

  }

  @Test
  public void testHashCode() {
    assertNotSame(duplicate, coverage);
    assertEquals(coverage.hashCode(), duplicate.hashCode());
  }

}
