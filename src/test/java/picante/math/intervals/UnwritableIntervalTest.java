package picante.math.intervals;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.math.intervals.Interval.ALL_DOUBLES;
import org.junit.Before;
import org.junit.Test;

public class UnwritableIntervalTest {

  private UnwritableInterval singleton;
  private UnwritableInterval interval;

  @Before
  public void setUp() throws Exception {
    singleton = new UnwritableInterval(5.0, 5.0);
    interval = new UnwritableInterval(-5.0, 5.0);
  }

  @Test
  public void testHashCode() {
    assertEquals(interval.hashCode(), new UnwritableInterval(-5.0, 5.0).hashCode());
    assertEquals(interval.hashCode(), new Interval(-5.0, 5.0).hashCode());
  }

  @Test
  public void testUnwritableInterval() {
    assertEquals(ALL_DOUBLES, new UnwritableInterval());
  }

  @Test
  public void testUnwritableIntervalUnwritableInterval() {
    assertEquals(singleton, new UnwritableInterval(singleton));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testUnwritableIntervalDoubleDoubleException() {
    new UnwritableInterval(10, 5);
  }

  @Test
  public void testUnwritableIntervalDoubleDouble() {
    assertEquals(-5.0, interval.getBegin(), 0.0);
    assertEquals(5.0, interval.getEnd(), 0.0);
  }

  @Test
  public void testGetBegin() {
    assertEquals(5.0, singleton.getBegin(), 0.0);
    assertEquals(-5.0, interval.getBegin(), 0.0);
  }

  @Test
  public void testGetEnd() {
    assertEquals(5.0, singleton.getEnd(), 0.0);
    assertEquals(5.0, interval.getEnd(), 0.0);
  }

  @Test
  public void testIsSingleton() {
    assertTrue(singleton.isSingleton());
    assertFalse(interval.isSingleton());
  }

  @Test
  public void testGetLength() {
    assertEquals(0.0, singleton.getLength(), 0.0);
    assertEquals(10.0, interval.getLength(), 0.0);
  }

  @Test
  public void testEqualsObject() {
    assertFalse(singleton.equals(null));
    assertTrue(singleton.equals(new UnwritableInterval(5.0, 5.0)));
    assertFalse(singleton.equals(interval));
    assertFalse(singleton.equals("Test"));
    assertTrue(singleton.equals(new Interval(5.0, 5.0)));
  }

  @Test
  public void testToString() {
    assertEquals("[" + Double.toString(-5.0) + "," + Double.toString(5.0) + "]",
        interval.toString());
  }

  @Test
  public void testDistanceTo() {

    UnwritableInterval interval = new UnwritableInterval(4.0, 18.0);

    assertEquals(0.0, interval.signedDistanceTo(4.0), 0.0);
    assertEquals(0.0, interval.signedDistanceTo(12.0), 0.0);
    assertEquals(0.0, interval.signedDistanceTo(18.0), 0.0);
    assertEquals(1.0, interval.signedDistanceTo(3.0), 0.0);
    assertEquals(4.0, interval.signedDistanceTo(0.0), 0.0);
    assertEquals(-1.0, interval.signedDistanceTo(19.0), 0.0);
    assertEquals(-4.0, interval.signedDistanceTo(22.0), 0.0);

    interval = new UnwritableInterval(-4.0, 18.0);

    assertEquals(0.0, interval.signedDistanceTo(-4.0), 0.0);
    assertEquals(0.0, interval.signedDistanceTo(12.0), 0.0);
    assertEquals(0.0, interval.signedDistanceTo(18.0), 0.0);
    assertEquals(1.0, interval.signedDistanceTo(-5.0), 0.0);
    assertEquals(4.0, interval.signedDistanceTo(-8.0), 0.0);
    assertEquals(-1.0, interval.signedDistanceTo(19.0), 0.0);
    assertEquals(-4.0, interval.signedDistanceTo(22.0), 0.0);

    interval = new UnwritableInterval(-18.0, -4.0);

    assertEquals(0.0, interval.signedDistanceTo(-18.0), 0.0);
    assertEquals(0.0, interval.signedDistanceTo(-12.0), 0.0);
    assertEquals(0.0, interval.signedDistanceTo(-4.0), 0.0);
    assertEquals(1.0, interval.signedDistanceTo(-19.0), 0.0);
    assertEquals(4.0, interval.signedDistanceTo(-22.0), 0.0);
    assertEquals(-1.0, interval.signedDistanceTo(-3), 0.0);
    assertEquals(-4.0, interval.signedDistanceTo(0.0), 0.0);
  }

  @Test
  public void testCopyOf() {

    UnwritableInterval unwritable = new UnwritableInterval(1, 2);
    UnwritableInterval notUnwritable = new UnwritableInterval(5, 6) {};

    UnwritableInterval result = UnwritableInterval.copyOf(unwritable);
    assertSame(result, unwritable);

    result = UnwritableInterval.copyOf(notUnwritable);
    assertNotSame(result, notUnwritable);
    assertEquals(result, notUnwritable);
    assertEquals(UnwritableInterval.class, result.getClass());

  }

  @Test
  public void testOpenContainsDouble() {
    assertFalse(interval.openContains(-10.0));
    assertFalse(interval.openContains(-5.0));
    assertTrue(interval.openContains(-5.0 + Math.ulp(-5.0)));
    assertTrue(interval.openContains(0.0));
    assertTrue(interval.openContains(5.0 - Math.ulp(5.0)));
    assertFalse(interval.openContains(5.0));
    assertFalse(interval.openContains(10.0));

    assertFalse(singleton.openContains(5.0));
  }

  @Test
  public void testClosedContainsDouble() {
    assertFalse(interval.closedContains(-10.0));
    assertTrue(interval.closedContains(-5.0));
    assertTrue(interval.closedContains(-5.0 + Math.ulp(-5.0)));
    assertTrue(interval.closedContains(0.0));
    assertTrue(interval.closedContains(5.0 - Math.ulp(5.0)));
    assertTrue(interval.closedContains(5.0));
    assertFalse(interval.closedContains(10.0));

    assertTrue(singleton.closedContains(5.0));
  }

  @Test
  public void testBeginOpenContainsDouble() {
    assertFalse(interval.beginOpenContains(-10.0));
    assertFalse(interval.beginOpenContains(-5.0));
    assertTrue(interval.beginOpenContains(-5.0 + Math.ulp(-5.0)));
    assertTrue(interval.beginOpenContains(0.0));
    assertTrue(interval.beginOpenContains(5.0 - Math.ulp(5.0)));
    assertTrue(interval.beginOpenContains(5.0));
    assertFalse(interval.beginOpenContains(10.0));

    assertFalse(singleton.beginOpenContains(5.0));
  }

  @Test
  public void testEndOpenContainsDouble() {
    assertFalse(interval.endOpenContains(-10.0));
    assertTrue(interval.endOpenContains(-5.0));
    assertTrue(interval.endOpenContains(-5.0 + Math.ulp(-5.0)));
    assertTrue(interval.endOpenContains(0.0));
    assertTrue(interval.endOpenContains(5.0 - Math.ulp(5.0)));
    assertFalse(interval.endOpenContains(5.0));
    assertFalse(interval.endOpenContains(10.0));

    assertFalse(singleton.endOpenContains(5.0));
  }

  @Test
  public void testGetMiddle() {
    assertEquals(0.0, interval.getMiddle(), 0.0);
    assertEquals(5.0, singleton.getMiddle(), 0.0);
  }

  @Test
  public void testClamp() {
    assertEquals(-5.0, interval.clamp(-10.0), 0.0);
    assertEquals(-5.0, interval.clamp(-5.0), 0.0);
    assertEquals(0.0, interval.clamp(0.0), 0.0);
    assertEquals(5.0, interval.clamp(5.0), 0.0);
    assertEquals(5.0, interval.clamp(10.0), 0.0);

    assertEquals(5.0, singleton.clamp(-4.0), 0.0);
    assertEquals(5.0, singleton.clamp(5.0), 0.0);
    assertEquals(5.0, singleton.clamp(6.0), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testClosedContainsDoubleDoubleException() {
    interval.closedContains(10.0, 5.0);
  }

  @Test
  public void testClosedContainsDoubleDouble() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.closedContains(-100, 100));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.closedContains(-10.0, -5.0 - Math.ulp(-5.0)));
    assertFalse(interval.closedContains(5.0 + Math.ulp(5.0), 10.0));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval.closedContains(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0)));
    assertTrue(interval.closedContains(-5.0, -5.0));
    assertTrue(interval.closedContains(0.0, 0.0));
    assertTrue(interval.closedContains(5.0, 5.0));
    assertFalse(interval.closedContains(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0)));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertTrue(interval.closedContains(-5.0, 5.0));
    assertTrue(interval.closedContains(-1.0, 1.0));
    assertTrue(interval.closedContains(-5.0, 0.0));
    assertTrue(interval.closedContains(0.0, 5.0));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.closedContains(-10.0, 0.0));
    assertFalse(interval.closedContains(0, 10.0));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertTrue(singleton.closedContains(5.0, 5.0));
  }

  @Test
  public void testClosedContainsUnwritableInterval() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.closedContains(new UnwritableInterval(-100, 100)));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.closedContains(new UnwritableInterval(-10.0, -5.0 - Math.ulp(-5.0))));
    assertFalse(interval.closedContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 10.0)));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval
        .closedContains(new UnwritableInterval(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0))));
    assertTrue(interval.closedContains(new UnwritableInterval(-5.0, -5.0)));
    assertTrue(interval.closedContains(new UnwritableInterval(0.0, 0.0)));
    assertTrue(interval.closedContains(new UnwritableInterval(5.0, 5.0)));
    assertFalse(
        interval.closedContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0))));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertTrue(interval.closedContains(new UnwritableInterval(-5.0, 5.0)));
    assertTrue(interval.closedContains(new UnwritableInterval(-1.0, 1.0)));
    assertTrue(interval.closedContains(new UnwritableInterval(-5.0, 0.0)));
    assertTrue(interval.closedContains(new UnwritableInterval(0.0, 5.0)));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.closedContains(new UnwritableInterval(-10.0, 0.0)));
    assertFalse(interval.closedContains(new UnwritableInterval(0, 10.0)));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertTrue(singleton.closedContains(new UnwritableInterval(5.0, 5.0)));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testOpenContainsDoubleDoubleException() {
    interval.openContains(10.0, 5.0);
  }

  @Test
  public void testOpenContainsDoubleDouble() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.openContains(-100, 100));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.openContains(-10.0, -5.0 - Math.ulp(-5.0)));
    assertFalse(interval.openContains(5.0 + Math.ulp(5.0), 10.0));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval.openContains(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0)));
    assertFalse(interval.openContains(-5.0, -5.0));
    assertTrue(interval.openContains(0.0, 0.0));
    assertFalse(interval.openContains(5.0, 5.0));
    assertFalse(interval.openContains(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0)));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertFalse(interval.openContains(-5.0, 5.0));
    assertTrue(interval.openContains(-1.0, 1.0));
    assertFalse(interval.openContains(-5.0, 0.0));
    assertFalse(interval.openContains(0.0, 5.0));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.openContains(-10.0, 0.0));
    assertFalse(interval.openContains(0, 10.0));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertFalse(singleton.openContains(5.0, 5.0));

  }

  @Test
  public void testOpenContainsUnwritableInterval() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.openContains(new UnwritableInterval(-100, 100)));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.openContains(new UnwritableInterval(-10.0, -5.0 - Math.ulp(-5.0))));
    assertFalse(interval.openContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 10.0)));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval
        .openContains(new UnwritableInterval(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0))));
    assertFalse(interval.openContains(new UnwritableInterval(-5.0, -5.0)));
    assertTrue(interval.openContains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(interval.openContains(new UnwritableInterval(5.0, 5.0)));
    assertFalse(
        interval.openContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0))));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertFalse(interval.openContains(new UnwritableInterval(-5.0, 5.0)));
    assertTrue(interval.openContains(new UnwritableInterval(-1.0, 1.0)));
    assertFalse(interval.openContains(new UnwritableInterval(-5.0, 0.0)));
    assertFalse(interval.openContains(new UnwritableInterval(0.0, 5.0)));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.openContains(new UnwritableInterval(-10.0, 0.0)));
    assertFalse(interval.openContains(new UnwritableInterval(0, 10.0)));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertFalse(singleton.openContains(new UnwritableInterval(5.0, 5.0)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testBeginOpenContainsDoubleDoubleException() {
    interval.beginOpenContains(10.0, 5.0);
  }

  @Test
  public void testBeginOpenContainsDoubleDouble() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.beginOpenContains(-100, 100));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.beginOpenContains(-10.0, -5.0 - Math.ulp(-5.0)));
    assertFalse(interval.beginOpenContains(5.0 + Math.ulp(5.0), 10.0));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval.beginOpenContains(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0)));
    assertFalse(interval.beginOpenContains(-5.0, -5.0));
    assertTrue(interval.beginOpenContains(0.0, 0.0));
    assertTrue(interval.beginOpenContains(5.0, 5.0));
    assertFalse(interval.beginOpenContains(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0)));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertFalse(interval.beginOpenContains(-5.0, 5.0));
    assertTrue(interval.beginOpenContains(-1.0, 1.0));
    assertFalse(interval.beginOpenContains(-5.0, 0.0));
    assertTrue(interval.beginOpenContains(0.0, 5.0));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.beginOpenContains(-10.0, 0.0));
    assertFalse(interval.beginOpenContains(0, 10.0));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertFalse(singleton.beginOpenContains(5.0, 5.0));

  }

  @Test
  public void testBeginOpenContainsUnwritableInterval() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.beginOpenContains(new UnwritableInterval(-100, 100)));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.beginOpenContains(new UnwritableInterval(-10.0, -5.0 - Math.ulp(-5.0))));
    assertFalse(interval.beginOpenContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 10.0)));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval
        .beginOpenContains(new UnwritableInterval(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0))));
    assertFalse(interval.beginOpenContains(new UnwritableInterval(-5.0, -5.0)));
    assertTrue(interval.beginOpenContains(new UnwritableInterval(0.0, 0.0)));
    assertTrue(interval.beginOpenContains(new UnwritableInterval(5.0, 5.0)));
    assertFalse(interval
        .beginOpenContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0))));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertFalse(interval.beginOpenContains(new UnwritableInterval(-5.0, 5.0)));
    assertTrue(interval.beginOpenContains(new UnwritableInterval(-1.0, 1.0)));
    assertFalse(interval.beginOpenContains(new UnwritableInterval(-5.0, 0.0)));
    assertTrue(interval.beginOpenContains(new UnwritableInterval(0.0, 5.0)));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.beginOpenContains(new UnwritableInterval(-10.0, 0.0)));
    assertFalse(interval.beginOpenContains(new UnwritableInterval(0, 10.0)));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertFalse(singleton.beginOpenContains(new UnwritableInterval(5.0, 5.0)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testEndOpenContainsDoubleDoubleException() {
    interval.endOpenContains(10.0, 5.0);
  }

  @Test
  public void testEndOpenContainsDoubleDouble() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.endOpenContains(-100, 100));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.endOpenContains(-10.0, -5.0 - Math.ulp(-5.0)));
    assertFalse(interval.endOpenContains(5.0 + Math.ulp(5.0), 10.0));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval.endOpenContains(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0)));
    assertTrue(interval.endOpenContains(-5.0, -5.0));
    assertTrue(interval.endOpenContains(0.0, 0.0));
    assertFalse(interval.endOpenContains(5.0, 5.0));
    assertFalse(interval.endOpenContains(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0)));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertFalse(interval.endOpenContains(-5.0, 5.0));
    assertTrue(interval.endOpenContains(-1.0, 1.0));
    assertTrue(interval.endOpenContains(-5.0, 0.0));
    assertFalse(interval.endOpenContains(0.0, 5.0));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.endOpenContains(-10.0, 0.0));
    assertFalse(interval.endOpenContains(0, 10.0));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertFalse(singleton.endOpenContains(5.0, 5.0));

  }

  @Test
  public void testEndOpenContainsUnwritableInterval() {
    /*
     * Test against interval that includes interval entirely.
     */
    assertFalse(interval.endOpenContains(new UnwritableInterval(-100, 100)));

    /*
     * Test intervals that are just next to the boundary of interval.
     */
    assertFalse(interval.endOpenContains(new UnwritableInterval(-10.0, -5.0 - Math.ulp(-5.0))));
    assertFalse(interval.endOpenContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 10.0)));

    /*
     * Test singleton intervals that are just outside, on the boundary, and inside.
     */
    assertFalse(interval
        .endOpenContains(new UnwritableInterval(-5.0 - Math.ulp(-5.0), -5.0 - Math.ulp(-5.0))));
    assertTrue(interval.endOpenContains(new UnwritableInterval(-5.0, -5.0)));
    assertTrue(interval.endOpenContains(new UnwritableInterval(0.0, 0.0)));
    assertFalse(interval.endOpenContains(new UnwritableInterval(5.0, 5.0)));
    assertFalse(
        interval.endOpenContains(new UnwritableInterval(5.0 + Math.ulp(5.0), 5.0 + Math.ulp(5.0))));

    /*
     * Test intervals that cover, are interior, and touch the left and right boundary but are
     * included.
     */
    assertFalse(interval.endOpenContains(new UnwritableInterval(-5.0, 5.0)));
    assertTrue(interval.endOpenContains(new UnwritableInterval(-1.0, 1.0)));
    assertTrue(interval.endOpenContains(new UnwritableInterval(-5.0, 0.0)));
    assertFalse(interval.endOpenContains(new UnwritableInterval(0.0, 5.0)));

    /*
     * Test intervals that have coverage inside and outside the interval.
     */
    assertFalse(interval.endOpenContains(new UnwritableInterval(-10.0, 0.0)));
    assertFalse(interval.endOpenContains(new UnwritableInterval(0, 10.0)));

    /*
     * Test the singleton for inclusion of it's point.
     */
    assertFalse(singleton.endOpenContains(new UnwritableInterval(5.0, 5.0)));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testClosedIntersectsDoubleDoubleException() {
    interval.closedIntersects(10.0, 5.0);
  }

  @Test
  public void testClosedIntersectsDoubleDouble() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.closedIntersects(-105, -100));
    // 2
    assertFalse(interval.closedIntersects(-105, -5.0 - Math.ulp(-5.0)));
    // 3
    assertTrue(interval.closedIntersects(-105, -5.0));
    // 4
    assertTrue(interval.closedIntersects(-105, -5.0 + Math.ulp(-5.0)));
    // 5
    assertTrue(interval.closedIntersects(-105, 0));
    // 6
    assertTrue(interval.closedIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertTrue(interval.closedIntersects(-105, 5.0));
    // 8
    assertTrue(interval.closedIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertTrue(interval.closedIntersects(-105, 105));
    // 10
    assertTrue(interval.closedIntersects(-5.0 - Math.ulp(-5.0), 105));
    // 11
    assertTrue(interval.closedIntersects(-5.0, 105));
    // 12
    assertTrue(interval.closedIntersects(-5.0 + Math.ulp(-5.0), 105));
    // 13
    assertTrue(interval.closedIntersects(0, 105));
    // 14
    assertTrue(interval.closedIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertTrue(interval.closedIntersects(5.0, 105));
    // 16
    assertFalse(interval.closedIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(interval.closedIntersects(100, 105));
    // 18
    assertTrue(interval.closedIntersects(-5.0, -5.0));
    // 19
    assertTrue(interval.closedIntersects(5.0, 5.0));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.closedIntersects(-105, -100));
    // 6
    assertFalse(singleton.closedIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertTrue(singleton.closedIntersects(-105, 5.0));
    // 8
    assertTrue(singleton.closedIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertTrue(singleton.closedIntersects(-105, 105));
    // 14
    assertTrue(singleton.closedIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertTrue(singleton.closedIntersects(5.0, 105));
    // 16
    assertFalse(singleton.closedIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(singleton.closedIntersects(100, 105));
    // 19
    assertTrue(singleton.closedIntersects(5.0, 5.0));

  }

  @Test
  public void testClosedIntersectsUnwritableInterval() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.closedIntersects(new UnwritableInterval(-105, -100)));
    // 2
    assertFalse(interval.closedIntersects(new UnwritableInterval(-105, -5.0 - Math.ulp(-5.0))));
    // 3
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, -5.0)));
    // 4
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, -5.0 + Math.ulp(-5.0))));
    // 5
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, 0)));
    // 6
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertTrue(interval.closedIntersects(new UnwritableInterval(-105, 105)));
    // 10
    assertTrue(interval.closedIntersects(new UnwritableInterval(-5.0 - Math.ulp(-5.0), 105)));
    // 11
    assertTrue(interval.closedIntersects(new UnwritableInterval(-5.0, 105)));
    // 12
    assertTrue(interval.closedIntersects(new UnwritableInterval(-5.0 + Math.ulp(-5.0), 105)));
    // 13
    assertTrue(interval.closedIntersects(new UnwritableInterval(0, 105)));
    // 14
    assertTrue(interval.closedIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertTrue(interval.closedIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(interval.closedIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(interval.closedIntersects(new UnwritableInterval(100, 105)));
    // 18
    assertTrue(interval.closedIntersects(new UnwritableInterval(-5.0, -5.0)));
    // 19
    assertTrue(interval.closedIntersects(new UnwritableInterval(5.0, 5.0)));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.closedIntersects(new UnwritableInterval(-105, -100)));
    // 6
    assertFalse(singleton.closedIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertTrue(singleton.closedIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertTrue(singleton.closedIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertTrue(singleton.closedIntersects(new UnwritableInterval(-105, 105)));
    // 14
    assertTrue(singleton.closedIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertTrue(singleton.closedIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(singleton.closedIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(singleton.closedIntersects(new UnwritableInterval(100, 105)));
    // 19
    assertTrue(singleton.closedIntersects(new UnwritableInterval(5.0, 5.0)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testOpenIntersectsDoubleDoubleException() {
    interval.openIntersects(10.0, 5.0);
  }

  @Test
  public void testOpenIntersectsDoubleDouble() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.openIntersects(-105, -100));
    // 2
    assertFalse(interval.openIntersects(-105, -5.0 - Math.ulp(-5.0)));
    // 3
    assertFalse(interval.openIntersects(-105, -5.0));
    // 4
    assertTrue(interval.openIntersects(-105, -5.0 + Math.ulp(-5.0)));
    // 5
    assertTrue(interval.openIntersects(-105, 0));
    // 6
    assertTrue(interval.openIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertTrue(interval.openIntersects(-105, 5.0));
    // 8
    assertTrue(interval.openIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertTrue(interval.openIntersects(-105, 105));
    // 10
    assertTrue(interval.openIntersects(-5.0 - Math.ulp(-5.0), 105));
    // 11
    assertTrue(interval.openIntersects(-5.0, 105));
    // 12
    assertTrue(interval.openIntersects(-5.0 + Math.ulp(-5.0), 105));
    // 13
    assertTrue(interval.openIntersects(0, 105));
    // 14
    assertTrue(interval.openIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertFalse(interval.openIntersects(5.0, 105));
    // 16
    assertFalse(interval.openIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(interval.openIntersects(100, 105));
    // 18
    assertFalse(interval.openIntersects(-5.0, -5.0));
    // 19
    assertFalse(interval.openIntersects(5.0, 5.0));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.openIntersects(-105, -100));
    // 6
    assertFalse(singleton.openIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertFalse(singleton.openIntersects(-105, 5.0));
    // 8
    assertFalse(singleton.openIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertFalse(singleton.openIntersects(-105, 105));
    // 14
    assertFalse(singleton.openIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertFalse(singleton.openIntersects(5.0, 105));
    // 16
    assertFalse(singleton.openIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(singleton.openIntersects(100, 105));
    // 19
    assertFalse(singleton.openIntersects(5.0, 5.0));

  }

  @Test
  public void testOpenIntersectsUnwritableInterval() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.openIntersects(new UnwritableInterval(-105, -100)));
    // 2
    assertFalse(interval.openIntersects(new UnwritableInterval(-105, -5.0 - Math.ulp(-5.0))));
    // 3
    assertFalse(interval.openIntersects(new UnwritableInterval(-105, -5.0)));
    // 4
    assertTrue(interval.openIntersects(new UnwritableInterval(-105, -5.0 + Math.ulp(-5.0))));
    // 5
    assertTrue(interval.openIntersects(new UnwritableInterval(-105, 0)));
    // 6
    assertTrue(interval.openIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertTrue(interval.openIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertTrue(interval.openIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertTrue(interval.openIntersects(new UnwritableInterval(-105, 105)));
    // 10
    assertTrue(interval.openIntersects(new UnwritableInterval(-5.0 - Math.ulp(-5.0), 105)));
    // 11
    assertTrue(interval.openIntersects(new UnwritableInterval(-5.0, 105)));
    // 12
    assertTrue(interval.openIntersects(new UnwritableInterval(-5.0 + Math.ulp(-5.0), 105)));
    // 13
    assertTrue(interval.openIntersects(new UnwritableInterval(0, 105)));
    // 14
    assertTrue(interval.openIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertFalse(interval.openIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(interval.openIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(interval.openIntersects(new UnwritableInterval(100, 105)));
    // 18
    assertFalse(interval.openIntersects(new UnwritableInterval(-5.0, -5.0)));
    // 19
    assertFalse(interval.openIntersects(new UnwritableInterval(5.0, 5.0)));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.openIntersects(new UnwritableInterval(-105, -100)));
    // 6
    assertFalse(singleton.openIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertFalse(singleton.openIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertFalse(singleton.openIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertFalse(singleton.openIntersects(new UnwritableInterval(-105, 105)));
    // 14
    assertFalse(singleton.openIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertFalse(singleton.openIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(singleton.openIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(singleton.openIntersects(new UnwritableInterval(100, 105)));
    // 19
    assertFalse(singleton.openIntersects(new UnwritableInterval(5.0, 5.0)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testBeginOpenIntersectsDoubleDoubleException() {
    interval.beginOpenIntersects(10.0, 5.0);
  }

  @Test
  public void testBeginOpenIntersectsDoubleDouble() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.beginOpenIntersects(-105, -100));
    // 2
    assertFalse(interval.beginOpenIntersects(-105, -5.0 - Math.ulp(-5.0)));
    // 3
    assertFalse(interval.beginOpenIntersects(-105, -5.0));
    // 4
    assertTrue(interval.beginOpenIntersects(-105, -5.0 + Math.ulp(-5.0)));
    // 5
    assertTrue(interval.beginOpenIntersects(-105, 0));
    // 6
    assertTrue(interval.beginOpenIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertTrue(interval.beginOpenIntersects(-105, 5.0));
    // 8
    assertTrue(interval.beginOpenIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertTrue(interval.beginOpenIntersects(-105, 105));
    // 10
    assertTrue(interval.beginOpenIntersects(-5.0 - Math.ulp(-5.0), 105));
    // 11
    assertTrue(interval.beginOpenIntersects(-5.0, 105));
    // 12
    assertTrue(interval.beginOpenIntersects(-5.0 + Math.ulp(-5.0), 105));
    // 13
    assertTrue(interval.beginOpenIntersects(0, 105));
    // 14
    assertTrue(interval.beginOpenIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertTrue(interval.beginOpenIntersects(5.0, 105));
    // 16
    assertFalse(interval.beginOpenIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(interval.beginOpenIntersects(100, 105));
    // 18
    assertFalse(interval.beginOpenIntersects(-5.0, -5.0));
    // 19
    assertTrue(interval.beginOpenIntersects(5.0, 5.0));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.beginOpenIntersects(-105, -100));
    // 6
    assertFalse(singleton.beginOpenIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertFalse(singleton.beginOpenIntersects(-105, 5.0));
    // 8
    assertFalse(singleton.beginOpenIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertFalse(singleton.beginOpenIntersects(-105, 105));
    // 14
    assertFalse(singleton.beginOpenIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertFalse(singleton.beginOpenIntersects(5.0, 105));
    // 16
    assertFalse(singleton.beginOpenIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(singleton.beginOpenIntersects(100, 105));
    // 19
    assertFalse(singleton.beginOpenIntersects(5.0, 5.0));

  }

  @Test
  public void testBeginOpenIntersectsUnwritableInterval() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.beginOpenIntersects(new UnwritableInterval(-105, -100)));
    // 2
    assertFalse(interval.beginOpenIntersects(new UnwritableInterval(-105, -5.0 - Math.ulp(-5.0))));
    // 3
    assertFalse(interval.beginOpenIntersects(new UnwritableInterval(-105, -5.0)));
    // 4
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-105, -5.0 + Math.ulp(-5.0))));
    // 5
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-105, 0)));
    // 6
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-105, 105)));
    // 10
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-5.0 - Math.ulp(-5.0), 105)));
    // 11
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-5.0, 105)));
    // 12
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(-5.0 + Math.ulp(-5.0), 105)));
    // 13
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(0, 105)));
    // 14
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(interval.beginOpenIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(interval.beginOpenIntersects(new UnwritableInterval(100, 105)));
    // 18
    assertFalse(interval.beginOpenIntersects(new UnwritableInterval(-5.0, -5.0)));
    // 19
    assertTrue(interval.beginOpenIntersects(new UnwritableInterval(5.0, 5.0)));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(-105, -100)));
    // 6
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(-105, 105)));
    // 14
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(100, 105)));
    // 19
    assertFalse(singleton.beginOpenIntersects(new UnwritableInterval(5.0, 5.0)));

  }

  @Test(expected = IllegalArgumentException.class)
  public void testEndOpenIntersectsDoubleDoubleException() {
    interval.endOpenIntersects(10.0, 5.0);
  }

  @Test
  public void testEndOpenIntersectsDoubleDouble() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.endOpenIntersects(-105, -100));
    // 2
    assertFalse(interval.endOpenIntersects(-105, -5.0 - Math.ulp(-5.0)));
    // 3
    assertTrue(interval.endOpenIntersects(-105, -5.0));
    // 4
    assertTrue(interval.endOpenIntersects(-105, -5.0 + Math.ulp(-5.0)));
    // 5
    assertTrue(interval.endOpenIntersects(-105, 0));
    // 6
    assertTrue(interval.endOpenIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertTrue(interval.endOpenIntersects(-105, 5.0));
    // 8
    assertTrue(interval.endOpenIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertTrue(interval.endOpenIntersects(-105, 105));
    // 10
    assertTrue(interval.endOpenIntersects(-5.0 - Math.ulp(-5.0), 105));
    // 11
    assertTrue(interval.endOpenIntersects(-5.0, 105));
    // 12
    assertTrue(interval.endOpenIntersects(-5.0 + Math.ulp(-5.0), 105));
    // 13
    assertTrue(interval.endOpenIntersects(0, 105));
    // 14
    assertTrue(interval.endOpenIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertFalse(interval.endOpenIntersects(5.0, 105));
    // 16
    assertFalse(interval.endOpenIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(interval.endOpenIntersects(100, 105));
    // 18
    assertTrue(interval.endOpenIntersects(-5.0, -5.0));
    // 19
    assertFalse(interval.endOpenIntersects(5.0, 5.0));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.endOpenIntersects(-105, -100));
    // 6
    assertFalse(singleton.endOpenIntersects(-105, 5.0 - Math.ulp(5.0)));
    // 7
    assertFalse(singleton.endOpenIntersects(-105, 5.0));
    // 8
    assertFalse(singleton.endOpenIntersects(-105, 5.0 + Math.ulp(5.0)));
    // 9
    assertFalse(singleton.endOpenIntersects(-105, 105));
    // 14
    assertFalse(singleton.endOpenIntersects(5.0 - Math.ulp(5.0), 105));
    // 15
    assertFalse(singleton.endOpenIntersects(5.0, 105));
    // 16
    assertFalse(singleton.endOpenIntersects(5.0 + Math.ulp(5.0), 105));
    // 17
    assertFalse(singleton.endOpenIntersects(100, 105));
    // 19
    assertFalse(singleton.endOpenIntersects(5.0, 5.0));

  }

  @Test
  public void testEndOpenIntersectsUnwritableInterval() {

    // For 9 possible locations: beg-far, beg-near-out, beg-exact,
    // beg-near-in, middle, end-near-in, end-exact, end-near-out, end-far

    // 19 Tests:
    // 1) beg-far, beg-far
    // 2) beg-far, beg-near-out
    // 3) beg-far, beg-exact
    // 4) beg-far, beg-near-in
    // 5) beg-far, middle
    // 6) beg-far, end-near-in
    // 7) beg-far, end-exact
    // 8) beg-far, end-near-out
    // 9) beg-far, end-far
    // 10) beg-near-out, end-far
    // 11) beg-exact, end-far
    // 12) beg-near-in, end-far
    // 13) middle, end-far
    // 14) end-near-in, end-far
    // 15) end-exact, end-far
    // 16) end-near-out, end-far
    // 17) end-far, end-far
    // 18) singleton at beg-exact
    // 19) singleton at end-exact

    // INTERVAL

    // 1
    assertFalse(interval.endOpenIntersects(new UnwritableInterval(-105, -100)));
    // 2
    assertFalse(interval.endOpenIntersects(new UnwritableInterval(-105, -5.0 - Math.ulp(-5.0))));
    // 3
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, -5.0)));
    // 4
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, -5.0 + Math.ulp(-5.0))));
    // 5
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, 0)));
    // 6
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-105, 105)));
    // 10
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-5.0 - Math.ulp(-5.0), 105)));
    // 11
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-5.0, 105)));
    // 12
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-5.0 + Math.ulp(-5.0), 105)));
    // 13
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(0, 105)));
    // 14
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertFalse(interval.endOpenIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(interval.endOpenIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(interval.endOpenIntersects(new UnwritableInterval(100, 105)));
    // 18
    assertTrue(interval.endOpenIntersects(new UnwritableInterval(-5.0, -5.0)));
    // 19
    assertFalse(interval.endOpenIntersects(new UnwritableInterval(5.0, 5.0)));

    // SINGLETON (excludes redundant tests)

    // 1
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(-105, -100)));
    // 6
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(-105, 5.0 - Math.ulp(5.0))));
    // 7
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(-105, 5.0)));
    // 8
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(-105, 5.0 + Math.ulp(5.0))));
    // 9
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(-105, 105)));
    // 14
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(5.0 - Math.ulp(5.0), 105)));
    // 15
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(5.0, 105)));
    // 16
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(5.0 + Math.ulp(5.0), 105)));
    // 17
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(100, 105)));
    // 19
    assertFalse(singleton.endOpenIntersects(new UnwritableInterval(5.0, 5.0)));

  }

}
