package picante.spice.kernel.ck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.data.list.AbstractGaugedRetrievable;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.TimeOutOfBoundsException;

public class IntervalStartsCKCoverageTest {

  private IntervalStartsCKCoverage singleSingleton;
  private IntervalStartsCKCoverage singleInterval;
  private IntervalStartsCKCoverage multipleSingleton;
  private IntervalStartsCKCoverage multipleInterval;
  private IntervalStartsCKCoverage mixed;

  private Interval interval;

  @Before
  public void setUp() throws Exception {

    interval = new Interval();

    singleSingleton = createCoverage(10.0, 10.0, Arrays.asList(0.0), 0);

    singleInterval = createCoverage(10.0, 10.0, Arrays.asList(0.0, 10.0), 0);

    multipleSingleton = createCoverage(0.0, 0.0, Arrays.asList(0.0, 1.0, 2.0, 3.0), 0, 1, 2, 3);

    multipleInterval = createCoverage(0.0, 5.0,
        Arrays.asList(0.0, 3.0, 6.0, 10.0, 15.0, 20.0, 23.0, 26.0), 0, 3, 5);

    mixed =
        createCoverage(5.0, 0.0, Arrays.asList(0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0), 0, 2, 3, 6);

  }

  /**
   * Creates a coverage instance for testing.
   * 
   * @param startDelta the amount to subtract from the first record time to declare as the start of
   *        the coverage
   * @param endDelta the amount to add to the last record time to declare as the end of the coverage
   * @param records the list of times for each record
   * @param intervalIndices the indices into the records array that indicate the start of
   *        interpolation intervals
   * 
   * @return a newly constructed interval coverage instance for testing
   */
  private static IntervalStartsCKCoverage createCoverage(double startDelta, double endDelta,
      List<Double> records, int... intervalIndices) {

    List<Double> intervals = new ArrayList<Double>(intervalIndices.length);

    for (int index : intervalIndices) {
      intervals.add(records.get(index));
    }

    return new IntervalStartsCKCoverage(records.get(0) - startDelta,
        records.get(records.size() - 1) + endDelta, new ListRecordTable(records),
        new ListRecordTable(intervals));

  }

  private static void assertIntervalEquals(double expectedStart, double expectedEnd,
      UnwritableInterval actual) {
    assertEquals(new UnwritableInterval(expectedStart, expectedEnd), actual);
  }

  @Test
  public void testSSContains() {
    assertFalse(singleSingleton.contains(-500.0));
    assertFalse(singleSingleton.contains(-5.0));
    assertTrue(singleSingleton.contains(0.0));
    assertFalse(singleSingleton.contains(5.0));
    assertFalse(singleSingleton.contains(20.0));
  }

  @Test
  public void testSSGetBoundingInterval() {
    Interval result = singleSingleton.getBoundingInterval(interval);
    assertSame(result, interval);
    assertIntervalEquals(-10, 10, result);
  }

  @Test
  public void testSSGetBracketingInterval() {
    Interval result = singleSingleton.getBracketingInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 0.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSSGetBracketingIntervalLeftOfCoverageException() {
    singleSingleton.getBracketingInterval(-500, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSSGetBracketingIntervalPriorException() {
    singleSingleton.getBracketingInterval(-5.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSSGetBracketingIntervalAfterException() {
    singleSingleton.getBracketingInterval(5.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSSGetBracketingIntervalRightOfCoverageException() {
    singleSingleton.getBracketingInterval(500, interval);
  }

  @Test
  public void testSSHasNextInterval() {
    assertTrue(singleSingleton.hasNextInterval(-10));
    assertTrue(singleSingleton.hasNextInterval(-0.1));
    assertFalse(singleSingleton.hasNextInterval(0.0));
    assertFalse(singleSingleton.hasNextInterval(20));
  }

  @Test
  public void testSSGetNextInterval() {
    Interval result = singleSingleton.getNextInterval(-20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 0.0, result);

    result = singleSingleton.getNextInterval(-1.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 0.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSSGetNextIntervalException() {
    singleSingleton.getNextInterval(0.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSSGetNextIntervalPastEndException() {
    singleSingleton.getNextInterval(5.0, interval);
  }

  @Test
  public void testSIContains() {
    assertFalse(singleInterval.contains(-500.0));
    assertFalse(singleInterval.contains(-5.0));
    assertTrue(singleInterval.contains(0.0));
    assertTrue(singleInterval.contains(5.0));
    assertTrue(singleInterval.contains(10.0));
    assertFalse(singleInterval.contains(15.0));
    assertFalse(singleInterval.contains(20.0));
  }

  @Test
  public void testSIGetBoundingInterval() {
    Interval result = singleInterval.getBoundingInterval(interval);
    assertSame(result, interval);
    assertIntervalEquals(-10, 20, result);
  }

  @Test
  public void testSIGetBracketingIntervalAtLeft() {
    Interval result = singleInterval.getBracketingInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 10.0, result);
  }

  @Test
  public void testSIGetBracketingIntervalInMiddle() {
    Interval result = singleInterval.getBracketingInterval(5.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 10.0, result);
  }

  @Test
  public void testSIGetBracketingIntervalAtRight() {
    Interval result = singleInterval.getBracketingInterval(10.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 10.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetBracketingIntervalLeftOfCoverageException() {
    singleInterval.getBracketingInterval(-500, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetBracketingIntervalPriorException() {
    singleInterval.getBracketingInterval(-5.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetBracketingIntervalAfterException() {
    singleInterval.getBracketingInterval(11.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetBracketingIntervalRightOfCoverageException() {
    singleInterval.getBracketingInterval(500, interval);
  }

  @Test
  public void testSIHasNextInterval() {
    assertTrue(singleInterval.hasNextInterval(-10.0));
    assertTrue(singleInterval.hasNextInterval(-0.01));
    assertFalse(singleInterval.hasNextInterval(0.0));
    assertFalse(singleInterval.hasNextInterval(10.0));
    assertFalse(singleInterval.hasNextInterval(20));
  }

  @Test
  public void testSIGetNextInterval() {
    Interval result = singleInterval.getNextInterval(-20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 10.0, result);

    interval = new Interval();
    result = singleInterval.getNextInterval(-1.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 10.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetNextIntervalAtLeftException() {
    singleInterval.getNextInterval(0.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetNextIntervalInMiddleException() {
    singleInterval.getNextInterval(5.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetNextIntervalAtRightException() {
    singleInterval.getNextInterval(10.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testSIGetNextIntervalPastEndException() {
    singleInterval.getNextInterval(11.0, interval);
  }

  @Test
  public void testMSContains() {
    assertFalse(multipleSingleton.contains(-500.0));
    assertFalse(multipleSingleton.contains(-5.0));
    assertTrue(multipleSingleton.contains(0.0));
    assertFalse(multipleSingleton.contains(0.5));
    assertTrue(multipleSingleton.contains(1.0));
    assertFalse(multipleSingleton.contains(1.5));
    assertTrue(multipleSingleton.contains(2.0));
    assertFalse(multipleSingleton.contains(2.5));
    assertTrue(multipleSingleton.contains(3.0));
    assertFalse(multipleSingleton.contains(5.0));
    assertFalse(multipleSingleton.contains(20.0));
  }

  @Test
  public void testMSGetBoundingInterval() {
    Interval result = multipleSingleton.getBoundingInterval(interval);
    assertSame(result, interval);
    assertIntervalEquals(0, 3, result);
  }

  @Test
  public void testMSGetBracketingInterval() {
    Interval result = multipleSingleton.getBracketingInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 0.0, result);

    interval = new Interval();
    result = multipleSingleton.getBracketingInterval(1.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(1.0, 1.0, result);

    interval = new Interval();
    result = multipleSingleton.getBracketingInterval(2.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(2.0, 2.0, result);

    interval = new Interval();
    result = multipleSingleton.getBracketingInterval(3.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(3.0, 3.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetBracketingIntervalLeftOfCoverageException() {
    multipleSingleton.getBracketingInterval(-500, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetBracketingIntervalBetween01Exception() {
    multipleSingleton.getBracketingInterval(0.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetBracketingIntervalBetween12Exception() {
    multipleSingleton.getBracketingInterval(1.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetBracketingIntervalBetween23Exception() {
    multipleSingleton.getBracketingInterval(2.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetBracketingIntervalRightOfCoverageException() {
    multipleSingleton.getBracketingInterval(500, interval);
  }

  @Test
  public void testMSHasNextInterval() {
    assertTrue(multipleSingleton.hasNextInterval(-20));
    assertTrue(multipleSingleton.hasNextInterval(2.0));
    assertTrue(multipleSingleton.hasNextInterval(2.9));
    assertFalse(multipleSingleton.hasNextInterval(3.0));
    assertFalse(multipleSingleton.hasNextInterval(3.1));

  }

  @Test
  public void testMSGetNextInterval() {
    Interval result = multipleSingleton.getNextInterval(-20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 0.0, result);

    interval = new Interval();
    result = multipleSingleton.getNextInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(1.0, 1.0, result);

    interval = new Interval();
    result = multipleSingleton.getNextInterval(0.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(1.0, 1.0, result);

    interval = new Interval();
    result = multipleSingleton.getNextInterval(1.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(2.0, 2.0, result);

    interval = new Interval();
    result = multipleSingleton.getNextInterval(1.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(2.0, 2.0, result);

    interval = new Interval();
    result = multipleSingleton.getNextInterval(2.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(3.0, 3.0, result);

    interval = new Interval();
    result = multipleSingleton.getNextInterval(2.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(3.0, 3.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetNextIntervalException() {
    multipleSingleton.getNextInterval(3.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMSGetNextIntervalPastEndException() {
    multipleSingleton.getNextInterval(3.5, interval);
  }

  @Test
  public void testMIContains() {
    assertFalse(multipleInterval.contains(-500.0));
    assertFalse(multipleInterval.contains(-5.0));
    assertTrue(multipleInterval.contains(0.0));
    assertTrue(multipleInterval.contains(1.5));
    assertTrue(multipleInterval.contains(3.0));
    assertTrue(multipleInterval.contains(4.5));
    assertTrue(multipleInterval.contains(6.0));
    assertFalse(multipleInterval.contains(7.5));
    assertTrue(multipleInterval.contains(10.0));
    assertTrue(multipleInterval.contains(12.5));
    assertTrue(multipleInterval.contains(15.0));
    assertFalse(multipleInterval.contains(17.5));
    assertTrue(multipleInterval.contains(20.0));
    assertTrue(multipleInterval.contains(21.5));
    assertTrue(multipleInterval.contains(23.0));
    assertTrue(multipleInterval.contains(24.5));
    assertTrue(multipleInterval.contains(26.0));
    assertFalse(multipleInterval.contains(27.5));
    assertFalse(multipleInterval.contains(31.5));
  }

  @Test
  public void testMIGetBoundingInterval() {
    Interval result = multipleInterval.getBoundingInterval(interval);
    assertSame(result, interval);
    assertIntervalEquals(0, 31, result);
  }

  @Test
  public void testMIGetBracketingInterval() {
    Interval result = multipleInterval.getBracketingInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 6.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(1.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 6.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(3.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 6.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(4.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 6.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(6.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 6.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(10.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(12.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(15.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(21.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(23.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(24.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getBracketingInterval(26.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetBracketingIntervalLeftOfCoverageException() {
    multipleInterval.getBracketingInterval(-500, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetBracketingIntervalBetween610Exception() {
    multipleInterval.getBracketingInterval(8.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetBracketingIntervalBetween1520Exception() {
    multipleInterval.getBracketingInterval(17.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetBracketingIntervalAfter26Exception() {
    multipleInterval.getBracketingInterval(27.8, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetBracketingIntervalRightOfCoverageException() {
    multipleInterval.getBracketingInterval(500, interval);
  }

  @Test
  public void testMIHasNextInterval() {
    assertTrue(multipleInterval.hasNextInterval(-10));
    assertTrue(multipleInterval.hasNextInterval(19.9));
    assertFalse(multipleInterval.hasNextInterval(20.0));
    assertFalse(multipleInterval.hasNextInterval(30.0));
  }

  @Test
  public void testMIGetNextInterval() {
    Interval result = multipleInterval.getNextInterval(-20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 6.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(1.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(3.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(4.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(6.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(7.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 15.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(10.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(12.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(15.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);

    interval = new Interval();
    result = multipleInterval.getNextInterval(17.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(20.0, 26.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetNextIntervalException() {
    multipleInterval.getNextInterval(27.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMIGetNextIntervalPastEndException() {
    multipleInterval.getNextInterval(31.5, interval);
  }

  @Test
  public void testMXContains() {
    assertFalse(mixed.contains(-500.0));
    assertFalse(mixed.contains(-5.0));
    assertTrue(mixed.contains(0.0));
    assertTrue(mixed.contains(2.5));
    assertTrue(mixed.contains(5.0));
    assertFalse(mixed.contains(7.5));
    assertTrue(mixed.contains(10.0));
    assertFalse(mixed.contains(12.5));
    assertTrue(mixed.contains(15.0));
    assertTrue(mixed.contains(17.5));
    assertTrue(mixed.contains(20.0));
    assertTrue(mixed.contains(22.5));
    assertTrue(mixed.contains(25.0));
    assertFalse(mixed.contains(27.5));
    assertTrue(mixed.contains(30.0));
    assertFalse(mixed.contains(500.0));
  }

  @Test
  public void testMXGetBoundingInterval() {
    Interval result = mixed.getBoundingInterval(interval);
    assertSame(result, interval);
    assertIntervalEquals(-5, 30, result);
  }

  @Test
  public void testMXGetBracketingInterval() {
    Interval result = mixed.getBracketingInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 5.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(2.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 5.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(5.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 5.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(10.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 10.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(15.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(17.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(22.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(25.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getBracketingInterval(30.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetBracketingIntervalLeftOfCoverageException() {
    mixed.getBracketingInterval(-500, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetBracketingIntervalLeftPriorToCoverageException() {
    mixed.getBracketingInterval(-7.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetBracketingIntervalBetween510Exception() {
    mixed.getBracketingInterval(7.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetBracketingIntervalBetween1015Exception() {
    mixed.getBracketingInterval(12.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetBracketingIntervalBetween2530Exception() {
    mixed.getBracketingInterval(27.5, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetBracketingIntervalRightOverCoverageException() {
    mixed.getBracketingInterval(35.0, interval);
  }

  @Test
  public void testMXHasNextInterval() {
    assertTrue(mixed.hasNextInterval(-1));
    assertTrue(mixed.hasNextInterval(0));
    assertTrue(mixed.hasNextInterval(29.9));
    assertFalse(mixed.hasNextInterval(30.0));
    assertFalse(mixed.hasNextInterval(35.0));
  }

  @Test
  public void testMXGetNextInterval() {
    Interval result = mixed.getNextInterval(-500.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 5.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(-5.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 5.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(-2.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(0.0, 5.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(0.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 10.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(2.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 10.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(5.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 10.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(7.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(10.0, 10.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(10.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(12.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(15.0, 25.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(15.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(17.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(20.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(22.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(25.0, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);

    interval = new Interval();
    result = mixed.getNextInterval(27.5, interval);
    assertSame(result, interval);
    assertIntervalEquals(30.0, 30.0, result);

  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetNextIntervalException() {
    mixed.getNextInterval(30.0, interval);
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testMXGetNextIntervalPastEndException() {
    mixed.getNextInterval(35.0, interval);
  }

}


/**
 * Simple test class to provide an implementation of the record table interface with appropriate
 * times driven from a list of doubles.
 */
class ListRecordTable extends AbstractGaugedRetrievable<Object> {

  private List<Double> records;

  public ListRecordTable(List<Double> records) {
    this.records = new ArrayList<Double>(records);
  }

  @Override
  public int size() {
    return records.size();
  }

  @Override
  public Object get(@SuppressWarnings("unused") int index,
      @SuppressWarnings("unused") Object buffer) {
    throw new UnsupportedOperationException();
  }

  @Override
  public double getGauge(int index) {
    return records.get(index);
  }

}
