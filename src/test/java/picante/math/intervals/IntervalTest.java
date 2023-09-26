package picante.math.intervals;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static picante.math.intervals.Interval.ALL_DOUBLES;
import static picante.math.intervals.Interval.BEGIN_COMPARATOR;
import static picante.math.intervals.Interval.BEGIN_EXTRACTOR;
import static picante.math.intervals.Interval.END_COMPARATOR;
import static picante.math.intervals.Interval.END_EXTRACTOR;
import static picante.math.intervals.Interval.LENGTH_COMPARATOR;
import static picante.math.intervals.Interval.LENGTH_EXTRACTOR;
import org.junit.Before;
import org.junit.Test;

public class IntervalTest {

  private Interval interval;
  private Interval singleton;
  private Interval anotherInterval;

  @Before
  public void setUp() throws Exception {
    interval = new Interval(-5, 5);
    singleton = new Interval(5, 5);
    anotherInterval = new Interval(0, 5);
  }

  @Test
  public void testInterval() {
    assertEquals(ALL_DOUBLES, new Interval());
  }

  @Test
  public void testIntervalUnwritableInterval() {
    assertEquals(interval, new Interval(interval));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIntervalDoubleDoubleException() {
    new Interval(5, 0);
  }

  @Test
  public void testIntervalDoubleDouble() {
    assertEquals(-5, interval.getBegin(), 0.0);
    assertEquals(5, interval.getEnd(), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetException() {
    interval.set(5, 0);
  }

  @Test
  public void testSet() {
    interval.set(anotherInterval.getBegin(), anotherInterval.getEnd());
    assertEquals(anotherInterval, interval);
  }

  @Test
  public void testSetTo() {
    Interval returned = interval.setTo(singleton);
    assertSame(interval, returned);
    assertEquals(singleton, interval);
  }

  @Test
  public void testAllDoublesField() {
    assertEquals(-Double.MAX_VALUE, ALL_DOUBLES.getBegin(), 0.0);
    assertEquals(Double.MAX_VALUE, ALL_DOUBLES.getEnd(), 0.0);
  }

  @Test
  public void testLengthExtractor() {
    assertEquals(0, LENGTH_EXTRACTOR.apply(singleton), 0.0);
    assertEquals(10.0, LENGTH_EXTRACTOR.apply(interval), 0.0);
  }

  @Test
  public void testBeginExtractor() {
    assertEquals(-5, BEGIN_EXTRACTOR.apply(interval), 0.0);
    assertEquals(5, BEGIN_EXTRACTOR.apply(singleton), 0.0);
  }

  @Test
  public void testEndExtractor() {
    assertEquals(5, END_EXTRACTOR.apply(interval), 0.0);
    assertEquals(5, END_EXTRACTOR.apply(singleton), 0.0);
  }

  @Test
  public void testSetBegin() {
    interval.setBegin(-10);
    assertEquals(-10.0, interval.getBegin(), 0.0);
    assertEquals(5.0, interval.getEnd(), 0.0);

    interval.setBegin(2.0);
    assertEquals(2.0, interval.getBegin(), 0.0);
    assertEquals(5.0, interval.getEnd(), 0.0);

    interval.setBegin(5.0);
    assertEquals(5.0, interval.getBegin(), 0.0);
    assertEquals(5.0, interval.getEnd(), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetBeginException() {
    interval.setBegin(6);
  }

  @Test
  public void testSetEnd() {
    interval.setEnd(20.0);
    assertEquals(-5.0, interval.getBegin(), 0.0);
    assertEquals(20.0, interval.getEnd(), 0.0);

    interval.setEnd(0.0);
    assertEquals(-5.0, interval.getBegin(), 0.0);
    assertEquals(0.0, interval.getEnd(), 0.0);

    interval.setEnd(-5.0);
    assertEquals(-5.0, interval.getBegin(), 0.0);
    assertEquals(-5.0, interval.getEnd(), 0.0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSetEndException() {
    interval.setEnd(-6.0);
  }

  @Test
  public void testLengthComparator() {
    assertTrue(LENGTH_COMPARATOR.compare(singleton, interval) < 0);
    assertTrue(LENGTH_COMPARATOR.compare(singleton, new Interval(singleton)) == 0);
    assertTrue(LENGTH_COMPARATOR.compare(interval, anotherInterval) > 0);
  }

  @Test
  public void testBeginComparator() {
    assertTrue(BEGIN_COMPARATOR.compare(interval, anotherInterval) < 0);
    assertTrue(BEGIN_COMPARATOR.compare(singleton, anotherInterval) > 0);
    assertTrue(BEGIN_COMPARATOR.compare(singleton, new Interval(5, 10)) == 0);
  }

  @Test
  public void testEndComparator() {
    assertTrue(END_COMPARATOR.compare(new Interval(0, 10), new Interval(0, 15)) < 0);
    assertTrue(END_COMPARATOR.compare(singleton, anotherInterval) == 0);
    assertTrue(END_COMPARATOR.compare(new Interval(0, 20), new Interval(5, 10)) > 0);
  }
}
