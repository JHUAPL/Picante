package picante.spice.adapters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.Coverage;
import picante.mechanics.Coverages;
import picante.mechanics.TimeOutOfBoundsException;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

public class TdbCoverageTest {

  private TdbCoverage tdbCoverage;

  private CoverageInterval coverageInterval;
  private EncodedSCLKConverter converter;

  private double startTimeTDB = 8.0;
  private double stopTimeTDB = 12.0;

  @Before
  public void setUp() throws Exception {
    /*
     * This corresponds to an encoded SCLK range of [15,27].
     */
    converter = new Convert(new Interval(7.0, 13.0));
    coverageInterval = new CoverageInterval();
    coverageInterval.setTimes(converter.convertToEncodedSclk(startTimeTDB),
        converter.convertToEncodedSclk(stopTimeTDB));
    tdbCoverage = new TdbCoverage(coverageInterval, converter);

  }

  @Test
  public void testContains() {

    double tdbTime = 10.0;

    assertEquals(true, tdbCoverage.contains(tdbTime));

    tdbTime = 5.0;

    assertEquals(false, tdbCoverage.contains(tdbTime));

    tdbTime = 15.0;

    assertEquals(false, tdbCoverage.contains(tdbTime));
  }

  @Test
  public void testGetBoundingInterval() {
    assertEquals(new Interval(startTimeTDB, stopTimeTDB),
        tdbCoverage.getBoundingInterval(new Interval()));
  }

  public void testGetBracketingInterval() {
    double tdbTime = 10.0;
    assertEquals(new Interval(startTimeTDB, stopTimeTDB),
        tdbCoverage.getBracketingInterval(tdbTime, new Interval()));
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetBracketingIntervalFail1() {
    double tdbTime = 5.0;
    tdbCoverage.getBracketingInterval(tdbTime, new Interval());
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetBracketingIntervalFail2() {
    double tdbTime = 15.0;
    tdbCoverage.getBracketingInterval(tdbTime, new Interval());
  }

  @Test
  public void testHasNextInterval() {
    assertTrue(tdbCoverage.hasNextInterval(-20.0));
    assertTrue(tdbCoverage.hasNextInterval(7.999));
    assertFalse(tdbCoverage.hasNextInterval(8.0));
    assertFalse(tdbCoverage.hasNextInterval(12.0));
    assertFalse(tdbCoverage.hasNextInterval(20.0));

  }

  @Test
  public void testGetNextIntervalPrecedesSupportedConversionRange() {}

  @Test
  public void testGetNextInterval() {
    double tdbTime = 5.0;
    assertEquals(new Interval(startTimeTDB, stopTimeTDB),
        tdbCoverage.getNextInterval(tdbTime, new Interval()));
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetNextIntervalFail1() {
    double tdbTime = 10.0;
    assertEquals(new Interval(startTimeTDB, stopTimeTDB),
        tdbCoverage.getNextInterval(tdbTime, new Interval()));
  }

  @Test(expected = TimeOutOfBoundsException.class)
  public void testGetNextIntervalFail2() {
    double tdbTime = 15.0;
    assertEquals(new Interval(startTimeTDB, stopTimeTDB),
        tdbCoverage.getNextInterval(tdbTime, new Interval()));
  }

  @Test
  public void testCovertToTDB() {
    assertEquals(new Interval(converter.convertToTDB(16.0), converter.convertToTDB(24.0)),
        tdbCoverage.convertToTDB(new Interval(16.0, 24.0)));
  }

  class Convert implements EncodedSCLKConverter {

    private final UnwritableInterval tdbRange;
    private final UnwritableInterval sclkRange;

    public Convert(UnwritableInterval tdbRange) {
      this.tdbRange = new UnwritableInterval(tdbRange);
      this.sclkRange = new UnwritableInterval(convertToEncodedSclk(tdbRange.getBegin()),
          convertToEncodedSclk(tdbRange.getEnd()));
    }

    @Override
    public double convertToTDB(double encodedSCLK) {
      if (!sclkRange.closedContains(encodedSCLK)) {
        throw new UnsupportedOperationException();
      }
      return ((encodedSCLK - 1.0) / 2.0);

    }

    @Override
    public double convertToEncodedSclk(double tdb) {
      if (!tdbRange.closedContains(tdb)) {
        throw new UnsupportedOperationException();
      }
      return (tdb * 2.0 + 1.0);
    }

    @Override
    public Interval getTDBRange(Interval buffer) {
      return buffer.setTo(this.tdbRange);
    }

    @Override
    public Interval getEncodedSclkRange(Interval buffer) {
      return buffer.setTo(this.sclkRange);
    }

  }

  class CoverageInterval implements Coverage {

    private double start;
    private double stop;

    public CoverageInterval() {

    }

    public CoverageInterval(double time) {
      this.start = time;
      this.stop = time;
    }

    public void setTimes(double start, double stop) {
      this.start = start;
      this.stop = stop;
    }

    @Override
    public boolean contains(double time) {
      return (time >= start) && (time <= stop);
    }

    @Override
    public Interval getBoundingInterval(Interval buffer) {
      buffer.set(start, stop);
      return buffer;
    }

    @Override
    public boolean hasNextInterval(double time) {
      return time < start;
    }

    @Override
    public Interval getBracketingInterval(double t, Interval buffer) {
      if (contains(t)) {
        buffer.set(start, stop);
        return buffer;
      } else {
        throw new TimeOutOfBoundsException();
      }
    }

    @Override
    public Interval getNextInterval(double time, Interval buffer) {
      if (time < start) {
        buffer.set(start, stop);
        return buffer;
      } else {
        throw new TimeOutOfBoundsException();
      }

    }

    @Override
    public boolean equals(Object o) {
      return Coverages.equalsImplementation(this, o);
    }

    @Override
    public int hashCode() {
      return Coverages.hashCodeImplementation(this);
    }

  }

}
