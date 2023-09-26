package picante.spice.kernel.tk.sclk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;

public class SCLKType1RecordTest {

  private static final double TOLERANCE = 1.0E-14;

  private SCLKType1Record record;

  @Before
  public void setUp() throws Exception {
    record = new SCLKType1Record(3179992000000.0, 1.47964828550577E+8, 1.00000001497, 1000000);
  }

  @Test
  public void testSclkType1Record() {
    SCLKType1Record r = new SCLKType1Record();
    assertEquals(0, r.getEncodedSCLK(), 0.0);
    assertEquals(0, r.getParallelTime(), 0.0);
    assertEquals(0, r.getRate(), 0.0);
    assertEquals(0, r.getTicksPerMostSignificantCount(), 0.0);
  }

  @Test
  public void testSclkType1RecordDoubleDoubleDoubleDouble() {
    /*
     * Implicitly tested by exercising the get* methods.
     */
  }

  @Test
  public void testGetEncodedSCLK() {
    assertEquals(3179992000000.0, record.getEncodedSCLK(), 0.0);
  }

  @Test
  public void testGetParallelTime() {
    assertEquals(1.47964828550577E+8, record.getParallelTime(), 0.0);
  }

  @Test
  public void testGetRate() {
    assertEquals(1.00000001497, record.getRate(), 0.0);
  }

  @Test
  public void testGetTicksPerMostSignificantCount() {
    assertEquals(1000000, record.getTicksPerMostSignificantCount(), 0.0);
  }

  @Test
  public void testSetRecord() {
    SCLKType1Record result = record.setRecord(0, 1, 2, 3);
    assertSame(result, record);
    assertEquals(0, record.getEncodedSCLK(), 0.0);
    assertEquals(1, record.getParallelTime(), 0.0);
    assertEquals(2, record.getRate(), 0.0);
    assertEquals(3, record.getTicksPerMostSignificantCount(), 0.0);
  }

  @Test
  public void testExtrapolateToParallelTime() {
    assertEquals(1.4802048155141014E+8, record.extrapolateToParallelTime(3.235645E+12), TOLERANCE);
  }

  @Test
  public void testExtrapolateToEncodedSCLK() {
    /*
     * This isn't an exact inversion, likely due to round-off in the extrpolation method.
     */
    assertEquals(3.2356449999999985E+12, record.extrapolateToEncodedSCLK(1.4802048155141014E+8),
        TOLERANCE);
  }

}
