package picante.spice.kernel.tk.pck;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import picante.units.FundamentalPhysicalConstants;

public class JEDOffsetConverterTest {

  private static final double TOLERANCE = 1e-10;

  @Test
  public void testComputeEvaluationTimeZeroOffset() {

    JEDOffsetConverter converter =
        new JEDOffsetConverter(FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000);

    assertEquals(1e6, converter.computeEvaluationTime(1e6), 0.0);

  }

  @Test
  public void testComputeEvaluationTimeHundredYearOffset() {

    JEDOffsetConverter converter =
        new JEDOffsetConverter(FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000 + 36525.0);

    assertEquals(-36525.0 * FundamentalPhysicalConstants.SECONDS_PER_DAY,
        converter.computeEvaluationTime(0), TOLERANCE);

  }

}
