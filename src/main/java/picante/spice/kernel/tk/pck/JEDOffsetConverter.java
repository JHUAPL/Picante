package picante.spice.kernel.tk.pck;

import static picante.units.FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000;
import static picante.units.FundamentalPhysicalConstants.SECONDS_PER_DAY;

/**
 * Julian Ephemeris Date offset implementation of the TimeConverter interface in support of text
 * based PCK frame definitions.
 */
public class JEDOffsetConverter implements TimeConverter {

  private final double tdbOffset;

  public JEDOffsetConverter(double jedEpoch) {
    tdbOffset = SECONDS_PER_DAY * (jedEpoch - JULIAN_DATE_OF_J2000);
  }

  @Override
  public double computeEvaluationTime(double et) {
    return et - tdbOffset;
  }

}
