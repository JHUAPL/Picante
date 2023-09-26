package picante.math;

/**
 *
 * Created : Jul 31, 2008 1:50:58 PM
 * 
 * @author vandejd1
 */
public class CoordUtilities {

  public static double convertLongitudeInRadiansToLocaltimeInFractionalHours(double longInRads) {
    // I want to know how far around past the -X axis this vector is, and
    // the longitude tells me how far around from +X it is, so I first
    // subtract off 180 degrees.

    longInRads -= Math.PI;

    // make sure the longitude is from 0 to 2*Pi:
    double TWOPI = Math.PI * 2.0;
    while (longInRads < 0) {
      longInRads += TWOPI;
    }
    while (longInRads > TWOPI) {
      longInRads -= TWOPI;
    }

    // now convert to hours:
    return longInRads / TWOPI * 24.0;
  }

}
