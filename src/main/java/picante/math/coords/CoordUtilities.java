package picante.math.coords;

import static picante.math.PicanteMath.PI;

/**
 * This class is for convenience methods related to angles and coordinate converters. Its not meant
 * to grow into an alternative coordinate conversion API, so if you find that you want to add lots
 * of methods here, see first if your conversion needs are already met with other existing
 * converters.
 * 
 * @author vandejd1
 */
public class CoordUtilities {

  private CoordUtilities() {};

  /**
   * converts colatitude in radians to latitude in radians
   */
  public static double toLatitude(double colatInRadians) {
    return PI / 2.0 - colatInRadians;
  }

  /**
   * converts latitude in radians to colatitude in radians
   */
  public static double toColatitude(double latInRadians) {
    return PI / 2.0 - latInRadians;
  }

}
