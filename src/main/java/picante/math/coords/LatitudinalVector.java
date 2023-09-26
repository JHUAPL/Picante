package picante.math.coords;

/**
 * A class representing a vector in the latitudinal coordinate system.
 * 
 * @author G.K.Stephens
 * 
 */
public final class LatitudinalVector extends AbstractVector {

  /**
   * The ZERO vector.
   */
  public static final LatitudinalVector ZERO = new LatitudinalVector(0, 0, 0);

  public LatitudinalVector(double radius, double latInRadians, double longInRadians) {
    super(radius, latInRadians, longInRadians);
  }

  // public LatitudinalVector(double[] data) {
  // super(data);
  // }

  /**
   * @return the radius
   */
  public double getRadius() {
    return super.getI();
  }

  /**
   * @return the latitude
   */
  public double getLatitude() {
    return super.getJ();
  }

  /**
   * @return the longitude
   */
  public double getLongitude() {
    return super.getK();
  }

  @Override
  public String toString() {
    return "LatitudinalVector [radius: " + getRadius() + ", latitude: " + getLatitude()
        + ", longitude: " + getLongitude() + "]";
  }

}
