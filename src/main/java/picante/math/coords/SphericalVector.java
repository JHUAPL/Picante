package picante.math.coords;

/**
 * A class representing a vector in the spherical coordinate system.
 * 
 * @author G.K.Stephens
 * 
 */
public final class SphericalVector extends AbstractVector {

  /**
   * The ZERO vector.
   */
  public static final SphericalVector ZERO = new SphericalVector(0, 0, 0);

  public SphericalVector(double radius, double colatInRadians, double longInRadians) {
    super(radius, colatInRadians, longInRadians);
  }

  // public SphericalVector(double[] data) {
  // super(data);
  // }

  /**
   * @return the radius
   */
  public final double getRadius() {
    return super.getI();
  }

  /**
   * @return the colatitude
   */
  public final double getColatitude() {
    return super.getJ();
  }

  /**
   * @return the longitude
   */
  public final double getLongitude() {
    return super.getK();
  }

  @Override
  public String toString() {
    return "SphericalVector [radius: " + getRadius() + ", colatitude: " + getColatitude()
        + ", longitude: " + getLongitude() + "]";
  }

}
