package picante.math.coords;

/**
 * A class representing a vector in the Celestial Coordinate system.
 * 
 * @author G.K.Stephens
 * 
 */
public final class RaDecVector extends AbstractVector {

  /**
   * The ZERO vector.
   */
  public static final RaDecVector ZERO = new RaDecVector(0, 0, 0);

  public RaDecVector(double radius, double raRadians, double decRadians) {
    super(radius, raRadians, decRadians);
  }

  // public RaDecVector(double[] data) {
  // super(data);
  // }

  /**
   * @return the radius
   */
  public double getRadius() {
    return super.getI();
  }

  /**
   * @return the right ascension
   */
  public double getRightAscension() {
    return super.getJ();
  }

  /**
   * @return the declination
   */
  public double getDeclination() {
    return super.getK();
  }

  @Override
  public String toString() {
    return "RaDecVector [radius: " + getRadius() + ", rightAscension: " + getRightAscension()
        + ", declination: " + getDeclination() + "]";
  }

}
