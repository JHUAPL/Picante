package picante.math.coords;

/**
 * A class representing a vector in the cylindrical coordinate system.
 * 
 * @author G.K.Stephens
 * 
 */
public final class CylindricalVector extends AbstractVector {

  /**
   * The ZERO vector.
   */
  public static final CylindricalVector ZERO = new CylindricalVector(0, 0, 0);

  public CylindricalVector(double cylindricalRadius, double longInRadians, double height) {
    super(cylindricalRadius, longInRadians, height);
  }

  // public CylindricalVector(double[] data) {
  // super(data);
  // }

  /**
   * @return the cylindrical radius (often denoted as r or &#961;)
   */
  public final double getCylindricalRadius() {
    return super.getI();
  }

  /**
   * @return the longitude
   */
  public final double getLongitude() {
    return super.getJ();
  }

  /**
   * @return the height (often denoted as z)
   */
  public final double getHeight() {
    return super.getK();
  }

  @Override
  public String toString() {
    return "CylindricalVector [cylindricalRadius: " + getCylindricalRadius() + ", longitude: "
        + getLongitude() + ", height: " + getHeight() + "]";
  }

}
