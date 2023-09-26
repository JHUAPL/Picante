package picante.math.coords;

/**
 * A class representing a vector in the polar coordinate system.
 * 
 * @author G.K.Stephens
 * 
 */
public final class PolarVector extends AbstractVectorIJ {

  /**
   * The ZERO vector.
   */
  public static final PolarVector ZERO = new PolarVector(0, 0);

  public PolarVector(double radius, double angle) {
    super(radius, angle);
  }

  // public PolarVector(double[] data) {
  // super(data);
  // }

  /**
   * @return the radius
   */
  public final double getRadius() {
    return super.getI();
  }

  /**
   * @return the angle
   */
  public final double getAngle() {
    return super.getJ();
  }

  @Override
  public String toString() {
    return "PolarVector [radius: " + getRadius() + ", angle: " + getAngle() + "]";
  }
}
