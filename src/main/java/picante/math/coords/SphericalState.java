package picante.math.coords;

public final class SphericalState extends AbstractState<SphericalVector> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final SphericalState ZERO =
      new SphericalState(SphericalVector.ZERO, SphericalVector.ZERO);

  /**
   * Creates a state vector.
   * 
   * @param position the position of one body relative to another.
   * 
   * @param velocity the velocity, the time derivative of the supplied position.
   */
  public SphericalState(SphericalVector position, SphericalVector velocity) {
    super(position, velocity);
  }

  @Override
  public String toString() {
    return "[" + super.getPosition().getRadius() + "," + super.getPosition().getColatitude() + ","
        + super.getPosition().getLongitude() + "; " + super.getVelocity().getRadius() + ","
        + super.getVelocity().getColatitude() + "," + super.getVelocity().getLongitude() + "]";
  }

}
