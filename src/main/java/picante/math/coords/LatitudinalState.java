package picante.math.coords;

public final class LatitudinalState extends AbstractState<LatitudinalVector> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final LatitudinalState ZERO =
      new LatitudinalState(LatitudinalVector.ZERO, LatitudinalVector.ZERO);

  public LatitudinalState(LatitudinalVector position, LatitudinalVector velocity) {
    super(position, velocity);
  }

  @Override
  public String toString() {
    return "[" + super.getPosition().getRadius() + "," + super.getPosition().getLatitude() + ","
        + super.getPosition().getLongitude() + "; " + super.getVelocity().getRadius() + ","
        + super.getVelocity().getLatitude() + "," + super.getVelocity().getLongitude() + "]";
  }

}
