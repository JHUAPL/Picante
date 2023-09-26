package picante.math.coords;

public final class PolarState extends AbstractState<PolarVector> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final PolarState ZERO = new PolarState(PolarVector.ZERO, PolarVector.ZERO);

  public PolarState(PolarVector position, PolarVector velocity) {
    super(position, velocity);
  }

  @Override
  public String toString() {
    return "[" + super.getPosition().getRadius() + "," + super.getPosition().getAngle() + "; "
        + super.getVelocity().getRadius() + "," + super.getVelocity().getAngle() + "]";
  }
}
