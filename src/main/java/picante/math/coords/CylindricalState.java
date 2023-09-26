package picante.math.coords;


public final class CylindricalState extends AbstractState<CylindricalVector> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final CylindricalState ZERO =
      new CylindricalState(CylindricalVector.ZERO, CylindricalVector.ZERO);

  /**
   * Creates a state vector.
   * 
   * @param position the position of one body relative to another.
   * 
   * @param velocity the velocity, the time derivative of the supplied position.
   */
  public CylindricalState(CylindricalVector position, CylindricalVector velocity) {
    super(position, velocity);
  }

  @Override
  public String toString() {
    return "UnwritableCylindricalState [position=" + super.getPosition() + ", velocity="
        + super.getVelocity() + "]";
  }

}
