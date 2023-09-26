package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.UnwritableStateVector;

/**
 * A State containing two {@link VectorIJK}, one for the Cartesian position and one for the
 * Cartesian Velocity. Similar to the {@link UnwritableStateVector} classes.
 * 
 * @author G.K.Stephens
 * 
 */
public final class CartesianState extends AbstractState<UnwritableVectorIJK> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final CartesianState ZERO = new CartesianState(VectorIJK.ZERO, VectorIJK.ZERO);

  /**
   * Creates a state vector. Defensive copies are made of the input vectors.
   * 
   * @param position the position of one object relative to another.
   * 
   * @param velocity the time derivative of the supplied position.
   */
  public CartesianState(UnwritableVectorIJK position, UnwritableVectorIJK velocity) {
    super(UnwritableVectorIJK.copyOf(position), UnwritableVectorIJK.copyOf(velocity));
  }

  @Override
  public final String toString() {
    return "[" + super.getPosition().getI() + "," + super.getPosition().getJ() + ","
        + super.getPosition().getK() + "; " + super.getVelocity().getI() + ","
        + super.getVelocity().getJ() + "," + super.getVelocity().getK() + "]";
  }

}
