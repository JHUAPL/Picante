package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.VectorIJ;
import picante.mechanics.UnwritableStateVector;

/**
 * A State containing two {@link VectorIJ}, one for the Cartesian position and one for the Cartesian
 * Velocity. Similar to the {@link UnwritableStateVector} classes.
 * 
 * @author G.K.Stephens
 * 
 */
public final class CartesianStateIJ extends AbstractState<UnwritableVectorIJ> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final CartesianStateIJ ZERO = new CartesianStateIJ(VectorIJ.ZERO, VectorIJ.ZERO);

  /**
   * Creates a state vector. Defensive copies are made of the input vectors.
   * 
   * @param position the position of one object relative to another.
   * 
   * @param velocity the time derivative of the supplied position.
   */
  public CartesianStateIJ(UnwritableVectorIJ position, UnwritableVectorIJ velocity) {
    super(UnwritableVectorIJ.copyOf(position), UnwritableVectorIJ.copyOf(velocity));
  }

  @Override
  public final String toString() {
    return "[" + super.getPosition().getI() + "," + super.getPosition().getJ() + "; "
        + super.getVelocity().getI() + "," + super.getVelocity().getJ() + "]";
  }

}
