package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * A container class for a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector
 * field value <img src="./doc-files/cartVect.png"/> at that coordinate.
 * 
 * @author G.K.Stephens
 *
 */
public final class CartesianVectorFieldValue extends AbstractVectorFieldValue<UnwritableVectorIJK> {

  /**
   * @param position a Cartesian coordinate <img src="./doc-files/cartCoord.png"/>
   * @param value a Cartesian vector field value <img src="./doc-files/cartVect.png"/> at that
   *        coordinate
   */
  public CartesianVectorFieldValue(UnwritableVectorIJK position, UnwritableVectorIJK value) {
    super(UnwritableVectorIJK.copyOf(position), UnwritableVectorIJK.copyOf(value));
  }

}
