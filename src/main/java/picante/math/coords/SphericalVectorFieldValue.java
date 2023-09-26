package picante.math.coords;

/**
 * A container class for a spherical coordinate <img src="./doc-files/sphCoord.png"/> and vector
 * field value <img src="./doc-files/sphVect.png"/> at that coordinate.
 * 
 * @author G.K.Stephens
 *
 */
public final class SphericalVectorFieldValue extends AbstractVectorFieldValue<SphericalVector> {

  /**
   * @param position a spherical coordinate <img src="./doc-files/sphCoord.png"/>
   * @param value a spherical vector field value <img src="./doc-files/sphVect.png"/> at that
   *        coordinate
   */
  public SphericalVectorFieldValue(SphericalVector position, SphericalVector value) {
    super(position, value);
  }

}
