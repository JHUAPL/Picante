package picante.math.coords;

/**
 * A container class for a cylindrical coordinate <img src="./doc-files/cylCoord.png"/> and vector
 * field value <img src="./doc-files/cylVect.png"/> at that coordinate.
 * 
 * @author G.K.Stephens
 *
 */
public final class CylindricalVectorFieldValue extends AbstractVectorFieldValue<CylindricalVector> {

  /**
   * @param position a cylindrical coordinate <img src="./doc-files/cylCoord.png"/>
   * @param value a cylindrical vector field value <img src="./doc-files/cylVect.png"/> at that
   *        coordinate
   */
  public CylindricalVectorFieldValue(CylindricalVector position, CylindricalVector value) {
    super(position, value);
  }

}
