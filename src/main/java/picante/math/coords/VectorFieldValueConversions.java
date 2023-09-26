package picante.math.coords;

import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * This class provides conversions from Cartesian coordinates <img src="./doc-files/cartCoord.png"/>
 * and vector field values <img src="./doc-files/cartVect.png"/> to cylindrical
 * <img src="./doc-files/cylCoord.png"/> and spherical <img src="./doc-files/sphCoord.png"/>
 * coordinates and vector field values <img src="./doc-files/cylVect.png"/>,
 * <img src="./doc-files/sphVect.png"/> and vice versa.
 * 
 * @author G.K.Stephens
 *
 */
public class VectorFieldValueConversions {

  private final static CylindricalToCartesianBasisTransformation CYLINDRICAL =
      new CylindricalToCartesianBasisTransformation();
  private final static SphericalToCartesianBasisTransformation SPHERICAL =
      new SphericalToCartesianBasisTransformation();

  /**
   * Private
   */
  private VectorFieldValueConversions() {}

  /**
   * Converts a cylindrical coordinate <img src="./doc-files/cylCoord.png"/> and vector field value
   * <img src="./doc-files/cylVect.png"/> at that coordinate to a Cartesian coordinate
   * <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/>.
   * 
   * @param cylindrical a cylindrical coordinate <img src="./doc-files/cylCoord.png"/> and vector
   *        field value <img src="./doc-files/cylVect.png"/> at that coordinate
   * @return a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   *         <img src="./doc-files/cartVect.png"/> at that coordinate
   */
  public static CartesianVectorFieldValue convert(CylindricalVectorFieldValue cylindrical) {

    MatrixIJK matrix = new MatrixIJK();

    UnwritableVectorIJK position = CoordConverters.convert(cylindrical.getPosition());

    CYLINDRICAL.getTransformation(cylindrical.getPosition(), matrix);

    UnwritableVectorIJK value = CYLINDRICAL.mxv(matrix, cylindrical.getValue());

    return new CartesianVectorFieldValue(position, value);
  }

  /**
   * Converts a cylindrical coordinate <img src="./doc-files/cylCoord.png"/> and vector field value
   * <img src="./doc-files/cylVect.png"/> at that coordinate to a Cartesian coordinate
   * <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/>.
   * 
   * @param cylPos a cylindrical coordinate <img src="./doc-files/cylCoord.png"/>
   * @param cylValue a cylindrical vector field value <img src="./doc-files/cylVect.png"/> at that
   *        coordinate
   * @return a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   *         <img src="./doc-files/cartVect.png"/> at that coordinate
   */
  public static CartesianVectorFieldValue convert(CylindricalVector cylPos,
      CylindricalVector cylValue) {

    MatrixIJK matrix = new MatrixIJK();

    UnwritableVectorIJK position = CoordConverters.convert(cylPos);

    CYLINDRICAL.getTransformation(cylPos, matrix);

    UnwritableVectorIJK value = CYLINDRICAL.mxv(matrix, cylValue);

    return new CartesianVectorFieldValue(position, value);
  }

  /**
   * Converts a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/> at that coordinate to a cylindrical coordinate
   * <img src="./doc-files/cylCoord.png"/> and vector field value
   * <img src="./doc-files/cylVect.png"/>.
   * 
   * @param Cartesian a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field
   *        value <img src="./doc-files/cartVect.png"/> at that coordinate
   * @return a cylindrical coordinate <img src="./doc-files/cylCoord.png"/> and vector field value
   *         <img src="./doc-files/cylVect.png"/> at that coordinate
   */
  public static CylindricalVectorFieldValue convertToCylindrical(
      CartesianVectorFieldValue cartesian) {

    MatrixIJK matrix = new MatrixIJK();

    CylindricalVector position = CoordConverters.convertToCylindrical(cartesian.getPosition());

    CYLINDRICAL.getInverseTransformation(position, matrix);

    CylindricalVector value = CYLINDRICAL.mxv(matrix, cartesian.getValue());

    return new CylindricalVectorFieldValue(position, value);
  }

  /**
   * Converts a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/> at that coordinate to a cylindrical coordinate
   * <img src="./doc-files/cylCoord.png"/> and vector field value
   * <img src="./doc-files/cylVect.png"/>.
   * 
   * @param cartPos a Cartesian coordinate <img src="./doc-files/cartCoord.png"/>
   * @param cartValue a vector field value <img src="./doc-files/cartVect.png"/> at that coordinate
   * @return a cylindrical coordinate <img src="./doc-files/cylCoord.png"/> and vector field value
   *         <img src="./doc-files/cylVect.png"/> at that coordinate
   */
  public static CylindricalVectorFieldValue convertToCylindrical(UnwritableVectorIJK cartPos,
      UnwritableVectorIJK cartValue) {

    MatrixIJK matrix = new MatrixIJK();

    CylindricalVector position = CoordConverters.convertToCylindrical(cartPos);

    CYLINDRICAL.getInverseTransformation(position, matrix);

    CylindricalVector value = CYLINDRICAL.mxv(matrix, cartValue);

    return new CylindricalVectorFieldValue(position, value);
  }

  /**
   * Converts a spherical coordinate <img src="./doc-files/sphCoord.png"/> and vector field value
   * <img src="./doc-files/sphVect.png"/> at that coordinate to a Cartesian coordinate
   * <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/>.
   * 
   * @param spherical a spherical coordinate <img src="./doc-files/sphCoord.png"/> and vector field
   *        value <img src="./doc-files/sphVect.png"/> at that coordinate
   * @return a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   *         <img src="./doc-files/cartVect.png"/> at that coordinate
   */
  public static CartesianVectorFieldValue convert(SphericalVectorFieldValue spherical) {

    MatrixIJK matrix = new MatrixIJK();

    UnwritableVectorIJK position = CoordConverters.convert(spherical.getPosition());

    SPHERICAL.getTransformation(spherical.getPosition(), matrix);

    UnwritableVectorIJK value = SPHERICAL.mxv(matrix, spherical.getValue());

    return new CartesianVectorFieldValue(position, value);
  }

  /**
   * Converts a spherical coordinate <img src="./doc-files/sphCoord.png"/> and vector field value
   * <img src="./doc-files/sphVect.png"/> at that coordinate to a Cartesian coordinate
   * <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/>.
   * 
   * @param sphPos a spherical coordinate <img src="./doc-files/sphCoord.png"/>
   * @param sphValue a vector field value <img src="./doc-files/sphVect.png"/> at that coordinate
   * @return a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   *         <img src="./doc-files/cartVect.png"/> at that coordinate
   */
  public static CartesianVectorFieldValue convert(SphericalVector sphPos,
      SphericalVector sphValue) {

    MatrixIJK matrix = new MatrixIJK();

    UnwritableVectorIJK position = CoordConverters.convert(sphPos);

    SPHERICAL.getTransformation(sphPos, matrix);

    UnwritableVectorIJK value = SPHERICAL.mxv(matrix, sphValue);

    return new CartesianVectorFieldValue(position, value);
  }

  /**
   * Converts a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/> at that coordinate to a spherical coordinate
   * <img src="./doc-files/sphCoord.png"/> and vector field value
   * <img src="./doc-files/sphVect.png"/>.
   * 
   * @param cartesian a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field
   *        value <img src="./doc-files/cartVect.png"/> at that coordinate
   * @return a spherical coordinate <img src="./doc-files/sphCoord.png"/> and vector field value
   *         <img src="./doc-files/sphVect.png"/> at that coordinate
   */
  public static SphericalVectorFieldValue convertToSpherical(CartesianVectorFieldValue cartesian) {

    MatrixIJK matrix = new MatrixIJK();

    SphericalVector position = CoordConverters.convertToSpherical(cartesian.getPosition());

    SPHERICAL.getInverseTransformation(position, matrix);

    SphericalVector value = SPHERICAL.mxv(matrix, cartesian.getValue());

    return new SphericalVectorFieldValue(position, value);
  }

  /**
   * Converts a Cartesian coordinate <img src="./doc-files/cartCoord.png"/> and vector field value
   * <img src="./doc-files/cartVect.png"/> at that coordinate to a spherical coordinate
   * <img src="./doc-files/sphCoord.png"/> and vector field value
   * <img src="./doc-files/sphVect.png"/>.
   * 
   * @param cartPos a Cartesian coordinate <img src="./doc-files/cartCoord.png"/>
   * @param cartValue a vector field value <img src="./doc-files/cartVect.png"/> at that coordinate
   * @return a spherical coordinate <img src="./doc-files/sphCoord.png"/> and vector field value
   *         <img src="./doc-files/sphVect.png"/> at that coordinate
   */
  public static SphericalVectorFieldValue convertToSpherical(UnwritableVectorIJK cartPos,
      UnwritableVectorIJK cartValue) {

    MatrixIJK matrix = new MatrixIJK();

    SphericalVector position = CoordConverters.convertToSpherical(cartPos);

    SPHERICAL.getInverseTransformation(position, matrix);

    SphericalVector value = SPHERICAL.mxv(matrix, cartValue);

    return new SphericalVectorFieldValue(position, value);
  }

}
