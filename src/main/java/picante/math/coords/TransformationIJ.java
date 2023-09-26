package picante.math.coords;

import picante.math.vectorspace.MatrixIJ;
import picante.math.vectorspace.UnwritableMatrixIJ;
import picante.math.vectorspace.UnwritableVectorIJ;

/**
 * This interface assists with the manipulation of Jacobians. A Jacobian is the matrix of all first
 * order partial derivatives of a vector with respect to another vector. In this package, it is
 * meant to represent the first order partial derivatives of some coordinate system with respect to
 * Cartesian. A Jacobian can obviously be identified with simply a {@link MatrixIJ}, however, this
 * eliminates all context.
 * 
 * Note: for a completely symmetric interface, getInverseJacobian would take a VectorIJ, however,
 * this is not done, because it allows the implementor to use the inversion of the getJacobian
 * method. This is not the most generic way, but it allows for only a single definition of the
 * Jacobian and not two. Spice also follows this convention.
 * 
 * TODO Within this package, implementations of this interface are assumed to be thread safe, and in
 * practice are so because they are stateless. Perhaps this restriction should be lifted, and the
 * threading issues present in this package be solved in a more robust way.
 * 
 * @author G.K.Stephens
 * 
 * @param <C> an {@link AbstractVectorIJ} type
 */
interface TransformationIJ<C extends AbstractVectorIJ> {

  /**
   * Gets the Jacobian from the Coordinate system to Cartesian
   * 
   * @param coordPosition The coordinate position in which the Jacobian will be calculated.
   * @param buffer A {@link MatrixIJ} containing the Jacobian from the specified Coordinate system
   *        to Cartesian.
   * @return A {@link MatrixIJ} containing the Jacobian from the specified Coordinate system to
   *         Cartesian.
   */
  public MatrixIJ getTransformation(C coordPosition, MatrixIJ buffer);

  /**
   * Returns the Jacobian from Cartesian to the specified Coordinate system. Note, that this takes a
   * coordinate position and not the Cartesian position, allowing the implementor to leverage the
   * other method.
   * 
   * @param coordPosition The coordinate position in which the inverse Jacobian will be calculated.
   * @param buffer A {@link MatrixIJ} containing the Jacobian from Cartesian to the specified
   *        Coordinate system.
   * @return A {@link MatrixIJ} containing the Jacobian from Cartesian to the specified Coordinate
   *         system.
   */
  public MatrixIJ getInverseTransformation(C coordPosition, MatrixIJ buffer);

  public UnwritableVectorIJ mxv(UnwritableMatrixIJ jacobian, C coordVelocity);

  public C mxv(UnwritableMatrixIJ inverseJacobian, UnwritableVectorIJ cartVelocity);

}
