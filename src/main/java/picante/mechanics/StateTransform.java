package picante.mechanics;

import picante.designpatterns.Writable;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

// TODO: Consider adding create*() style methods as useful.

/**
 * The writable child of a container class capturing the matrix components of a state vector
 * coordinate transformation.
 * <p>
 * The fields containing the actual data are defined in the parent class, per the usual weak
 * immutability pattern.
 * </p>
 */
public class StateTransform extends UnwritableStateTransform
    implements Writable.ImplementationInterface<UnwritableStateTransform, StateTransform> {

  /**
   * The identity state transformation: zero derivative matrix and an identity rotation.
   */
  public static final UnwritableStateTransform IDENTITY =
      new UnwritableStateTransform(RotationMatrixIJK.IDENTITY, MatrixIJK.ZEROS);

  /**
   * Default constructor, creates an identity state transform.
   */
  public StateTransform() {
    super(IDENTITY);
  }

  /**
   * Creates a state transformation.
   * 
   * @param r the rotation matrix. Contents are copied into the local memory allocated by the
   *        instance.
   * @param dr the derivative of the rotation matrix. Contents are copied into the local memory
   *        allocated by the instance.
   */
  public StateTransform(UnwritableRotationMatrixIJK r, UnwritableMatrixIJK dr) {
    super(r, dr);
  }

  /**
   * Copy constructor.
   * 
   * @param transform the transform whose contents are to be copied.
   */
  public StateTransform(UnwritableStateTransform transform) {
    super(transform);
  }

  @Override
  public StateTransform createInverse() {
    return new StateTransform(this).invert();
  }

  /**
   * Get the rotation matrix component.
   * 
   * @return a writable view of the rotation matrix component of the state transform
   */
  @Override
  public RotationMatrixIJK getRotation() {
    return this.rotation;
  }

  /**
   * Get the derivative of the rotation matrix component.
   * 
   * @return a writable view of the rotation derivative component.
   */
  @Override
  public MatrixIJK getRotationDerivative() {
    return this.dRotation;
  }

  /**
   * Set the rotation component of the state transformation.
   * 
   * @param rotation the rotation matrix. Contents are copied into memory local to the instance
   */
  public final void setRotation(UnwritableRotationMatrixIJK rotation) {
    this.rotation.setTo(rotation);
  }

  /**
   * Set the derivative component of the state transformation.
   * 
   * @param rotationDerivative the derivative matrix. Contents are copied into memory local to the
   *        instance.
   */
  public final void setRotationDerivative(UnwritableMatrixIJK rotationDerivative) {
    this.dRotation.setTo(rotationDerivative);
  }

  /**
   * Set the contents of the state transform to the supplied transform.
   * 
   * @param transform a state transform which is to have its contents copied to the memory local to
   *        the instance.
   * 
   * @return a reference to the instance for convenience
   */
  @Override
  public final StateTransform setTo(UnwritableStateTransform transform) {
    this.rotation.setTo(transform.rotation);
    this.dRotation.setTo(transform.dRotation);
    return this;
  }

  /**
   * Compute the inverse of the state transform.
   * <p>
   * Due to the block nature of a state transform, and the individual constraints on the blocks, one
   * can show that the following is true:
   * 
   * <pre>
   *    -            -
   *   |       :      |
   *   |   R   :   0  |
   *   |.......:......|
   *   |       :      |
   *   |  W*R  :   R  |
   *   |       :      |
   *    -            -
   * </pre>
   * 
   * has the inverse:
   * 
   * <pre>
   *    -            -
   *   |    t  :      |
   *   |   R   :   0  |
   *   |.......:......|
   *   |      t:    t |
   *   | (W*R) :   R  |
   *   |       :      |
   *    -            -
   * </pre>
   * 
   * </p>
   * <p>
   * Diagrams borrowed directly from NAIF's INVSTM routine in SPICELIB.
   * </p>
   * 
   * @return a reference to the instance for convenience.
   */
  public StateTransform invert() {
    this.rotation.transpose();
    this.dRotation.transpose();
    return this;
  }

  /**
   * Compute the product of a state transform with the inverse of another state transform.
   * 
   * @param a the left hand transform
   * @param b the right hand transform to invert, then multiply. Note: the contents of this state
   *        transform are unmodified by this routine, unless it is also supplied as the buffer
   *        argument.
   * @param buffer the buffer to receive the product, a*inverse(b)
   * 
   * @return a reference to buffer for convenience
   */
  public static StateTransform mxmi(UnwritableStateTransform a, UnwritableStateTransform b,
      StateTransform buffer) {

    MatrixIJK.mxmtadd(a.dRotation, b.rotation, a.rotation, b.dRotation, buffer.dRotation);
    RotationMatrixIJK.mxmt(a.rotation, b.rotation, buffer.rotation);

    return buffer;
  }

  /**
   * Compute the product of a state transform with the inverse of another state transform.
   * 
   * @param a the left hand transform
   * @param b the right hand transform to invert, then multiply. Note: the contents of this state
   *        transform are unmodified by this routine, unless it is also supplied as the buffer
   *        argument.
   * 
   * @return the product, a*inverse(b)
   */
  public static StateTransform mxmi(UnwritableStateTransform a, UnwritableStateTransform b) {
    return mxmi(a, b, new StateTransform());
  }

  /**
   * Compute the product of the inverse of a state transform with another state transform.
   * 
   * @param a the left hand transform to invert, then multiply. It is left unmodified by the call to
   *        this method, unless supplied as the buffer argument.
   * @param b the right hand transform.
   * @param buffer the buffer to receive the product, inverse(a)*b
   * 
   * @return a reference to buffer for convenience
   */
  public static StateTransform mixm(UnwritableStateTransform a, UnwritableStateTransform b,
      StateTransform buffer) {

    MatrixIJK.mtxmadd(a.dRotation, b.rotation, a.rotation, b.dRotation, buffer.dRotation);
    RotationMatrixIJK.mtxm(a.rotation, b.rotation, buffer.rotation);

    return buffer;
  }

  /**
   * Compute the product of the inverse of a state transform with another state transform.
   * 
   * @param a the left hand transform to invert, then multiply. It is left unmodified by the call to
   *        this method, unless supplied as the buffer argument.
   * @param b the right hand transform.
   * 
   * @return the product, inverse(a)*b
   */
  public static StateTransform mixm(UnwritableStateTransform a, UnwritableStateTransform b) {
    return mixm(a, b, new StateTransform());
  }

  /**
   * Compute the product of two state transformations.
   * 
   * @param a the left hand state transform
   * @param b the right hand state transform
   * @param buffer the buffer to receive the product, a*b
   * 
   * @return a referenece to buffer for convenience
   */
  public static StateTransform mxm(UnwritableStateTransform a, UnwritableStateTransform b,
      StateTransform buffer) {

    MatrixIJK.mxmadd(a.dRotation, b.rotation, a.rotation, b.dRotation, buffer.dRotation);
    RotationMatrixIJK.mxm(a.rotation, b.rotation, buffer.rotation);

    return buffer;
  }

  /**
   * Compute the product of two state transformations.
   * 
   * @param a the left hand state transform
   * @param b the right hand state transform
   * 
   * @return the product, a*b
   */
  public static StateTransform mxm(UnwritableStateTransform a, UnwritableStateTransform b) {
    return mxm(a, b, new StateTransform());
  }

  /**
   * Compute the product of the inverse of a state transformation with a body state.
   * 
   * @param m the state transform
   * @param v the state vector
   * @param buffer the buffer to receive the product, inverse(m)*v
   * 
   * @return a reference to buffer for convenience
   */
  @Deprecated
  public static StateVector mixv(UnwritableStateTransform m, UnwritableStateVector v,
      StateVector buffer) {

    double i = v.position.getI();
    double j = v.position.getJ();
    double k = v.position.getK();

    /*
     * Compute the velocity term first, using the position component as a buffer to receive the
     * first part of the computation.
     */
    m.dRotation.mtxv(v.position, buffer.position);
    m.rotation.mtxv(v.velocity, buffer.velocity);
    VectorIJK.add(buffer.position, buffer.velocity, buffer.velocity);

    v.position.setI(i);
    v.position.setJ(j);
    v.position.setK(k);

    m.rotation.mtxv(v.position, buffer.position);

    return buffer;
  }

  /**
   * Compute the product of a state transformation with a state vector.
   * 
   * @param m the state transformation
   * @param v the state vector
   * @param buffer the buffer to receive the product m*v
   * 
   * @return a reference to buffer for convenience
   */
  @Deprecated
  public static StateVector mxv(UnwritableStateTransform m, UnwritableStateVector v,
      StateVector buffer) {

    double i = v.position.getI();
    double j = v.position.getJ();
    double k = v.position.getK();

    /*
     * Compute the velocity term first, using the position component as a buffer to receive the
     * first part of the computation.
     */
    m.dRotation.mxv(v.position, buffer.position);
    m.rotation.mxv(v.velocity, buffer.velocity);
    VectorIJK.add(buffer.position, buffer.velocity, buffer.velocity);

    v.position.setI(i);
    v.position.setJ(j);
    v.position.setK(k);

    m.rotation.mxv(v.position, buffer.position);

    return buffer;
  }
}
