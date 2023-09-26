package picante.mechanics;

import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * The unwritable parent of a simple container class that captures the two matrices of a state
 * vector coordinate transformation.
 * <p>
 * The class is simply a container for two matrices, a rotation and it's derivative. This class
 * provides functions to obtain unwritable views of the internal memory allocated by the class.
 * </p>
 * <p>
 * Note: You are obtaining an unwritable view of the actual buffers allocated in the class when you
 * call the appropriate get methods. As a direct consequence of this, if you are holding onto a
 * reference to the writable subclass the contents of the buffer could change out from underneath
 * you. If this is a concern then copy the contents of the buffer upon receiving them.
 * </p>
 */
public class UnwritableStateTransform {

  /**
   * The field containing the buffer that holds the rotation matrix.
   */
  protected final RotationMatrixIJK rotation = new RotationMatrixIJK(RotationMatrixIJK.IDENTITY);

  /**
   * The field containing the buffer that holds the derivative of the rotation matrix.
   */
  protected final MatrixIJK dRotation = new MatrixIJK(MatrixIJK.ZEROS);

  /**
   * Creates an unwritable state transformation.
   * 
   * @param r the rotation matrix. Contents are copied into memory local to the newly created
   *        instance.
   * @param dr the derivative of the rotation matrix. Contents are copied into memory local to the
   *        newly created instance.
   */
  public UnwritableStateTransform(UnwritableRotationMatrixIJK r, UnwritableMatrixIJK dr) {
    this.rotation.setTo(r);
    this.dRotation.setTo(dr);
  }

  /**
   * Copy constructor.
   * 
   * @param transform the transform whose contents are to be copied.
   */
  public UnwritableStateTransform(UnwritableStateTransform transform) {
    this.rotation.setTo(transform.rotation);
    this.dRotation.setTo(transform.dRotation);
  }

  /**
   * Creates an new transform that is the inverse of this transform.
   * 
   * @return a newly created transform that is the inverse of this
   */
  public UnwritableStateTransform createInverse() {
    UnwritableStateTransform result = new UnwritableStateTransform(this);
    result.rotation.transpose();
    result.dRotation.transpose();
    return result;
  }

  /**
   * Get the rotation matrix component.
   * 
   * @return an unwritable view of the rotation matrix component
   */
  public UnwritableRotationMatrixIJK getRotation() {
    return this.rotation;
  }

  /**
   * Get the derivative of the rotation matrix component.
   * 
   * @return an unwritable view of the derivative of the rotation matrix component.
   */
  public UnwritableMatrixIJK getRotationDerivative() {
    return this.dRotation;
  }

  /**
   * Compute the product of the inverse of a this state transformation with a body state.
   * 
   * @param v the state vector
   * @param buffer the buffer to receive the product, inverse(m)*v
   * 
   * @return a reference to buffer for convenience
   */
  public StateVector mixv(UnwritableStateVector v, StateVector buffer) {

    double i = v.position.getI();
    double j = v.position.getJ();
    double k = v.position.getK();

    /*
     * Compute the velocity term first, using the position component as a buffer to receive the
     * first part of the computation.
     */
    dRotation.mtxv(v.position, buffer.position);
    rotation.mtxv(v.velocity, buffer.velocity);
    VectorIJK.add(buffer.position, buffer.velocity, buffer.velocity);

    v.position.setI(i);
    v.position.setJ(j);
    v.position.setK(k);

    rotation.mtxv(v.position, buffer.position);

    return buffer;
  }

  /**
   * Computes the product of the inverse of this state transform with a body state.
   * 
   * @param v the state vector
   * 
   * @return a newly created StateVector containing the product inverse(m)*v
   */
  public StateVector mixv(UnwritableStateVector v) {
    return mixv(v, new StateVector());
  }

  /**
   * Compute the product of this state transformation with a state vector.
   * 
   * @param v the state vector
   * @param buffer the buffer to receive the product m*v
   * 
   * @return a reference to buffer for convenience
   */
  public StateVector mxv(UnwritableStateVector v, StateVector buffer) {

    double i = v.position.getI();
    double j = v.position.getJ();
    double k = v.position.getK();

    /*
     * Compute the velocity term first, using the position component as a buffer to receive the
     * first part of the computation.
     */
    dRotation.mxv(v.position, buffer.position);
    rotation.mxv(v.velocity, buffer.velocity);
    VectorIJK.add(buffer.position, buffer.velocity, buffer.velocity);

    v.position.setI(i);
    v.position.setJ(j);
    v.position.setK(k);

    rotation.mxv(v.position, buffer.position);

    return buffer;
  }

  /**
   * Compute the product of this state transformation with a state vector.
   * 
   * @param v the state vector
   * 
   * @return a newly created StateVector containing the product m*v
   */
  public StateVector mxv(UnwritableStateVector v) {
    return mxv(v, new StateVector());
  }

  /**
   * Makes an unwritable copy of the supplied transform.
   * <p>
   * This method makes an unwritable copy only if necessary. It tries to avoid making a copy
   * wherever possible.
   * </p>
   * 
   * @param transform a transform to copy.
   * 
   * @return either a reference to transform (if transform is already only an instance of
   *         {@link UnwritableStateTransform}, otherwise an unwritable copy of transform's contents
   */
  public static UnwritableStateTransform copyOf(UnwritableStateTransform transform) {
    if (transform.getClass().equals(UnwritableStateTransform.class)) {
      return transform;
    }
    return new UnwritableStateTransform(transform);
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((dRotation == null) ? 0 : dRotation.hashCode());
    result = prime * result + ((rotation == null) ? 0 : rotation.hashCode());
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof UnwritableStateTransform)) {
      return false;
    }
    UnwritableStateTransform other = (UnwritableStateTransform) obj;
    if (dRotation == null) {
      if (other.dRotation != null) {
        return false;
      }
    } else if (!dRotation.equals(other.dRotation)) {
      return false;
    }
    if (rotation == null) {
      if (other.rotation != null) {
        return false;
      }
    } else if (!rotation.equals(other.rotation)) {
      return false;
    }
    return true;
  }

}
