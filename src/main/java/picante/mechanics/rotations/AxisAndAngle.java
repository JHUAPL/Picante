package picante.mechanics.rotations;

import static picante.math.PicanteMath.PI;
import static picante.math.PicanteMath.atan2;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Implementation of a rotation axis and angle and the corresponding arithmetic algorithms.
 * <p>
 * This class can be thought of as capturing the NAIF routines RAXISA and AXISAR, with a few small
 * differences.
 * </p>
 * <p>
 * TODO: Add a description of the mathematical action of this class, and the non-symmetric nature of
 * the inverse methods.
 * </p>
 */
public class AxisAndAngle implements Rotation {

  /**
   * Axis of rotation, should always be a unit length vector.
   */
  private final VectorIJK axis = new VectorIJK();

  /**
   * Angle of rotation, specified in radians.
   */
  private double angle;

  /**
   * Create the default axis and angle rotation representation, a rotation of zero radians about K.
   */
  public AxisAndAngle() {
    axis.setTo(VectorIJK.K);
    angle = 0.0;
  }

  /**
   * Create an axis and angle rotation from the supplied vector components and angle.
   * 
   * @param axisi the ith component of the potentially non-unit rotation axis
   * @param axisj the jth component of the potentially non-unit rotation axis
   * @param axisk the kth component of the potentially non-unit rotation axis
   * @param angle the rotation angle in radians
   * 
   * @throws UnsupportedOperationException if the axis components are all identically zero.
   */
  public AxisAndAngle(double axisi, double axisj, double axisk, double angle) {
    this.setTo(axisi, axisj, axisk, angle);
  }

  /**
   * Create an axis and angle rotation from the specified rotation axis and angle.
   * 
   * @param axis any vector whose length is strictly greater than zero.
   * @param angle the rotation angle specified in radians.
   * 
   * @throws UnsupportedOperationException if the length of axis is zero.
   */
  public AxisAndAngle(UnwritableVectorIJK axis, double angle) {
    this.setTo(axis, angle);
  }

  /**
   * Create an axis and angle rotation from the specified rotation matrix.
   * <p>
   * In the event the identity matrix is supplied, this method will set the axis to K and the angle
   * to zero.
   * </p>
   * 
   * @param matrix a rotation matrix to convert to axis and angle representation.
   */
  public AxisAndAngle(UnwritableRotationMatrixIJK matrix) {
    this.setTo(matrix);
  }

  /**
   * Create a copy of the supplied axis and angle instance.
   * 
   * @param axisAndAngle the instance whose contents are to be copied
   */
  public AxisAndAngle(AxisAndAngle axisAndAngle) {
    this.setTo(axisAndAngle);
  }

  /**
   * Retrieve the rotation axis.
   * <p>
   * There really is no difference between this method and the get method with the buffer argument,
   * except that this method does not perform a copy.
   * </p>
   * 
   * @return a reference to the internally held rotation axis.
   */
  public VectorIJK getAxis() {
    return axis;
  }

  /**
   * Retrieve a copy of the unit-length rotation axis.
   * 
   * @param buffer a buffer to receive the rotation axis.
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJK getAxis(VectorIJK buffer) {
    return buffer.setTo(axis);
  }

  /**
   * Get the rotation angle.
   * 
   * @return the rotation angle expressed in radians
   */
  public double getAngle() {
    return angle;
  }

  /**
   * Set the rotation axis.
   * 
   * @param axis the non-zero length axis of rotation.
   * 
   * @throws UnsupportedOperationException if axis has length zero.
   */
  public void setAxis(UnwritableVectorIJK axis) {
    this.axis.setTo(axis).unitize();
  }

  /**
   * Set the rotation angle.
   * 
   * @param angle the rotation angle specified in radians.
   */
  public void setAngle(double angle) {
    this.angle = angle;
  }

  /**
   * Simultaneously set the axis and angle.
   * 
   * @param axis the non-zero length rotation axis.
   * @param angle a rotation angle specified in radians.
   * 
   * @return a reference to the instance for convenience.
   * 
   * @throws UnsupportedOperationException if the supplied axis is of zero length.
   */
  public AxisAndAngle setTo(UnwritableVectorIJK axis, double angle) {
    this.axis.setTo(axis).unitize();
    this.angle = angle;
    return this;
  }

  /**
   * Copy the contents of the supplied axis and angle into the instance.
   * 
   * @param axisAndAngle the axis and angle whose contents are to be copied.
   * 
   * @return a reference to the instance for convenience.
   */
  public AxisAndAngle setTo(AxisAndAngle axisAndAngle) {
    this.axis.setTo(axisAndAngle.axis);
    this.angle = axisAndAngle.angle;
    return this;
  }

  /**
   * Set the axis and angle to the supplied axis components and angle.
   * 
   * @param axisi the ith component of a potentially non-unit length axis
   * @param axisj the jth component of a potentially non-unit length axis
   * @param axisk the kth component of a potentially non-unit length axis
   * @param angle the angle of rotation specified in radians
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws UnsupportedOperationException if the supplied axis components are all identially zero.
   */
  public AxisAndAngle setTo(double axisi, double axisj, double axisk, double angle) {
    axis.setTo(axisi, axisj, axisk).unitize();
    this.angle = angle;
    return this;
  }

  /**
   * {@inheritDoc}
   * 
   * In the event that the identity matrix is supplied to this method, the axis is selected to be
   * VectorIJK.K with a rotation angle of zero by convention. This is done to preserve the integrity
   * of the instance under configuration and to prevent the handling of unnecessary unchecked
   * exceptions.
   */
  @Override
  public AxisAndAngle setTo(UnwritableRotationMatrixIJK matrix) {

    Quaternion q = new Quaternion();

    /*
     * First convert the supplied matrix to a quaternion.
     */
    q.setTo(matrix);

    q.getVector(axis);

    /*
     * Handle the identity rotation case. By convention, all rotations that are identity rotations
     * have their axis as VectorIJK.K.
     */
    if (axis.getLength() == 0.0) {
      angle = 0;
      axis.setTo(VectorIJK.K);
      return this;
    }

    /*
     * Now handle the case when the rotation magnitude is Pi.
     */
    double scalar = q.getScalar();

    /*
     * There is no need to set the axis, as we already retrieved it with the q.getVector() method
     * above--since the scalar component is 0.0.
     */
    if (scalar == 0.0) {
      angle = PI;
      return this;
    }

    angle = 2.0 * atan2(axis.getLength(), scalar);
    axis.unitize();
    return this;
  }

  @Override
  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer) {

    if (angle == 0.0) {
      buffer.setTo(RotationMatrixIJK.IDENTITY);
      return buffer;
    }

    PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();

    VectorIJK vi = VectorIJK.rotate(VectorIJK.I, axis, angle);
    VectorIJK vj = VectorIJK.rotate(VectorIJK.J, axis, angle);
    VectorIJK vk = VectorIJK.rotate(VectorIJK.K, axis, angle);

    assigner.setToWithoutCheck(vi, vj, vk);
    buffer.setTo(assigner);

    return buffer;
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(angle);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((axis == null) ? 0 : axis.hashCode());
    return result;
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this considers the equality of the axis and angle only in the comparison. It is possible
   * that these fields, while different capture precisely the same rotation. This method only
   * evaluates whether the individual fields are identical. Note, classes that derive from this
   * class may be considered equal to instances of the parent if they have identical fields. This
   * method uses instanceof in its determination of whether to proceed, not Class comparison.
   */
  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof AxisAndAngle)) {
      return false;
    }
    final AxisAndAngle other = (AxisAndAngle) obj;
    if (Double.doubleToLongBits(angle) != Double.doubleToLongBits(other.angle)) {
      return false;
    }
    if (axis == null) {
      if (other.axis != null) {
        return false;
      }
    } else if (!axis.equals(other.axis)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return ("[" + String.valueOf(angle) + ", " + String.valueOf(axis) + "]");
  }

}
