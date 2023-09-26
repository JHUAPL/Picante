package picante.mechanics.rotations;

import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;

/**
 * Class pairing an angular rate with any supported rotation to create a differentiated rotation.
 * <p>
 * (Note: the following discussion borrows heavily from NAIF's RAV2XF and XF2RAV header.) The
 * supplied rotation component of this class can be thought of as a rotation that transforms FRAME1
 * to another frame FRAME2. Further, the supplied angular rate vector is the angular velocity of the
 * transformation. That is to say, if P is a position of a fixed point in FRAME2, then from the
 * point of view of FRAME1, P rotates in a right-handed sense about an axis parallel to the angular
 * rate. The rate of this rotation is captured by the length of the rate vector and is expressed in
 * units radians per unit time. Formally, the velocity V of P in FRAME 1 is given by:
 * 
 * <pre>
 *                 t
 *  V  = AV x ( ROT * P )
 * </pre>
 * 
 * </p>
 * <p>
 * In this context the state transform captured by or supplied to the methods of this class then
 * represents a transformation from FRAME1 to FRAME2.
 * </p>
 * <p>
 * Converting Between Angular Rate and Rotational Derivatives:
 * </p>
 * <p>
 * Recall that the internal rotation is a transformation that converts positions in some frame
 * FRAME1 to positions in another frame FRAME2. The angular velocity matrix, OMEGA (the cross
 * product matrix corresponding to AV) has the following property:
 * </p>
 * <p>
 * If P is the position of an object that is stationary with respect to FRAME2, then the velocity V
 * of that object in FRAME1 is given by:
 * 
 * <pre>
 *                   t
 *  V  =  OMEGA * ROT  *  P
 * </pre>
 * 
 * But V is also given by:
 * 
 * <pre>
 *             t
 *        d ROT
 *  V =   -----  * P
 *          dt
 * </pre>
 * 
 * So that:
 * 
 * <pre>
 *                           t
 *             t        d ROT
 *  OMEGA * ROT    =   -------
 *                        dt
 * </pre>
 * 
 * Hence,
 * 
 * <pre>
 * 
 *    d ROT                 t
 *    -----   =  ROT * OMEGA
 *      dt
 * </pre>
 * 
 * </p>
 * <p>
 * So, all that remains is to compute the transpose of OMEGA, since ROT is provided by the supplied
 * rotation directly.
 * </p>
 * <p>
 * Converting between Rotational Derivatives and Angular Rate Vectors:
 * </p>
 * <p>
 * This follows much as the discussion above, however, recognize the last relationship also implies:
 * 
 * <pre>
 *                     t
 *                d ROT
 *  OMEGA    =   -------  *  ROT
 *                  dt
 * </pre>
 * 
 * Then recall the form of OMEGA:
 * 
 * <pre>
 *  _                     _
 * |                       |
 * |   0    -AV(K)  AV(J)  |
 * |                       |
 * |  AV(K)    0   -AV(I)  |
 * |                       |
 * | -AV(J)   AV(I)   0    |
 * |_                     _|
 * </pre>
 * 
 * where AV is the angular velocity vector.
 * </p>
 * 
 * @param <R> the type of rotation to pair with an angular rate vector.
 */
public class WrapperWithRate<R extends Rotation> implements DifferentiatedRotation, Rotation {

  /**
   * A rotation from the base frame (FRAME1) to the reference frame (FRAME2).
   */
  private final R rotation;

  /**
   * An angular velocity vector expressing the rotation rate of the reference frame (FRAME2)
   * relative to the base frame (FRAME1) expressed in the base frame (FRAME1) of the rotation field.
   */
  private final VectorIJK rate = new VectorIJK(VectorIJK.ZERO);

  /**
   * Creates a new rotation and angular rate wrapper around the supplied rotation, with a zero
   * angular rate.
   * 
   * @param rotation the rotation to wrap, a reference to the value supplied is retained by the
   *        instance.
   */
  public WrapperWithRate(R rotation) {
    this.rotation = rotation;
  }

  /**
   * Creates a new rotation and angular rate wrapper around the supplied rotation and angular rate.
   * 
   * @param rotation the rotation to wrap, a reference to the value supplied is retained by the
   *        instance
   * @param rate the angular rate vector, the contents of which are copied to memory internal to the
   *        instance
   */
  public WrapperWithRate(R rotation, UnwritableVectorIJK rate) {
    this.rotation = rotation;
    this.rate.setTo(rate);
  }

  /**
   * Creates a new rotation and angular rate wrapper around the supplied rotation using the state
   * transform.
   * <p>
   * Note: whatever state the supplied rotation is currently in is discarded in favor of the
   * rotation component of the state transform. The constructor effectively executes:
   * 
   * <pre>
   * <code>
   *    rotation.setTo(transform.getRotation());
   * </code>
   * </pre>
   * 
   * </p>
   * 
   * @param rotation
   * @param transform
   */
  public WrapperWithRate(R rotation, UnwritableStateTransform transform) {
    this.rotation = rotation;
    this.setTo(transform);
  }

  /**
   * Retrieves the angular rate vector captured by this wrapper.
   * 
   * @return a reference to the internally held vector containing the angular rate of interest.
   */
  public VectorIJK getRate() {
    return this.rate;
  }

  /**
   * Retrieves the rotation captured by this angular rate wrapper.
   * 
   * @return a reference to the internally held rotation object.
   */
  @Override
  public R getRotation() {
    return this.rotation;
  }

  /**
   * Retrieves the angular rate vector component of the wrapper.
   * 
   * @return a reference to the internally held vector.
   */
  public VectorIJK getRate(VectorIJK buffer) {
    return buffer.setTo(this.rate);
  }

  /**
   * Sets the angular rate vector component of the wrapper.
   * 
   * @param velocity an angular velocity vector with compoents expressed in radians per second, in
   *        the base frame of the rotation
   */
  public void setRate(UnwritableVectorIJK velocity) {
    this.rate.setTo(velocity);
  }

  @Override
  public WrapperWithRate<R> setTo(UnwritableStateTransform transform) {

    UnwritableRotationMatrixIJK r = transform.getRotation();
    UnwritableMatrixIJK dr = transform.getRotationDerivative();

    rotation.setTo(transform.getRotation());

    /*
     * This is what we would like to do, but it requires retaining a reference to a matrix buffer.
     * So just perform the subset of the multiplications here in line, and return the result.
     */
    // MatrixIJK.mtxm(transform.getRotationDerivative(), transform
    // .getRotation(), matBuffer);
    // rate.setI(matBuffer.getKJ());
    // rate.setJ(matBuffer.getIK());
    // rate.setK(matBuffer.getJI());
    rate.setI(dr.getIK() * r.getIJ() + dr.getJK() * r.getJJ() + dr.getKK() * r.getKJ());
    rate.setJ(dr.getII() * r.getIK() + dr.getJI() * r.getJK() + dr.getKI() * r.getKK());
    rate.setK(dr.getIJ() * r.getII() + dr.getJJ() * r.getJI() + dr.getKJ() * r.getKI());

    return this;
  }

  @Override
  public StateTransform getTransform(StateTransform buffer) {

    RotationMatrixIJK r = buffer.getRotation();
    rotation.getRotation(r);

    /*
     * Use the buffer's derivative component as a temporary computation buffer. Populate it with the
     * transpose of OMEGA.
     */
    MatrixIJK drdt = buffer.getRotationDerivative();

    drdt.setTo(0.0, -rate.getK(), rate.getJ(), rate.getK(), 0.0, -rate.getI(), -rate.getJ(),
        rate.getI(), 0.0);

    // drdt.setII(0.0);
    // drdt.setJI(-rate.getK());
    // drdt.setKI(rate.getJ());
    // drdt.setIJ(rate.getK());
    // drdt.setJJ(0.0);
    // drdt.setKJ(-rate.getI());
    // drdt.setIK(-rate.getJ());
    // drdt.setJK(rate.getI());
    // drdt.setKK(0.0);

    MatrixIJK.mxm(r, drdt, drdt);

    return buffer;
  }

  @Override
  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer) {
    return rotation.getRotation(buffer);
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((rate == null) ? 0 : rate.hashCode());
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
    if (!(obj instanceof WrapperWithRate<?>)) {
      return false;
    }
    WrapperWithRate<?> other = (WrapperWithRate<?>) obj;
    if (rate == null) {
      if (other.rate != null) {
        return false;
      }
    } else if (!rate.equals(other.rate)) {
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

  @Override
  public String toString() {
    return "[" + rotation.toString() + ";" + rate.toString() + "]";
  }

  @Override
  public Rotation setTo(UnwritableRotationMatrixIJK matrix) {
    return rotation.setTo(matrix);
  }

}
