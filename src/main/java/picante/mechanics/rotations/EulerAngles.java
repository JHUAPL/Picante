package picante.mechanics.rotations;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.PI;
import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.acos;
import static picante.math.PicanteMath.asin;
import static picante.math.PicanteMath.atan2;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import static picante.units.FundamentalPhysicalConstants.HALFPI;
import java.util.HashSet;
import java.util.Set;

import com.google.common.collect.ImmutableList;
import picante.exceptions.BugException;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Class implementing all possible decompositions of a rotation into Euler angles.
 * <p>
 * The EulerAngles class is abstract, as it can not be instantiated directly. Instead users are to
 * elect which specific Euler decomposition of interest by using one of the specific static inner
 * classes that are publicly accessible.
 * </p>
 * <p>
 * Rotations can be considered of the form:
 * 
 * <pre>
 * R = [ LEFT_ANGLE ]          [ CENTER_ANGLE ]            [ RIGHT_ANGLE ]
 *                   LEFT_AXIS                 CENTER_AXIS                RIGHT_AXIS
 * </pre>
 * 
 * </p>
 * <p>
 * Note: Angles supplied to this class are not normalized to the standard ranges. In most cases it
 * would be wise to avoid feeding angles that are ridiculously outside the bounds of the standard
 * angle ranges. You can only expect this to work if the input angle lies in the canonical range
 * supported by the class:
 * 
 * <pre>
 *     EulerAngles angles = ...;
 *     double angle = ...;
 *     angles.setLeftAngle(angle);
 *     
 *     // Might be false! If angle lies outside canonical range.
 *     boolean mustBeTrue = (angle == angles.getLeftAngle);
 * </pre>
 * 
 * A convenience method to canonicalize the instance, if necessary, is provided. If you are placing
 * these instances into collections using {@link Object#equals(Object)} or {@link Object#hashCode()}
 * you may want to apply this method first to obtain the behavior you may be expecting.
 * </p>
 * <p>
 * Each concrete, inner subclass has three constructors, one taking angles, one taking an unwritable
 * rotation matrix, and one taking no arguments. The no argument constructors initialize the Euler
 * angles to identically zero.
 * </p>
 * 
 * @see EulerAngles.Axis for details of the individual component rotation definitions.
 */
public abstract class EulerAngles implements Rotation {

  /**
   * Defines an interval capturing the canonical range of angles for the left and right
   * decomposition angles. Note: containment is to be tested using the begin open contains method,
   * as the range is (-Math.PI,Math.PI].
   */
  private static final UnwritableInterval CANONICAL_ANGLE_RANGE = new UnwritableInterval(-PI, PI);

  /**
   * Normalize a list of column vectors.
   * 
   * @param rotation the rotation from which to extract columns for normalization
   * @param buffer the buffer to receive the normalized column vectors. On output it will contain
   *        the normalized column vectors of the supplied rotation.
   * @param work an array of at least length 3 to capture the column vectors temporarily.
   */
  private static void normColumnVectors(UnwritableRotationMatrixIJK rotation,
      RotationMatrixIJK buffer) {
    VectorIJK i = rotation.getIthColumn(new VectorIJK());
    VectorIJK j = rotation.getJthColumn(new VectorIJK());
    VectorIJK k = rotation.getKthColumn(new VectorIJK());
    i.unitize();
    j.unitize();
    k.unitize();
    PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();
    assigner.setToWithoutCheck(i, j, k);
    buffer.setTo(assigner);
  }

  /**
   * An enumeration describing the axes choices available for the Euler decomposition.
   */
  public enum Axis {

    /**
     * The first principal axis about which rotations are performed. Component rotations about this
     * axis take the form:
     * 
     * <pre>
    	 * +-                    -+
    	 * |  1      0       0    |
    	 * |                      |
    	 * |  0    cos(x)  sin(x) |.
    	 * |                      |
    	 * |  0   -sin(x)  cos(x) |
    	 * +-                    -+
     * </pre>
     */
    I(0) {
      @Override
      RotationMatrixIJK getRotation(double angle, RotationMatrixIJK buffer) {
        double cosAngle = cos(angle);
        double sinAngle = sin(angle);
        PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();
        assigner.setToWithoutCheck(1.0, 0.0, 0.0, 0.0, cosAngle, -sinAngle, 0.0, sinAngle,
            cosAngle);
        buffer.setTo(assigner);
        return buffer;
      }

      @Override
      Axis getCompletingAxis(Axis axis) {
        switch (axis) {
          case J:
            return K;
          case K:
            return J;
          default:
            throw new IllegalArgumentException(
                "Can not complete axis triplet with duplicate pair.");
        }
      }

      @Override
      double getCrossSign(Axis secondaryAxis) {
        switch (secondaryAxis) {
          case J:
            return 1.0;
          case K:
            return -1.0;
          default:
            throw new IllegalArgumentException(
                "Unable to estimate sign of cross product with self.");
        }
      }

    },

    /**
     * The second principal axis about which rotations are performed. Component rotations assume
     * this form:
     * 
     * <pre>
    	 * +-                    -+
    	 * | cos(x)   0   -sin(x) |
    	 * |                      |
    	 * |  0       1      0    |,
    	 * |                      |
    	 * | sin(x)   0    cos(x) |
    	 * +-                    -+
     * </pre>
     */
    J(1) {
      @Override
      RotationMatrixIJK getRotation(double angle, RotationMatrixIJK buffer) {
        double cosAngle = cos(angle);
        double sinAngle = sin(angle);
        PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();
        assigner.setToWithoutCheck(cosAngle, 0.0, sinAngle, 0.0, 1.0, 0.0, -sinAngle, 0.0,
            cosAngle);
        buffer.setTo(assigner);
        return buffer;
      }

      @Override
      Axis getCompletingAxis(Axis axis) {
        switch (axis) {
          case I:
            return K;
          case K:
            return I;
          default:
            throw new IllegalArgumentException(
                "Can not complete axis triplet with duplicate pair.");
        }
      }

      @Override
      double getCrossSign(Axis secondaryAxis) {
        switch (secondaryAxis) {
          case I:
            return -1.0;
          case K:
            return 1.0;
          default:
            throw new IllegalArgumentException(
                "Unable to estimate sign of cross product with self.");
        }
      }

    },

    /**
     * The third principal axis about which rotations are performed. Component rotations assume this
     * form:
     * 
     * <pre>
    	 * +-                    -+
    	 * |  cos(x)  sin(x)   0  |
    	 * |                      |
    	 * | -sin(x)  cos(x)   0  |.
    	 * |                      |
    	 * |  0        0       1  |
    	 * +-                    -+
     * </pre>
     */
    K(2) {
      @Override
      RotationMatrixIJK getRotation(double angle, RotationMatrixIJK buffer) {
        double cosAngle = cos(angle);
        double sinAngle = sin(angle);
        PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();
        assigner.setToWithoutCheck(cosAngle, -sinAngle, 0.0, sinAngle, cosAngle, 0.0, 0.0, 0.0,
            1.0);
        buffer.setTo(assigner);
        return buffer;
      }

      @Override
      Axis getCompletingAxis(Axis axis) {
        switch (axis) {
          case I:
            return J;
          case J:
            return I;
          default:
            throw new IllegalArgumentException(
                "Can not complete axis triplet with duplicate pair.");
        }
      }

      @Override
      double getCrossSign(Axis secondaryAxis) {
        switch (secondaryAxis) {
          case I:
            return 1.0;
          case J:
            return -1.0;
          default:
            throw new IllegalArgumentException(
                "Unable to estimate sign of cross product with self.");
        }
      }

    };

    private final int axisIndex;

    private Axis(int axisIndex) {
      this.axisIndex = axisIndex;
    }

    /**
     * Method used to retrieve the rotation matrix about the axis of the supplied angle.
     * 
     * @param angle the angle describing the magnitude of the rotation
     * @param buffer a buffer to capture the rotation matrix
     * @return a reference to buffer
     */
    abstract RotationMatrixIJK getRotation(double angle, RotationMatrixIJK buffer);

    /**
     * Retrieves the third axis from a triplet.
     * 
     * @param axis the second axis in the triplet.
     * 
     * @return the third axis in the triplet.
     * 
     * @throws IllegalArgumentException if the axis supplied is the same as the one on which the
     *         method is invoked.
     */
    abstract Axis getCompletingAxis(Axis axis);

    /**
     * @return the index (row or column) into a matrix or vector associated with this axis.
     */
    int getAxisIndex() {
      return axisIndex;
    }

    /**
     * Retrieves the sign of the cross product of basis vectors associated with the axis.
     * 
     * @param secondaryAxis the second axis in the cross product: this x secondaryAxis.
     * 
     * @return -1 or 1 depending on whether this x secondaryAxis results in a positive or negative
     *         basis vector
     * 
     * @throws IllegalArgumentException if the axis supplied is the same as the one on which the
     *         method is invoked.
     */
    abstract double getCrossSign(Axis secondaryAxis);
  }

  /**
   * Creates a list of EulerAngle instances the cover all possible concrete subclasses.
   * 
   * @return a newly created list of newly created instances.
   */
  public static ImmutableList<EulerAngles> createAllPossibleInstances() {
    return ImmutableList.of(new EulerAngles.IJI(), new EulerAngles.IJK(), new EulerAngles.IKI(),
        new EulerAngles.IKJ(), new EulerAngles.JIJ(), new EulerAngles.JIK(), new EulerAngles.JKJ(),
        new EulerAngles.JKI(), new EulerAngles.KIK(), new EulerAngles.KIJ(), new EulerAngles.KJK(),
        new EulerAngles.KJI());
  }

  /**
   * Creates an Euler angle instance of the appropriate type.
   * 
   * @param left the left axis (last to be applied)
   * @param center the center axis
   * @param right the right axis (first to be applied)
   * @param leftAngle the left rotation angle
   * @param centerAngle the center rotation angle
   * @param rightAngle the right rotation angle
   * 
   * @return a newly created instance of the axis triplet requested sub type.
   * 
   * @throws IllegalArgumentException if left equals center, or right equals center.
   */
  public static EulerAngles create(Axis left, Axis center, Axis right, double leftAngle,
      double centerAngle, double rightAngle) {

    checkArgument(!left.equals(center) && !right.equals(center),
        "Center axis can not equal left or right.");

    /*
     * This is hideous, but it works well enough for the purposes of this method's implementation.
     */
    if (left.equals(Axis.I) && center.equals(Axis.J) && right.equals(Axis.I)) {
      return new EulerAngles.IJI(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.I) && center.equals(Axis.K) && right.equals(Axis.I)) {
      return new EulerAngles.IKI(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.J) && center.equals(Axis.I) && right.equals(Axis.J)) {
      return new EulerAngles.JIJ(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.J) && center.equals(Axis.K) && right.equals(Axis.J)) {
      return new EulerAngles.JKJ(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.K) && center.equals(Axis.I) && right.equals(Axis.K)) {
      return new EulerAngles.KIK(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.K) && center.equals(Axis.J) && right.equals(Axis.K)) {
      return new EulerAngles.KJK(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.I) && center.equals(Axis.J) && right.equals(Axis.K)) {
      return new EulerAngles.IJK(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.I) && center.equals(Axis.K) && right.equals(Axis.J)) {
      return new EulerAngles.IKJ(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.J) && center.equals(Axis.I) && right.equals(Axis.K)) {
      return new EulerAngles.JIK(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.J) && center.equals(Axis.K) && right.equals(Axis.I)) {
      return new EulerAngles.JKI(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.K) && center.equals(Axis.I) && right.equals(Axis.J)) {
      return new EulerAngles.KIJ(leftAngle, centerAngle, rightAngle);
    }

    if (left.equals(Axis.K) && center.equals(Axis.J) && right.equals(Axis.I)) {
      return new EulerAngles.KJI(leftAngle, centerAngle, rightAngle);
    }

    throw new BugException("If the code reaches here then something has gone horribly wrong.");
  }

  /**
   * Creates a set of all &quot;possible&quot; Euler angle decompositions of the supplied rotation
   * matrix.
   * 
   * @param matrix the matrix to compute decompositions of
   * 
   * @return a newly created set of angles.
   */
  public static Set<EulerAngles> createDecompositions(UnwritableRotationMatrixIJK matrix) {

    UnwritableRotationMatrixIJK transpose = matrix.createTranspose();

    Set<EulerAngles> result = new HashSet<>();

    for (EulerAngles angles : createAllPossibleInstances()) {
      angles.setTo(matrix);
      result.add(angles);
      result.add(angles.createAlternateDecomposition(transpose));
    }

    return result;

  }

  /**
   * Creates a copy of the supplied instance.
   * <p>
   * This method creates a new instance of an Euler angle using the specific subtype specified by
   * the retrieval of the three axes.
   * </p>
   * 
   * @param angles the angles value to copy.
   * 
   * @return a newly created copy of the supplied angles instance.
   * 
   */
  public static EulerAngles copyOf(EulerAngles angles) {
    return create(angles.getLeftAxis(), angles.getCenterAxis(), angles.getRightAxis(),
        angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle());
  }

  private double leftAngle;
  private double centerAngle;
  private double rightAngle;
  private final Axis leftAxis;
  private final Axis centerAxis;
  private final Axis rightAxis;

  /**
   * This constructor is marked private to prevent external classes that might opt to subclass it
   * from properly instantiating the parent.
   * 
   * @param leftAxis the axis of the leftmost component rotation
   * @param centerAxis the axis of the center component rotation
   * @param rightAxis the axis of the rightmost component rotation
   * @param leftAngle the angle of the leftmost component rotation
   * @param centerAngle the angle of the center component rotation
   * @param rightAngle the angle of the rightmost component rotation
   */
  private EulerAngles(Axis leftAxis, Axis centerAxis, Axis rightAxis, double leftAngle,
      double centerAngle, double rightAngle) {
    this.leftAxis = leftAxis;
    this.centerAxis = centerAxis;
    this.rightAxis = rightAxis;
    this.leftAngle = leftAngle;
    this.centerAngle = centerAngle;
    this.rightAngle = rightAngle;
  }

  /**
   * Retrieves the angle associated with the left component rotation.
   * 
   * @return an angle in radians
   */
  public double getLeftAngle() {
    return leftAngle;
  }

  /**
   * Sets the angle associated with the left component rotation.
   * 
   * @param leftAngle an angle in radians
   */
  public void setLeftAngle(double leftAngle) {
    this.leftAngle = leftAngle;
  }

  /**
   * Retrieves the angle associated with the center component rotation.
   * 
   * @return an angle in radians
   */
  public double getCenterAngle() {
    return centerAngle;
  }

  /**
   * Sets the angle associated with the center component rotation.
   * <p>
   * The value supplied is canonicalized against the appropriate range of values for this
   * decomposition.
   * </p>
   * 
   * @param centerAngle an angle in radians
   */
  public void setCenterAngle(double centerAngle) {
    this.centerAngle = centerAngle;
  }

  /**
   * Retrieves the angle associated with the right component rotation.
   * 
   * @return an angle in radians
   */
  public double getRightAngle() {
    return rightAngle;
  }

  /**
   * Sets the angle associated with the right component rotation.
   * <p>
   * The value supplied is canonicalized against the appropriate range of values for this
   * decomposition.
   * </p>
   * 
   * @param rightAngle an angle in radians
   */
  public void setRightAngle(double rightAngle) {
    this.rightAngle = rightAngle;
  }

  /**
   * Retrieves the axis associated with the left component rotation.
   * 
   * @return the axis of interest
   */
  public Axis getLeftAxis() {
    return leftAxis;
  }

  /**
   * Retrieves the axis associated with the center component rotation.
   * 
   * @return the axis of interest
   */
  public Axis getCenterAxis() {
    return centerAxis;
  }

  /**
   * Retrieves the axis associated with the right component rotation.
   * 
   * @return the axis of interest
   */
  public Axis getRightAxis() {
    return rightAxis;
  }

  /**
   * Set all three angles simultaneously.
   * <p>
   * The values are canonicalized to the ranges appropriate for this instance.
   * </p>
   * 
   * @param leftAngle the left component rotation angle, in radians
   * @param centerAngle the center component rotation angle, in radians
   * @param rightAngle the right component rotation angle, in radians
   */
  public void set(double leftAngle, double centerAngle, double rightAngle) {
    setLeftAngle(leftAngle);
    setCenterAngle(centerAngle);
    setRightAngle(rightAngle);
  }

  @Override
  public EulerAngles setTo(UnwritableRotationMatrixIJK matrix) {
    configureToRotation(matrix);
    return this;
  }

  /**
   * {@inheritDoc}
   * 
   * The format of the string, subject to change, is at the moment:
   * 
   * <pre>
   * (LCR)[left, center, right]
   * </pre>
   * 
   * where L,C,R are the single letter axis identifiers and left, center, right are the angles
   * expressed in radians.
   */
  @Override
  public String toString() {
    return "(" + leftAxis + centerAxis + rightAxis + ")[" + String.valueOf(leftAngle) + ", "
        + String.valueOf(centerAngle) + ", " + String.valueOf(rightAngle) + "]";
  }

  /**
   * Configures this instance of the Euler decomposition to the supplied matrix.
   * 
   * @param rotation the rotation to decompose into Euler angles
   */
  abstract void configureToRotation(UnwritableRotationMatrixIJK rotation);

  /**
   * Retrieves the canonical range for the central angle in the decomposition.
   * 
   * @return an unwritable interval containing the range
   */
  abstract UnwritableInterval getCenterAngleRange();

  /**
   * Package private method used to create an alternate decomposition other than the canonical one.
   * This method exists to support
   * {@link EulerAngles#createDecompositions(UnwritableRotationMatrixIJK)}
   * 
   * @param transpose transpose of the rotation to decompose into Euler angles
   * 
   * @return
   */
  abstract EulerAngles createAlternateDecomposition(UnwritableRotationMatrixIJK rotation);

  /**
   * If necessary converts the angles defined on the instance into their canonical ranges.
   * 
   * @return reference to the instance for convenience
   */
  public EulerAngles canonicalize() {

    /*
     * First check to see if there's nothing to be done.
     */
    if (CANONICAL_ANGLE_RANGE.beginOpenContains(leftAngle)
        && getCenterAngleRange().closedContains(centerAngle)
        && CANONICAL_ANGLE_RANGE.beginOpenContains(rightAngle)) {
      return this;
    }

    /*
     * The simplest way to canonicalize the angles is to convert this to a rotation and back.
     */
    RotationMatrixIJK canonicalizer = new RotationMatrixIJK();
    getRotation(canonicalizer);
    setTo(canonicalizer);
    return this;
  }

  @Override
  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer) {
    rightAxis.getRotation(rightAngle, buffer);
    RotationMatrixIJK umat = centerAxis.getRotation(centerAngle, new RotationMatrixIJK());
    RotationMatrixIJK.mxm(umat, buffer, buffer);
    leftAxis.getRotation(leftAngle, umat);
    RotationMatrixIJK.mxm(umat, buffer, buffer);
    return buffer;
  }

  /**
   * {@inheritDoc}
   * 
   * This function is implemented on the abstract parent class, because of the fact the
   * implementation retains references to the fields.
   */
  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(centerAngle);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((centerAxis == null) ? 0 : centerAxis.hashCode());
    temp = Double.doubleToLongBits(leftAngle);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((leftAxis == null) ? 0 : leftAxis.hashCode());
    temp = Double.doubleToLongBits(rightAngle);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((rightAxis == null) ? 0 : rightAxis.hashCode());
    return result;
  }

  /**
   * {@inheritDoc}
   * 
   * This function is implemented on the abstract parent class, because of the fact the
   * implementation retains references to the fields.
   */
  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    EulerAngles other = (EulerAngles) obj;
    if (Double.doubleToLongBits(centerAngle) != Double.doubleToLongBits(other.centerAngle)) {
      return false;
    }
    if (centerAxis != other.centerAxis) {
      return false;
    }
    if (Double.doubleToLongBits(leftAngle) != Double.doubleToLongBits(other.leftAngle)) {
      return false;
    }
    if (leftAxis != other.leftAxis) {
      return false;
    }
    if (Double.doubleToLongBits(rightAngle) != Double.doubleToLongBits(other.rightAngle)) {
      return false;
    }
    if (rightAxis != other.rightAxis) {
      return false;
    }
    return true;
  }

  /**
   * Package private class used to aid in the implementation of the ABA style Euler decompositions.
   * It is private because it is purely an implementation detail used to consolidate code.
   */
  static abstract class EulerAnglesABA extends EulerAngles {

    /**
     * The range of acceptable values for the center angle in ABA style decompositions.
     */
    private static final UnwritableInterval CENTER_ANGLE_RANGE = new UnwritableInterval(0, PI);

    private final UnwritableRotationMatrixIJK basisChange;

    private EulerAnglesABA(UnwritableRotationMatrixIJK basisChange, Axis leftAxis, Axis centerAxis,
        Axis rightAxis) {
      this(basisChange, leftAxis, centerAxis, rightAxis, 0.0, 0.0, 0.0);
    }

    private EulerAnglesABA(UnwritableRotationMatrixIJK basisChange, Axis leftAxis, Axis centerAxis,
        Axis rightAxis, double leftAngle, double centerAngle, double rightAngle) {
      super(leftAxis, centerAxis, rightAxis, leftAngle, centerAngle, rightAngle);
      this.basisChange = basisChange;
    }

    @Override
    EulerAngles createAlternateDecomposition(UnwritableRotationMatrixIJK transpose) {

      EulerAngles result =
          create(this.getLeftAxis(), this.getCenterAxis(), this.getRightAxis(), 0, 0, 0);

      result.setTo(transpose);

      result.set(-result.getRightAngle(), -result.getCenterAngle(), -result.getLeftAngle());

      return result;
    }

    @Override
    void configureToRotation(UnwritableRotationMatrixIJK rotation) {

      /*
       * Unitize the columns of the supplied rotation matrix prior to proceeding.
       */
      RotationMatrixIJK vmat = new RotationMatrixIJK();
      normColumnVectors(rotation, vmat);

      /*
       * The basis change matrix is designed to convert whatever the natural ordering of the Euler
       * this is into a KIK rotation. It does this in the following manner:
       * 
       * transpose(basisChange) * rotation * basisChange
       * 
       * which can then be factored into the appropriate this.directly. This is consistent with the
       * SPICE M2EUL algorithm.
       */
      RotationMatrixIJK tmp = new RotationMatrixIJK();

      RotationMatrixIJK.mxm(vmat, basisChange, tmp);
      RotationMatrixIJK.mtxm(basisChange, tmp, tmp);

      /*
       * Determine whether we are in a degenerate case.
       */
      if (((tmp.get(0, 2) == 0.0) && (tmp.get(1, 2) == 0.0))
          || ((tmp.get(2, 0) == 0.0) && (tmp.get(2, 1) == 0.0)) || (abs(tmp.get(2, 2)) == 1.0)) {
        super.leftAngle = 0.0;
        super.centerAngle = acos(tmp.get(2, 2));
        super.rightAngle = atan2(tmp.get(0, 1), tmp.get(0, 0));
        return;
      }

      super.leftAngle = atan2(tmp.get(0, 2), tmp.get(1, 2));
      super.centerAngle = acos(tmp.get(2, 2));
      super.rightAngle = atan2(tmp.get(2, 0), -tmp.get(2, 1));

    }

    /**
     * {@inheritDoc}
     * 
     * The range returned is [0, Math.PI]
     */
    @Override
    UnwritableInterval getCenterAngleRange() {
      return CENTER_ANGLE_RANGE;
    }

  }

  /**
   * The I-J-I Euler decomposition.
   */
  public static final class IJI extends EulerAnglesABA {

    /**
     * <pre>
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000
     * </pre>
     */
    private final static UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0);

    public IJI() {
      super(CHANGE, Axis.I, Axis.J, Axis.I);
    }

    public IJI(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, Axis.I, Axis.J, Axis.I, leftAngle, centerAngle, rightAngle);
    }

    public IJI(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.IJI setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.IJI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The I-K-I Euler decomposition.
   */
  public static final class IKI extends EulerAnglesABA {

    /**
     * <pre>
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000     
    	 * 0.0000000000000000       -1.0000000000000000        0.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000
     * </pre>
     */
    private final static UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 0.0, 1.0, 0.0, -1.0, 0.0, 1.0, 0.0, 0.0);

    public IKI() {
      super(CHANGE, Axis.I, Axis.K, Axis.I);
    }

    public IKI(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, Axis.I, Axis.K, Axis.I, leftAngle, centerAngle, rightAngle);
    }

    public IKI(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.IKI setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.IKI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-I-J Euler decomposition.
   */
  public static final class JIJ extends EulerAnglesABA {

    /**
     * <pre>
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000     
    	 * 0.0000000000000000       -1.0000000000000000        0.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0);

    public JIJ() {
      super(CHANGE, Axis.J, Axis.I, Axis.J);
    }

    public JIJ(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, Axis.J, Axis.I, Axis.J, leftAngle, centerAngle, rightAngle);
    }

    public JIJ(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.JIJ setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.JIJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-K-J Euler decomposition.
   */
  public static final class JKJ extends EulerAnglesABA {

    /**
     * <pre>
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0);

    public JKJ() {
      super(CHANGE, Axis.J, Axis.K, Axis.J);
    }

    public JKJ(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, Axis.J, Axis.K, Axis.J, leftAngle, centerAngle, rightAngle);
    }

    public JKJ(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.JKJ setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.JKJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-I-K Euler decomposition.
   */
  public static final class KIK extends EulerAnglesABA {

    /**
     * <pre>
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

    public KIK() {
      super(CHANGE, Axis.K, Axis.I, Axis.K);
    }

    public KIK(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, Axis.K, Axis.I, Axis.K, leftAngle, centerAngle, rightAngle);
    }

    public KIK(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.KIK setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.KIK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-J-K Euler decomposition.
   */
  public static final class KJK extends EulerAnglesABA {

    /**
     * <pre>
    	 * 0.0000000000000000       -1.0000000000000000        0.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0);

    public KJK() {
      super(CHANGE, Axis.K, Axis.J, Axis.K);
    }

    public KJK(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, Axis.K, Axis.J, Axis.K, leftAngle, centerAngle, rightAngle);
    }

    public KJK(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.KJK setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.KJK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * Package private class used to aid in the implementation of the ABC style Euler decompositions.
   * It is private because it is purely an implementation detail used to consolidate code.
   */
  static abstract class EulerAnglesABC extends EulerAngles {

    /**
     * Acceptable range for center angles in the ABC style decomposition.
     */
    private static final UnwritableInterval CENTER_ANGLE_RANGE =
        new UnwritableInterval(-HALFPI, HALFPI);

    private final UnwritableRotationMatrixIJK basisChange;

    /**
     * Assumes one of two possible values: -1.0 if the rotation is left handed and 1.0 if it is
     * right handed. The constructor does not check this, as only code generated in this class can
     * invoke this constructor, so there is no reason to validate it.
     */
    private final double sign;

    private EulerAnglesABC(UnwritableRotationMatrixIJK basisChange, double sign, Axis leftAxis,
        Axis centerAxis, Axis rightAxis) {
      this(basisChange, sign, leftAxis, centerAxis, rightAxis, 0.0, 0.0, 0.0);
    }

    private EulerAnglesABC(UnwritableRotationMatrixIJK basisChange, double sign, Axis leftAxis,
        Axis centerAxis, Axis rightAxis, double leftAngle, double centerAngle, double rightAngle) {
      super(leftAxis, centerAxis, rightAxis, leftAngle, centerAngle, rightAngle);
      this.basisChange = basisChange;
      this.sign = sign;
    }

    @Override
    EulerAngles createAlternateDecomposition(UnwritableRotationMatrixIJK transpose) {

      EulerAngles flipped =
          create(this.getRightAxis(), this.getCenterAxis(), this.getLeftAxis(), 0, 0, 0);
      flipped.setTo(transpose);

      return create(this.getLeftAxis(), this.getCenterAxis(), this.getRightAxis(),
          -flipped.getRightAngle(), -flipped.getCenterAngle(), -flipped.getLeftAngle());

    }

    @Override
    void configureToRotation(UnwritableRotationMatrixIJK rotation) {

      /*
       * Unitize the columns of the supplied rotation matrix prior to proceeding.
       */
      RotationMatrixIJK vmat = new RotationMatrixIJK();
      normColumnVectors(rotation, vmat);

      /*
       * The basis change matrix is designed to convert whatever the natural ordering of the Euler
       * super.is into a KJI rotation. It does this in the following manner:
       * 
       * transpose(basisChange) * rotation * basisChange
       * 
       * which can then be factored into the appropriate super.directly. This is consistent with the
       * SPICE M2EUL algorithm.
       */
      RotationMatrixIJK tmp = new RotationMatrixIJK();

      RotationMatrixIJK.mxm(vmat, basisChange, tmp);
      RotationMatrixIJK.mtxm(basisChange, tmp, tmp);

      /*
       * Determine whether we are in a degenerate case.
       */
      if (((tmp.get(0, 0) == 0.0) && (tmp.get(0, 1) == 0.0))
          || ((tmp.get(1, 2) == 0.0) && (tmp.get(2, 2) == 0.0)) || (abs(tmp.get(0, 2)) == 1.0)) {
        super.leftAngle = 0.0;
        super.centerAngle = asin(-tmp.get(0, 2));
        super.rightAngle = sign * atan2(-tmp.get(1, 0), tmp.get(1, 1));
        return;
      }
      super.leftAngle = atan2(tmp.get(1, 2), tmp.get(2, 2));
      super.centerAngle = asin(-tmp.get(0, 2));
      super.rightAngle = sign * atan2(tmp.get(0, 1), tmp.get(0, 0));

    }

    /**
     * {@inheritDoc}
     * 
     * The range returned is [-Math.PI/2.0, Math.PI/2.0]
     */
    @Override
    UnwritableInterval getCenterAngleRange() {
      return CENTER_ANGLE_RANGE;
    }

  }

  /**
   * The I-J-K, right-handed Euler decomposition.
   */
  public static final class IJK extends EulerAnglesABC {

    /**
     * <pre>
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000
     * </pre>
     */
    private final static UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

    public IJK() {
      super(CHANGE, 1.0, Axis.I, Axis.J, Axis.K);
    }

    public IJK(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, 1.0, Axis.I, Axis.J, Axis.K, leftAngle, centerAngle, rightAngle);
    }

    public IJK(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.IJK setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.IJK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The I-K-J, left-handed Euler decomposition.
   */
  public static final class IKJ extends EulerAnglesABC {

    /**
     * <pre>
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000       -1.0000000000000000     
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000
     * </pre>
     */
    private final static UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, -1.0, 0.0);

    public IKJ() {
      super(CHANGE, -1.0, Axis.I, Axis.K, Axis.J);
    }

    public IKJ(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, -1.0, Axis.I, Axis.K, Axis.J, leftAngle, centerAngle, rightAngle);
    }

    public IKJ(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.IKJ setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.IKJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-I-K, left-handed Euler decomposition.
   */
  public static final class JIK extends EulerAnglesABC {

    /**
     * <pre>
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000       -1.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0);

    public JIK() {
      super(CHANGE, -1.0, Axis.J, Axis.I, Axis.K);
    }

    public JIK(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, -1.0, Axis.J, Axis.I, Axis.K, leftAngle, centerAngle, rightAngle);
    }

    public JIK(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.JIK setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.JIK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-K-I, right-handed Euler decomposition.
   */
  public static final class JKI extends EulerAnglesABC {

    /**
     * <pre>
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0);

    public JKI() {
      super(CHANGE, 1.0, Axis.J, Axis.K, Axis.I);
    }

    public JKI(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, 1.0, Axis.J, Axis.K, Axis.I, leftAngle, centerAngle, rightAngle);
    }

    public JKI(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.JKI setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.JKI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-I-J, right-handed Euler decomposition.
   */
  public static final class KIJ extends EulerAnglesABC {

    /**
     * <pre>
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000     
    	 * 0.0000000000000000        0.0000000000000000        1.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0);

    public KIJ() {
      super(CHANGE, 1.0, Axis.K, Axis.I, Axis.J);
    }

    public KIJ(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, 1.0, Axis.K, Axis.I, Axis.J, leftAngle, centerAngle, rightAngle);
    }

    public KIJ(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.KIJ setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.KIJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-J-I, left-handed Euler decomposition.
   */
  public static final class KJI extends EulerAnglesABC {

    /**
     * <pre>
    	 * 0.0000000000000000        0.0000000000000000       -1.0000000000000000     
    	 * 0.0000000000000000        1.0000000000000000        0.0000000000000000     
    	 * 1.0000000000000000        0.0000000000000000        0.0000000000000000
     * </pre>
     */
    private static final UnwritableRotationMatrixIJK CHANGE =
        new UnwritableRotationMatrixIJK(0.0, 0.0, 1.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0);

    public KJI() {
      super(CHANGE, -1.0, Axis.K, Axis.J, Axis.I);
    }

    public KJI(double leftAngle, double centerAngle, double rightAngle) {
      super(CHANGE, -1.0, Axis.K, Axis.J, Axis.I, leftAngle, centerAngle, rightAngle);
    }

    public KJI(UnwritableRotationMatrixIJK matrix) {
      this();
      configureToRotation(matrix);
    }

    @Override
    public EulerAngles.KJI setTo(UnwritableRotationMatrixIJK matrix) {
      super.setTo(matrix);
      return this;
    }

    @Override
    public EulerAngles.KJI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

}
