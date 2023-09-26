package picante.mechanics.rotations;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import picante.exceptions.BugException;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;
import picante.mechanics.rotations.EulerAngles.Axis;
import picante.mechanics.rotations.EulerAngles.EulerAnglesABA;
import picante.mechanics.rotations.EulerAngles.EulerAnglesABC;

/**
 * Class implementing all possible decompositions of a rotation into Euler angles with derivatives.
 * <p>
 * The DifferentiatedEulerAngles class is abstract, as it can not be instantiated directly. Instead
 * users are to elect which specific decomposition is of interest by using one of the specific
 * static inner classes that are publicly accessible.
 * </p>
 * <p>
 * For a detailed discussion of Euler decompositions, refer to {@link EulerAngles} for details. This
 * class implements the Euler angle conversions with the addition of angle derivatives. As a brief
 * reminder, rotations can be considered of the form:
 * 
 * <pre>
 * R = [ LEFT_ANGLE ]          [ CENTER_ANGLE ]            [ RIGHT_ANGLE ]
 *                   LEFT_AXIS                 CENTER_AXIS                RIGHT_AXIS
 * </pre>
 * 
 * And this class provides these conversions along with the derivatives of these angles with respect
 * to some parameter, typically time.
 * </p>
 * <p>
 * There is a uniqueness issue in the conversion between an arbitrary state transformation and an
 * Euler decomposition with derivatives. The code here detects when this occurs and selects one of
 * the two possible derivatives. At the moment there is on mechanism to warn the user that this has
 * happened, it may be added in a future release of this class.
 * </p>
 * <p>
 * This class utilizes the axis enumeration {@link EulerAngles.Axis} rather than creating a
 * duplicate enumeration for its own purposes.
 * </p>
 * <p>
 * What follows is a detailed discussion of the derivation of the mathematics utilized in this class
 * for converting back and forth between Euler angles and their derivatives and state transformation
 * matrices. This documentation was adapted directly from the NAIF SPICELIB routines XF2EUL and
 * EUL2XF. These notes can be considered implementation details and safely ignored by consumers of
 * this class.
 * </p>
 * <p>
 * The computation of the non-derivative terms is handled by an internally captured reference to the
 * appropriate instance of {@link EulerAngles}. This class simply extends the functionality provided
 * there to include derivatives components. The discussion below routinely refers to basis vectors
 * A, B, and L and integer indices associated with them. SPICE is written in FORTRAN, so indices
 * begin at 1. A is the left axis, B is the center axis, and L is the axis that completes the
 * triplet {@link EulerAngles.Axis#getCompletingAxis(Axis))}.
 * </p>
 * <p>
 * First we note that if b is one of the basis vectors i,j, or k or the opposite of one of these
 * (-i, -j, or -k) then
 * </p>
 * 
 * <pre>
 *     [ ANGLE ]  * b  = COS( {1 - |<e_n,b>|}*ANGLE )b
 *              n
 *                     - SIN( ANGLE ) e_n x b
 * </pre>
 * <p>
 * where &lt;,&gt; denotes the dot product, and x is used to denote the cross product operation and
 * e_1, e_2, and e_3 are the standard basis vectors i, j, and k respectively.
 * </p>
 * <p>
 * Using {@link EulerAngles} we can readily determine the values of ALPHA, BETA and GAMMA such that
 * </p>
 * 
 * <pre>
 *      R   = [ ALPHA ]  [ BETA ]  [ GAMMA ]
 *                     A         B          C
 * </pre>
 * <p>
 * From this equation we have:
 * </p>
 * 
 * <pre>
 *      dR/dt =   dALPHA/dt OMEGA [ ALPHA ]  [ BETA ]  [ GAMMA ]
 *                               A         A         B          C
 * 
 *            +   dBETA/dt  [ ALPHA ] OMEGA  [ BETA ]  [ GAMMA ]
 *                                   A     B         B          C
 *                                   
 *            +   dGAMMA/dt [ ALPHA ] [ BETA ]  OMEGA [ GAMMA ]
 *                                   A        B      C         C
 * 
 *   where OMEGA   is the cross product matrix.
 *              n
 * 
 *       [   0      D_3n    -D_2n  ]
 *       |  -D_3n    0       D_1n  |
 *       [   D_2n  -D_1n      0    ]
 * 
 *   (D_ij   denotes the Kronecker delta.)  Note that OMEGA * v
 *                                                         n
 *   yields -e  x  v  for all vectors v.
 *            n
 * </pre>
 * <p>
 * Multiplying both sides of the equation for dR/dt by the transpose of R yields:
 * </p>
 * 
 * <pre>
 *          T
 *   dR/dt*R  = dALPHA/dt OMEGA
 *                              A
 *                              
 *            + dBETA/dt  [ ALPHA ] OMEGA  [ -ALPHA ]
 *                                 A     B           A
 * 
 *            + dGAMMA/dt [ ALPHA ] [ BETA ] OMEGA [ -BETA ]  [-ALPHA]
 *                                 A        B     C         B         A
 *                      T
 *   The product dR/dt*R  is a skew symmetric matrix and hence can
 *   be represented as a cross product,
 *   
 *             T
 *      dR/dt*R  V  = W x V
 * 
 *   for all vectors V, provided that
 * 
 *                     T
 *      W(1) =  dR/dt*R  (3,2)
 * 
 *                     T
 *      W(2) =  dR/dt*R  (1,3)
 * 
 *                     T
 *      W(3) =  dR/dt*R  (2,1)
 * </pre>
 * <p>
 * For any vector V, there is a corresponding skew symmetric matrix CROSS{V} such that CROSS{V} * W
 * = V x W for all vectors W. Moreover, if ROT is any rotation, then
 * </p>
 * 
 * <pre>
 *                                     T
 *     CROSS{ROT(V)} = ROT CROSS{V} ROT
 * </pre>
 * <p>
 * This can easily be verified by noting that
 * </p>
 * 
 * <pre>
 *      ROT(VxU) = ROT(V) X ROT(U)
 * </pre>
 * <p>
 * From these observations it follows that
 * </p>
 * 
 * <pre>
 * 
 *      W =   -dALPHA/dt e_A
 * 
 * 
 *        -    dBETA/dt [ALPHA]  e_B
 *                             A
 * 
 *        -    dGAMMA/dt [ ALPHA ] [ BETA ] e_C
 *                                A        B
 * 
 * 
 *      W =   -dALPHA/dt e_A
 * 
 * 
 *        -    dBETA/dt {    COS ( ALPHA (1 - |<e_A,e_B>|)) e_B
 * 
 *                        -  SIN ( ALPHA ) e_A x e_B }
 * 
 * 
 *        -    dGAMMA/dt [ ALPHA ] {    COS(BETA(1 - |<e_B,e_C>|)) e_C
 *                                A
 *                                   -  SIN (BETA) e_B x e_C }
 * </pre>
 * <p>
 * But <e_A,e_B> = 0 = <e_B,e_C> so that the above expression simplifies to:
 * </p>
 * 
 * <pre>
 *      W =   -dALPHA/dt e_A
 *      
 * 
 *        -    dBETA/dt {COS(ALPHA)e_B -  SIN(ALPHA) e_A x e_B}
 * 
 * 
 *        -    dGAMMA/dt [ ALPHA ] {COS(BETA)e_C - SIN(BETA)e_B x e_C}
 *                                A
 * </pre>
 * <p>
 * If we let L = 6 - A - B, then by construction e_L is the third vector needed to complete the
 * basis containing e_A and e_B. Let D be +1 or -1, so that D*e_L = e_A x e_B (note D = <e_L,e_A x
 * e_B> )
 * </p>
 * <p>
 * Then applying our rotation formula again and simplifying we have
 * </p>
 * 
 * <pre>
 *   W =   -dALPHA/dt e_A
 * 
 * 
 *     -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
 * 
 * 
 *     -  dGAMMA/dt COS(BETA){ COS(ALPHA(1-<e_A , e_C>))e_C
 *                            -SIN(ALPHA)   e_A x e_C }
 * 
 *     +  dGAMMA/dt SIN(BETA){ COS(ALPHA(1-|<e_A,e_B x e_C>|))e_B x e_C
 *                            -SIN(ALPHA) e_A x (e_B x e_C )
 * 
 * </pre>
 * <p>
 * Now we have two cases: 1) e_A = e_C or 2) e_C = e_L
 * </p>
 * 
 * <pre>
 *   Case 1. e_A = e_C
 *   ====================
 * 
 *      W =   -dALPHA/dt e_A
 * 
 * 
 *        -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
 * 
 *        -  dGAMMA/dt COS(BETA)e_A
 * 
 *        -  dGAMMA/dt D*SIN(BETA)COS(ALPHA)e_L
 *         
 *        -  dGAMMA/dt SIN(BETA)SIN(ALPHA)e_B
 * 
 * 
 *      W = e_A{-dALPHA/dt - COS(BETA)dGAMMA/dt}
 *        + e_B{ -COS(ALPHA)dBETA/dt -   SIN(ALPHA)SIN(BETA)dGAMMA/dt}
 *        + e_L{D*SIN(ALPHA)dBETA/dt - D*COS(ALPHA)SIN(BETA)dGAMMA/dt}
 * 
 * 
 *      let U =    COS(BETA)
 *          V =  D*SIN(BETA)
 * 
 *      then
 * 
 *      W = e_A{-dALPHA/dt                                -U*dGAMMA/dt}
 *        + e_B{         -COS(ALPHA)dBETA/dt -D*SIN(ALPHA)*V*dGAMMA/dt}
 *        + e_L{        D*SIN(ALPHA)dBETA/dt   -COS(ALPHA)*V*dGAMMA/dt}
 * 
 * 
 *   Case 2. e_L = e_C
 *   ====================
 * 
 *      W =   -dALPHA/dt e_A
 * 
 * 
 *        -  dBETA/dt {COS(ALPHA)e_B -  D*SIN(ALPHA) e_L }
 * 
 * 
 *        -  dGAMMA/dt COS(BETA){ COS(ALPHA)e_L
 *                               -D*SIN(ALPHA)e_B }
 * 
 *        +  dGAMMA/dt SIN(BETA) D*e_A
 * 
 * 
 *     W  = e_A{-dALPHA/dt + D*SIN(BETA)dGAMMA/dt}
 *        + e_B{-COS(ALPHA)dBETA/dt  - D*SIN(ALPHA)COS(BETA)dGAMMA/dt}
 *        + e_L{D*SIN(ALPHA)dBETA/dt -   COS(ALPHA)COS(BETA)dGAMMA/dt}
 * 
 * 
 *     Let U = -D*SIN(BETA)
 *         V =    COS(BETA)
 * 
 *     then
 * 
 *     W  = e_A{-dALPHA/dt                  -              U*dGAMMA/dt}
 *        + e_B{       -COS(ALPHA)*dBETA/dt - D*SIN(ALPHA)*V*dGAMMA/dt}
 *        + e_L{      D*SIN(ALPHA)dBETA/dt  -   COS(ALPHA)*V*dGAMMA/dt}
 * </pre>
 * <p>
 * As we can see from the above, by choosing appropriate assignments for U and V, the two cases can
 * be unified in a single expression.
 * </p>
 * <p>
 * Substituting CA and SA for COS(ALPHA) and SIN(ALPHA) and re-writing the last expression in matrix
 * form we have:
 * </p>
 * 
 * <pre>
 *                        [ -1     0      0 ][ 1  0  U ] [dALPHA/dt]
 *    W  = {e_A  e_B  e_L}|  0   -CA  -D*SA || 0  1  0 | |dBETA /dt|
 *                        [  0  D*SA    -CA ][ 0  0  V ] [dGAMMA/dt]
 * </pre>
 * <p>
 * If we let E_n stand for the transpose of e_n, then solving for the derivative vector we have:
 * </p>
 * 
 * <pre>
 *   [dALPHA/dt]   [ 1 0 -U/V ] [ -1     0     0] [ E_A ]
 *   |dBETA /dt| = | 0 1   0  | |  0   -CA  D*SA| | E_B | W
 *   [dGAMMA/dt]   [ 0 0  1/V ] [  0 -D*SA   -CA] [ E_L ]
 * </pre>
 * <p>
 * But since the matrix product E_n W is <e_n,W> = W(n) this can be rewritten as
 * </p>
 * 
 * <pre>
 *   [dALPHA/dt]   [ -1  U*D*SA/V  U*CA/V ] [ W(A) ]
 *   |dBETA /dt| = |  0   -CA      D*SA   | [ W(B) |
 *   [dGAMMA/dt]   [  0   -D*SA/V   -CA/V ] [ W(L) ]
 * </pre>
 * <p>
 * Thus we see that there is a relatively elementary computation required to determine the
 * derivatives of the three Euler angles captured in {@link EulerAngles}.
 * </p>
 */
public abstract class DifferentiatedEulerAngles implements DifferentiatedRotation {

  private final EulerAngles angles;

  private double leftAngleDerivative;
  private double centerAngleDerivative;
  private double rightAngleDerivative;

  /**
   * This constructor is marked private to prevent external classes that might opt to subclass it
   * from properly instantiating the parent.
   * 
   * @param angles the instance of EulerAngles to retain a reference to internally
   * @param leftAngleDerivative the derivative of the left rotation angle, radians per unit
   * @param centerAngleDerivative the derivative of the center rotation angle, radians per unit
   * @param rightAngleDerivative the derivative of the right angle, radians per unit
   */
  private DifferentiatedEulerAngles(EulerAngles angles, double leftAngleDerivative,
      double centerAngleDerivative, double rightAngleDerivative) {
    this.angles = angles;
    this.leftAngleDerivative = leftAngleDerivative;
    this.centerAngleDerivative = centerAngleDerivative;
    this.rightAngleDerivative = rightAngleDerivative;
  }

  /**
   * Creates a differentiated Euler angle instance of the appropriate type.
   * 
   * @param left the left axis (last to be applied)
   * @param center the center axis
   * @param right the right axis (first to be applied)
   * @param leftAngle the left rotation angle
   * @param centerAngle the center rotation angle
   * @param rightAngle the right rotation angle
   * @param leftAngleDerivative the left rotation angle derivative
   * @param centerAngleDerivative the center rotation angle derivative
   * @param rightAngleDerivative the right rotation angle derivative
   * 
   * @return a newly created instance of the differentiated angles of the axis triplet requested sub
   *         type.
   * 
   * @throws IllegalArgumentException if left equals center, or right equals center
   */
  public static DifferentiatedEulerAngles create(Axis left, Axis center, Axis right,
      double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
      double centerAngleDerivative, double rightAngleDerivative) {

    checkArgument(!left.equals(center) && !right.equals(center),
        "Center axis can not equal left or right.");

    /*
     * This is hideous, but it works well enough for the purposes of this method's implementation.
     */
    if (left.equals(Axis.I) && center.equals(Axis.J) && right.equals(Axis.I)) {
      return new DifferentiatedEulerAngles.IJI(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.I) && center.equals(Axis.K) && right.equals(Axis.I)) {
      return new DifferentiatedEulerAngles.IKI(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.J) && center.equals(Axis.I) && right.equals(Axis.J)) {
      return new DifferentiatedEulerAngles.JIJ(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.J) && center.equals(Axis.K) && right.equals(Axis.J)) {
      return new DifferentiatedEulerAngles.JKJ(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.K) && center.equals(Axis.I) && right.equals(Axis.K)) {
      return new DifferentiatedEulerAngles.KIK(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.K) && center.equals(Axis.J) && right.equals(Axis.K)) {
      return new DifferentiatedEulerAngles.KJK(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.I) && center.equals(Axis.J) && right.equals(Axis.K)) {
      return new DifferentiatedEulerAngles.IJK(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.I) && center.equals(Axis.K) && right.equals(Axis.J)) {
      return new DifferentiatedEulerAngles.IKJ(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.J) && center.equals(Axis.I) && right.equals(Axis.K)) {
      return new DifferentiatedEulerAngles.JIK(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.J) && center.equals(Axis.K) && right.equals(Axis.I)) {
      return new DifferentiatedEulerAngles.JKI(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.K) && center.equals(Axis.I) && right.equals(Axis.J)) {
      return new DifferentiatedEulerAngles.KIJ(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    if (left.equals(Axis.K) && center.equals(Axis.J) && right.equals(Axis.I)) {
      return new DifferentiatedEulerAngles.KJI(leftAngle, centerAngle, rightAngle,
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    throw new BugException("If the code reaches here then something has gone horribly wrong.");

  }

  /**
   * Creates a new differentiated Euler angles instance using the angles to specify the appropriate
   * decomposition sub type and angles while augmenting them with the specified derivatives
   * 
   * @param angles the angles to copy
   * @param leftAngleDerivative the derivative of the left angle
   * @param centerAngleDerivative the derivative of the center angle
   * @param rightAngleDerivative the derivative of the right angle
   * 
   * @return a newly created instance of the sub type of angles.
   */
  public static DifferentiatedEulerAngles create(EulerAngles angles, double leftAngleDerivative,
      double centerAngleDerivative, double rightAngleDerivative) {
    return create(angles.getLeftAxis(), angles.getCenterAxis(), angles.getRightAxis(),
        angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(), leftAngleDerivative,
        centerAngleDerivative, rightAngleDerivative);
  }

  /**
   * Creates a copy of the supplied instance.
   * <p>
   * This method creates a new instance of the differentiated Euler angle using the specific subtype
   * specified by the retrieval of the three axes.
   * </p>
   * 
   * @param angles the value to copy
   * 
   * @return a newly created copy of the supplied instance
   */
  public static DifferentiatedEulerAngles copyOf(DifferentiatedEulerAngles angles) {
    return create(angles.getLeftAxis(), angles.getCenterAxis(), angles.getRightAxis(),
        angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
        angles.getLeftAngleDerivative(), angles.getCenterAngleDerivative(),
        angles.getRightAngleDerivative());
  }

  /**
   * Retrieves the angle associated with the left component rotation.
   * 
   * @return an angle in radians
   */
  public double getLeftAngle() {
    return angles.getLeftAngle();
  }

  /**
   * Sets the angle associated with the left component rotation.
   * <p>
   * The value supplied is canonicalized against the appropriate range of values for this
   * decomposition.
   * </p>
   * 
   * @param leftAngle an angle in radians
   */
  public void setLeftAngle(double leftAngle) {
    angles.setLeftAngle(leftAngle);
  }

  /**
   * Retrieves the angle associated with the center component rotation.
   * 
   * @return an angle in radians
   */
  public double getCenterAngle() {
    return angles.getCenterAngle();
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
    angles.setCenterAngle(centerAngle);
  }

  /**
   * Retrieves the angle associated with the right component rotation.
   * 
   * @return an angle in radians
   */
  public double getRightAngle() {
    return angles.getRightAngle();
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
    angles.setRightAngle(rightAngle);
  }

  /**
   * Retrieves the axis associated with the left component rotation.
   * 
   * @return the axis of interest
   */
  public Axis getLeftAxis() {
    return angles.getLeftAxis();
  }

  /**
   * Retrieves the axis associated with the center component rotation.
   * 
   * @return the axis of interest
   */
  public Axis getCenterAxis() {
    return angles.getCenterAxis();
  }

  /**
   * Retrieves the axis associated with the right component rotation.
   * 
   * @return the axis of interest
   */
  public Axis getRightAxis() {
    return angles.getRightAxis();
  }

  /**
   * Retrieves the derivative of the left component rotation angle.
   * 
   * @return a derivative in radians per unit with which the derivative is respect to
   */
  public double getLeftAngleDerivative() {
    return leftAngleDerivative;
  }

  /**
   * Sets the derivative of the left component rotation angle.
   * 
   * @param leftAngleDerivative a derivative in radians per unit which which the derivative is
   *        respect to
   */
  public void setLeftAngleDerivative(double leftAngleDerivative) {
    this.leftAngleDerivative = leftAngleDerivative;
  }

  /**
   * Retrieves the derivative of the center component rotation angle.
   * 
   * @return a derivative in radians per unit with which the derivative is respect to
   */
  public double getCenterAngleDerivative() {
    return centerAngleDerivative;
  }

  /**
   * Sets the derivative of the center component rotation angle.
   * 
   * @param centerAngleDerivative a derivative in radians per unit which which the derivative is
   *        respect to
   */
  public void setCenterAngleDerivative(double centerAngleDerivative) {
    this.centerAngleDerivative = centerAngleDerivative;
  }

  /**
   * Retrieves the derivative of the right component rotation angle.
   * 
   * @return a derivative in radians per unit with which the derivative is respect to
   */
  public double getRightAngleDerivative() {
    return rightAngleDerivative;
  }

  /**
   * Sets the derivative of the right component rotation angle.
   * 
   * @param rightAngleDerivative a derivative in radians per unit which which the derivative is
   *        respect to
   */
  public void setRightAngleDerivative(double rightAngleDerivative) {
    this.rightAngleDerivative = rightAngleDerivative;
  }

  /**
   * Set all angles and their associated derivatives simultaneously.
   * 
   * @param leftAngle the left component rotation angle, in radians
   * @param centerAngle the center component rotation angle, in radians
   * @param rightAngle the right component rotation angle, in radians
   * @param leftAngleDerivative the left component rotation angle derivative in radians per
   *        differentiated unit
   * @param centerAngleDerivative the center component rotation angle derivative in radians per
   *        differentiated unit
   * @param rightAngleDerivative the right component rotation angle derivative in radians per
   *        differentiated unit
   */
  public void set(double leftAngle, double centerAngle, double rightAngle,
      double leftAngleDerivative, double centerAngleDerivative, double rightAngleDerivative) {
    angles.set(leftAngle, centerAngle, rightAngle);
    this.leftAngleDerivative = leftAngleDerivative;
    this.centerAngleDerivative = centerAngleDerivative;
    this.rightAngleDerivative = rightAngleDerivative;
  }

  /**
   * Computes the U parameter utilized in both the conversion to and from state transforms as
   * captured in NAIF's algorithm in XF2EUL and EUL2XF.
   * 
   * @return the U parameter
   */
  abstract double computeU();

  /**
   * Computes the V parameter utilized in both the conversion to and from state transforms as
   * captured in NAIF's algorithm in XF2EUL and EUL2XF.
   * 
   * @return the V parameter
   */
  abstract double computeV();

  @Override
  public EulerAngles getRotation() {
    return angles;
  }

  @Override
  public StateTransform getTransform(StateTransform buffer) {

    /*
     * First determine the triplet of axis and their orderings.
     */
    Axis completingAxis = angles.getLeftAxis().getCompletingAxis(angles.getCenterAxis());
    double delta = angles.getLeftAxis().getCrossSign(angles.getCenterAxis());

    double cosLeft = cos(angles.getLeftAngle());
    double sinLeft = sin(angles.getLeftAngle());

    double u = computeU();
    double v = computeV();

    /*
     * Install the proper components into solution:
     * 
     * SOLUTN(1,1) = -D SOLUTN(2,1) = 0.0D0 SOLUTN(3,1) = 0.0D0
     * 
     * SOLUTN(1,2) = 0.0D0 SOLUTN(2,2) = -D*CA SOLUTN(3,2) = SA
     * 
     * SOLUTN(1,3) = -D*U SOLUTN(2,3) = -SA*V SOLUTN(3,3) = -D*CA*V
     */
    MatrixIJK solution = new MatrixIJK();
    solution.setTo(-delta, 0.0, 0.0, 0.0, -delta * cosLeft, sinLeft, -delta * u, -sinLeft * v,
        -delta * cosLeft * v);

    VectorIJK vector = new VectorIJK();
    vector.setTo(getLeftAngleDerivative(), getCenterAngleDerivative(), getRightAngleDerivative());

    solution.mxv(vector, vector);

    /*
     * Now configure drdtrt.
     * 
     * DRDTRT(L,B) = DOMEGA(1) DRDTRT(B,L) = -DOMEGA(1)
     * 
     * DRDTRT(A,L) = DOMEGA(2) DRDTRT(L,A) = -DOMEGA(2)
     * 
     * DRDTRT(B,A) = DOMEGA(3) DRDTRT(A,B) = -DOMEGA(3)
     * 
     * DRDTRT(1,1) = 0.0D0 DRDTRT(2,2) = 0.0D0 DRDTRT(3,3) = 0.0D0
     */
    MatrixIJK drdtrt = new MatrixIJK();
    drdtrt.set(completingAxis.getAxisIndex(), angles.getCenterAxis().getAxisIndex(), vector.getI());
    drdtrt.set(angles.getCenterAxis().getAxisIndex(), completingAxis.getAxisIndex(),
        -vector.getI());
    drdtrt.set(angles.getLeftAxis().getAxisIndex(), completingAxis.getAxisIndex(), vector.getJ());
    drdtrt.set(completingAxis.getAxisIndex(), angles.getLeftAxis().getAxisIndex(), -vector.getJ());
    drdtrt.set(angles.getCenterAxis().getAxisIndex(), angles.getLeftAxis().getAxisIndex(),
        vector.getK());
    drdtrt.set(angles.getLeftAxis().getAxisIndex(), angles.getCenterAxis().getAxisIndex(),
        -vector.getK());

    drdtrt.set(0, 0, 0);
    drdtrt.set(1, 1, 0);
    drdtrt.set(2, 2, 0);

    RotationMatrixIJK rotation = new RotationMatrixIJK();
    angles.getRotation(rotation);

    MatrixIJK.mxm(drdtrt, rotation, drdtrt);

    buffer.getRotation().setTo(rotation);
    buffer.getRotationDerivative().setTo(drdtrt);

    return buffer;
  }

  @Override
  public DifferentiatedEulerAngles setTo(UnwritableStateTransform transform) {

    /*
     * Populate the internal Euler angles first.
     */
    angles.setTo(transform.getRotation());

    Axis completingAxis = angles.getLeftAxis().getCompletingAxis(angles.getCenterAxis());

    double delta = angles.getLeftAxis().getCrossSign(angles.getCenterAxis());

    MatrixIJK drdtrt = MatrixIJK.mxmt(transform.getRotationDerivative(), transform.getRotation());

    double cosLeft = cos(angles.getLeftAngle());
    double sinLeft = sin(angles.getLeftAngle());

    double u = computeU();
    double v = computeV();

    /*
     * OMEGA(1) = D*DRDTRT(L,B) OMEGA(2) = D*DRDTRT(A,L) OMEGA(3) = D*DRDTRT(B,A)
     */
    VectorIJK vector = new VectorIJK();
    vector.setI(
        delta * drdtrt.get(completingAxis.getAxisIndex(), angles.getCenterAxis().getAxisIndex()));
    vector.setJ(
        delta * drdtrt.get(angles.getLeftAxis().getAxisIndex(), completingAxis.getAxisIndex()));
    vector.setK(delta
        * drdtrt.get(angles.getCenterAxis().getAxisIndex(), angles.getLeftAxis().getAxisIndex()));

    /*
     * To avoid floating point overflows, examine u. If it is 1, then set v to zero.
     */
    if (abs(u) == 1.0) {
      v = 0.0;
    }

    /*
     * Handle the singular, non-unique case first.
     */
    if (v == 0.0) {
      leftAngleDerivative = 0.0;
      rightAngleDerivative = -u * vector.getI();

      if (abs(cosLeft) > abs(sinLeft)) {
        centerAngleDerivative = -vector.getJ() / cosLeft;
      } else {
        centerAngleDerivative = delta * vector.getK() / sinLeft;
      }

      return this;
    }

    /*
     * SOLUTN(1,1) = -1.0D0 SOLUTN(2,1) = 0.0D0 SOLUTN(3,1) = 0.0D0
     * 
     * SOLUTN(1,2) = U*D*SA/V SOLUTN(2,2) = -CA SOLUTN(3,2) = -D*SA/V
     * 
     * SOLUTN(1,3) = U*CA/V SOLUTN(2,3) = D*SA SOLUTN(3,3) = -CA/V
     * 
     * CALL MXV ( SOLUTN, OMEGA, EULANG(4) )
     */
    MatrixIJK solution = new MatrixIJK();
    solution.setTo(-1.0, 0.0, 0.0, u * delta * sinLeft / v, -cosLeft, -delta * sinLeft / v,
        u * cosLeft / v, delta * sinLeft, -cosLeft / v);
    solution.mxv(vector, vector);

    leftAngleDerivative = vector.getI();
    centerAngleDerivative = vector.getJ();
    rightAngleDerivative = vector.getK();

    return this;
  }

  public DifferentiatedEulerAngles canonicalize() {
    angles.canonicalize();
    return this;
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
    result = prime * result + ((angles == null) ? 0 : angles.hashCode());
    long temp;
    temp = Double.doubleToLongBits(centerAngleDerivative);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(leftAngleDerivative);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(rightAngleDerivative);
    result = prime * result + (int) (temp ^ (temp >>> 32));
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
    DifferentiatedEulerAngles other = (DifferentiatedEulerAngles) obj;
    if (angles == null) {
      if (other.angles != null) {
        return false;
      }
    } else if (!angles.equals(other.angles)) {
      return false;
    }
    if (Double.doubleToLongBits(centerAngleDerivative) != Double
        .doubleToLongBits(other.centerAngleDerivative)) {
      return false;
    }
    if (Double.doubleToLongBits(leftAngleDerivative) != Double
        .doubleToLongBits(other.leftAngleDerivative)) {
      return false;
    }
    if (Double.doubleToLongBits(rightAngleDerivative) != Double
        .doubleToLongBits(other.rightAngleDerivative)) {
      return false;
    }
    return true;
  }

  /**
   * {@inheritDoc}
   * 
   * The format of the string, subject to change, is at the moment:
   * 
   * <pre>
   * (LCR)[left, center, right; dLeft, dCenter, dRight]
   * </pre>
   * 
   * where L,C,R are the single letter axis identifiers; left, center, right are the angles
   * expressed in radians; and dLeft, dCenter, and dRight are the derivatives expressed in radians
   * per unit of differentation
   */
  @Override
  public String toString() {
    return "(" + angles.getLeftAxis() + angles.getCenterAxis() + angles.getRightAxis() + ")["
        + String.valueOf(angles.getLeftAngle()) + ", " + String.valueOf(angles.getCenterAngle())
        + ", " + String.valueOf(angles.getRightAngle()) + "; " + String.valueOf(leftAngleDerivative)
        + ", " + String.valueOf(centerAngleDerivative) + ", " + String.valueOf(rightAngleDerivative)
        + "]";
  }

  /**
   * Private class used to aid in the implementation of the ABA style Euler decompositions with
   * derivatives. It is private because it is purely an implementation detail used to consolidate
   * code.
   */
  private abstract static class DifferentiatedEulerAnglesABA extends DifferentiatedEulerAngles {

    private DifferentiatedEulerAnglesABA(EulerAnglesABA angles, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(angles, leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    @Override
    double computeU() {
      return cos(getCenterAngle());
    }

    @Override
    double computeV() {
      return getLeftAxis().getCrossSign(getCenterAxis()) * sin(getCenterAngle());
    }

  }

  /**
   * The I-J-I Euler decomposition with derivatives.
   */
  public static final class IJI extends DifferentiatedEulerAnglesABA {

    public IJI() {
      super(new EulerAngles.IJI(), 0, 0, 0);
    }

    public IJI(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.IJI(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public IJI(EulerAngles.IJI angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public IJI(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public IJI(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.IJI getRotation() {
      return (EulerAngles.IJI) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.IJI setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.IJI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The I-K-I Euler decomposition with derivatives.
   */
  public static final class IKI extends DifferentiatedEulerAnglesABA {

    public IKI() {
      super(new EulerAngles.IKI(), 0, 0, 0);
    }

    public IKI(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.IKI(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public IKI(EulerAngles.IKI angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public IKI(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public IKI(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.IKI getRotation() {
      return (EulerAngles.IKI) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.IKI setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.IKI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-I-J Euler decomposition with derivatives.
   */
  public static final class JIJ extends DifferentiatedEulerAnglesABA {

    public JIJ() {
      super(new EulerAngles.JIJ(), 0, 0, 0);
    }

    public JIJ(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.JIJ(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public JIJ(EulerAngles.JIJ angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public JIJ(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public JIJ(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.JIJ getRotation() {
      return (EulerAngles.JIJ) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.JIJ setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.JIJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-K-J Euler decomposition with derivatives.
   */
  public static final class JKJ extends DifferentiatedEulerAnglesABA {

    public JKJ() {
      super(new EulerAngles.JKJ(), 0, 0, 0);
    }

    public JKJ(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.JKJ(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public JKJ(EulerAngles.JKJ angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public JKJ(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public JKJ(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.JKJ getRotation() {
      return (EulerAngles.JKJ) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.JKJ setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.JKJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-I-K Euler decomposition with derivatives.
   */
  public static final class KIK extends DifferentiatedEulerAnglesABA {

    public KIK() {
      super(new EulerAngles.KIK(), 0, 0, 0);
    }

    public KIK(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.KIK(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public KIK(EulerAngles.KIK angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public KIK(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public KIK(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.KIK getRotation() {
      return (EulerAngles.KIK) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.KIK setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.KIK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-J-K Euler decomposition with derivatives.
   */
  public static final class KJK extends DifferentiatedEulerAnglesABA {

    public KJK() {
      super(new EulerAngles.KJK(), 0, 0, 0);
    }

    public KJK(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.KJK(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public KJK(EulerAngles.KJK angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public KJK(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public KJK(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.KJK getRotation() {
      return (EulerAngles.KJK) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.KJK setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.KJK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * Private class used to aid in the implementation of the ABC style Euler decompositions with
   * derivatives. It is private because it is purely an implementation detail used to consolidate
   * code.
   */
  private abstract static class DifferentiatedEulerAnglesABC extends DifferentiatedEulerAngles {

    private DifferentiatedEulerAnglesABC(EulerAnglesABC angles, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(angles, leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    @Override
    double computeU() {
      return -getLeftAxis().getCrossSign(getCenterAxis()) * sin(getCenterAngle());
    }

    @Override
    double computeV() {
      return cos(getCenterAngle());
    }

  }

  /**
   * The I-J-K Euler decomposition with derivatives.
   */
  public static final class IJK extends DifferentiatedEulerAnglesABC {

    public IJK() {
      super(new EulerAngles.IJK(), 0, 0, 0);
    }

    public IJK(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.IJK(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public IJK(EulerAngles.IJK angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public IJK(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public IJK(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.IJK getRotation() {
      return (EulerAngles.IJK) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.IJK setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.IJK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The I-K-J Euler decomposition with derivatives.
   */
  public static final class IKJ extends DifferentiatedEulerAnglesABC {

    public IKJ() {
      super(new EulerAngles.IKJ(), 0, 0, 0);
    }

    public IKJ(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.IKJ(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public IKJ(EulerAngles.IKJ angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public IKJ(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public IKJ(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.IKJ getRotation() {
      return (EulerAngles.IKJ) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.IKJ setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.IKJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-K-I Euler decomposition with derivatives.
   */
  public static final class JKI extends DifferentiatedEulerAnglesABC {

    public JKI() {
      super(new EulerAngles.JKI(), 0, 0, 0);
    }

    public JKI(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.JKI(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public JKI(EulerAngles.JKI angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public JKI(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public JKI(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.JKI getRotation() {
      return (EulerAngles.JKI) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.JKI setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.JKI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The J-I-K Euler decomposition with derivatives.
   */
  public static final class JIK extends DifferentiatedEulerAnglesABC {

    public JIK() {
      super(new EulerAngles.JIK(), 0, 0, 0);
    }

    public JIK(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.JIK(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public JIK(EulerAngles.JIK angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public JIK(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public JIK(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.JIK getRotation() {
      return (EulerAngles.JIK) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.JIK setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.JIK canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-I-J Euler decomposition with derivatives.
   */
  public static final class KIJ extends DifferentiatedEulerAnglesABC {

    public KIJ() {
      super(new EulerAngles.KIJ(), 0, 0, 0);
    }

    public KIJ(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.KIJ(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public KIJ(EulerAngles.KIJ angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public KIJ(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public KIJ(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.KIJ getRotation() {
      return (EulerAngles.KIJ) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.KIJ setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.KIJ canonicalize() {
      super.canonicalize();
      return this;
    }

  }

  /**
   * The K-J-I Euler decomposition with derivatives.
   */
  public static final class KJI extends DifferentiatedEulerAnglesABC {

    public KJI() {
      super(new EulerAngles.KJI(), 0, 0, 0);
    }

    public KJI(double leftAngle, double centerAngle, double rightAngle, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      super(new EulerAngles.KJI(leftAngle, centerAngle, rightAngle), leftAngleDerivative,
          centerAngleDerivative, rightAngleDerivative);
    }

    public KJI(EulerAngles.KJI angles, double leftAngleDerivative, double centerAngleDerivative,
        double rightAngleDerivative) {
      this(angles.getLeftAngle(), angles.getCenterAngle(), angles.getRightAngle(),
          leftAngleDerivative, centerAngleDerivative, rightAngleDerivative);
    }

    public KJI(UnwritableStateTransform transform) {
      this();
      setTo(transform);
    }

    public KJI(UnwritableRotationMatrixIJK rotation, double leftAngleDerivative,
        double centerAngleDerivative, double rightAngleDerivative) {
      this();
      getRotation().setTo(rotation);
      setLeftAngleDerivative(leftAngleDerivative);
      setCenterAngleDerivative(centerAngleDerivative);
      setRightAngleDerivative(rightAngleDerivative);

    }

    @Override
    public EulerAngles.KJI getRotation() {
      return (EulerAngles.KJI) super.getRotation();
    }

    @Override
    public DifferentiatedEulerAngles.KJI setTo(UnwritableStateTransform transform) {
      super.setTo(transform);
      return this;
    }

    @Override
    public DifferentiatedEulerAngles.KJI canonicalize() {
      super.canonicalize();
      return this;
    }

  }

}
