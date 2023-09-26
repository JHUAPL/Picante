package picante.mechanics.rotations;

import static picante.math.PicanteMath.abs;
import static picante.math.PicanteMath.acos;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.sin;
import static picante.math.PicanteMath.sqrt;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Implementation of a rotational quaternion and the corresponding arithmetic algorithms. Note: this
 * is not a general implementation of a quaternion, this class is solely focused on quaternions of
 * unit length used to capture rotations.
 * <p>
 * The quaternions constructed and presented by this class encapsulate rotations. As such, they are
 * necessarily of length unity. The constructors normalize input to preserve this property to the
 * best precision available in the Java native double type. Further, this class is writable for
 * performance considerations. In an ideal world, it would be unwritable and each method resulting
 * in a quaternion would be static and construct a new quaternion to encapsulate the output.
 * </p>
 * <p>
 * The following text is taken from NAIF's QXQ routine: (Note: this class implements the SPICE
 * quaternion.)
 * </p>
 * <p>
 * There are (at least) two popular "styles" of quaternions; these differ in the layout of the
 * quaternion elements, the definition of the multiplication operation, and the mapping between the
 * set of unit quaternions and corresponding rotation matrices.
 * </p>
 * <p>
 * SPICE-style quaternions have the scalar part in the first component and the vector part in the
 * subsequent components. The SPICE convention, along with the multiplication rules for SPICE
 * quaternions, are those used by William Rowan Hamilton, the inventor of quaternions.
 * </p>
 * <p>
 * Another common quaternion style places the scalar component last. This style is often used in
 * engineering applications.
 * </p>
 * <p>
 * The correspondence between SPICE quaternions and rotation matrices is defined as follows: Let R
 * be a rotation matrix that transforms vectors from a right-handed, orthogonal reference frame F1
 * to a second right-handed, orthogonal reference frame F2. If a vector V has components x, y, z in
 * the frame F1, then V has components x', y', z' in the frame F2, and R satisfies the relation:
 * </p>
 * <p>
 * 
 * <pre>
 *    [ x' ]     [       ] [ x ]
 *    | y' |  =  |   R   | | y |
 *    [ z' ]     [       ] [ z ]
 * </pre>
 * 
 * </p>
 * <p>
 * Letting Q = (q0, q1, q2, q3) be the SPICE unit quaternion representing R, we have the relation:
 * </p>
 * <p>
 * 
 * <pre>
 *        +-                                                          -+
 *        |           2    2                                           |
 *        | 1 - 2 ( q2 + q3 )    2 (q1 q2 - q0 q3)   2 (q1 q3 + q0 q2) |
 *        |                                                            |
 *        |                                                            |
 *        |                                2    2                      |
 *    R = | 2 (q1 q2 + q0 q3)    1 - 2 ( q1 + q3 )   2 (q2 q3 - q0 q1) |
 *        |                                                            |
 *        |                                                            |
 *        |                                                    2    2  |
 *        | 2 (q1 q3 - q0 q2)    2 (q2 q3 + q0 q1)   1 - 2 ( q1 + q2 ) |
 *        |                                                            |
 *        +-                                                          -+
 * </pre>
 * 
 * </p>
 * <p>
 * To map the rotation matrix R to a unit quaternion, we start by decomposing the rotation matrix as
 * a sum of symmetric and skew-symmetric parts:
 * </p>
 * <p>
 * 
 * <pre>
 *                                    2
 *    R = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]
 *                 symmetric                   skew-symmetric
 * </pre>
 * 
 * </p>
 * <p>
 * OMEGA is a skew-symmetric matrix of the form:
 * </p>
 * <p>
 * 
 * <pre>
 *              +-             -+
 *              |  0   -n3   n2 |
 *              |               |
 *    OMEGA  =  |  n3   0   -n1 |
 *              |               |
 *              | -n2   n1   0  |
 *              +-             -+
 * </pre>
 * 
 * </p>
 * <p>
 * The vector N of matrix entries (n1, n2, n3) is the rotation axis of R and theta is R's rotation
 * angle. Note that N and theta are not unique.
 * </p>
 * <p>
 * Let
 * </p>
 * <p>
 * 
 * <pre>
 *    C = cos(theta/2)
 *    S = sin(theta/2)
 * </pre>
 * 
 * </p>
 * <p>
 * Then the unit quaternions Q corresponding to R are
 * </p>
 * <p>
 * 
 * <pre>
 *    Q = +/- ( C, S*n1, S*n2, S*n3 )
 * </pre>
 * 
 * </p>
 * <p>
 * The mappings between quaternions and the corresponding rotations are carried out by the methods
 * on this class:
 * </p>
 * <ul>
 * <li>{@link Quaternion#getRotation(RotationMatrixIJK)} {quaternion to matrix}</li>
 * <li>{@link Quaternion#setTo(UnwritableRotationMatrixIJK)} {matrix to quaternion}</li>
 * </ul>
 * <p>
 * {@link Quaternion#setTo(UnwritableRotationMatrixIJK)} always returns a quaternion with scalar
 * part greater than or equal to zero.
 * </p>
 */
public class Quaternion implements Rotation {

  /**
   * Scalar component of the quaternion.
   */
  private double q0;

  /**
   * Vector components of the quaternion.
   */
  private double q1, q2, q3;

  /**
   * Multiply two quaternions and store the result in a buffer.
   * <p>
   * 
   * <pre>
   * Quaternion r = Quaternion.multiply(a, b, new Quaternion());
   * </pre>
   * 
   * </p>
   * <p>
   * has the effect:
   * </p>
   * <p>
   * 
   * <pre>
   * r = a * b
   * </pre>
   * 
   * </p>
   * <p>
   * where a and b are represented as the sums of scalar (real) and vector (imaginary) parts as
   * follows:
   * </p>
   * <p>
   * 
   * <pre>
   * a = as + av
   * b = bs + bv
   * </pre>
   * 
   * </p>
   * <p>
   * then the resultant output quaternion stored in q is:
   * </p>
   * <p>
   * 
   * <pre>
   * r = os + ov
   * </pre>
   * 
   * </p>
   * <p>
   * where:
   * </p>
   * <p>
   * 
   * <pre>
   *    os = as * bs + &lt;av, bv&gt;
   *    ov = as * bv + bs * av + av x bv
   * </pre>
   * 
   * </p>
   * <p>
   * and the notation <,> denotes the inner product operator and x denotes the cross product
   * operator.
   * </p>
   * 
   * @param a the quaternion on the left of the multiplication operator
   * @param b the quaternion on the right of the multiplication operator
   * @param buffer a buffer to capture the results of the multiplication. Note: buffer may be a
   *        reference to either a or b.
   * 
   * @return a reference to buffer for convenience.
   */
  public static Quaternion multiply(Quaternion a, Quaternion b, Quaternion buffer) {

    double t0, t1, t2, t3;

    /*
     * NAIF's quaternion implementation requires multiplication to be carried out in the following
     * manner:
     * 
     * q = ab
     * 
     * q0 = a0*b0 - <aV,bV> qV = a0*bV + b0*aV + (aV x bV)
     * 
     * where q0, a0, and b0 are the scalar components of q, a, and b respectively; and qV, aV, and
     * bV are the vector components of q, a, and b respectively.
     */
    t0 = a.q0 * b.q0 - a.q1 * b.q1 - a.q2 * b.q2 - a.q3 * b.q3;
    t1 = a.q0 * b.q1 + b.q0 * a.q1 + a.q2 * b.q3 - a.q3 * b.q2;
    t2 = a.q0 * b.q2 + b.q0 * a.q2 - a.q1 * b.q3 + a.q3 * b.q1;
    t3 = a.q0 * b.q3 + b.q0 * a.q3 + a.q1 * b.q2 - a.q2 * b.q1;

    /*
     * Normalize the resultant output quaternion.
     */
    double scale = computeNorm(t0, t1, t2, t3);

    if (scale == 0.0) {
      buffer.q0 = 0.0;
      buffer.q1 = 0.0;
      buffer.q2 = 0.0;
      buffer.q3 = 0.0;
    }

    buffer.q0 = t0 / scale;
    buffer.q1 = t1 / scale;
    buffer.q2 = t2 / scale;
    buffer.q3 = t3 / scale;

    return buffer;
  }

  /**
   * Multiply two quaternions together and store the results in a newly created quaternion.
   * 
   * @param a the quaternion on the left of the multiplication operator
   * 
   * @param b the quaternion on the right of the multiplication operator
   * 
   * @return a newly created quaternion containing the result of a*b.
   */
  public static Quaternion multiply(Quaternion a, Quaternion b) {
    return multiply(a, b, new Quaternion());
  }

  /**
   * Computes the norm of four quaternion components in an overflow safe way.
   * 
   * @param q0 the scalar component
   * @param q1 the ith component
   * @param q2 the jth component
   * @param q3 the kth component
   * 
   * @return the length of the quaternion [q0,q1,q2,q3]
   */
  private static double computeNorm(double q0, double q1, double q2, double q3) {

    double max = max(abs(q0), max(abs(q1), max(abs(q2), abs(q3))));

    if (max == 0.0) {
      return 0.0;
    }

    q0 /= max;
    q1 /= max;
    q2 /= max;
    q3 /= max;

    return max * sqrt(q0 * q0 + q1 * q1 + q2 * q2 + q3 * q3);
  }

  /**
   * Construct an identity quaternion.
   */
  public Quaternion() {
    q0 = 1.0;
    q1 = 0.0;
    q2 = 0.0;
    q3 = 0.0;
  }

  /**
   * Construct a quaternion from a scalar and 3 vector components.
   * <p>
   * Note: this constructor normalizes the input components to enforce the unity restriction.
   * </p>
   * 
   * @param scalar the scalar component
   * @param vector1 the ith vector component
   * @param vector2 the jth vector component
   * @param vector3 the kth vector component
   */
  public Quaternion(double scalar, double vector1, double vector2, double vector3) {
    this.setTo(scalar, vector1, vector2, vector3);
  }

  /**
   * Construct a quaternion from a rotation axis and angle specified in radians.
   * <p>
   * Note: the rotation axis is normalized to unit length before creating the quaternion.
   * </p>
   * 
   * @param axis a vector containing the desired rotation axis.
   * @param angle an angle, specified in radians, that determines the magnitude of the rotation
   *        about axis.
   */
  public Quaternion(UnwritableVectorIJK axis, double angle) {
    this.setTo(axis, angle);
  }

  /**
   * Construct a copy of an existing quaternion.
   * 
   * @param q the quaternion to copy.
   */
  public Quaternion(Quaternion q) {
    q0 = q.q0;
    q1 = q.q1;
    q2 = q.q2;
    q3 = q.q3;
  }

  /**
   * Construct a quaternion from a rotation matrix.
   * <p>
   * Note: as of the current release of this class, this method does no checking to enforce that the
   * input matrix is in fact a rotation matrix.
   * </p>
   * <p>
   * If <code>matrix</code> rotates vectors by an angle of r radians about a unit vector A, where r
   * is in [0, pi], then if h = r/2,
   * </p>
   * <p>
   * 
   * <pre>
   *    Q = ( cos(h), sin(h)A ,  sin(h)A ,  sin(h)A ).
   *                         1          2          3
   * </pre>
   * 
   * </p>
   * <p>
   * The restriction that r must be in the range [0, pi] determines the output quaternion Q uniquely
   * except when r = pi; in this special case, both of the quaternions:
   * </p>
   * <p>
   * 
   * <pre>
   *    Q = ( 0,  A ,  A ,  A  )
   *               1    2    3
   * </pre>
   * 
   * </p>
   * <p>
   * and:
   * </p>
   * <p>
   * 
   * <pre>
   *    Q = ( 0, -A , -A , -A  )
   *               1    2    3
   * </pre>
   * 
   * </p>
   * <p>
   * are possible outputs, if A is a choice of rotation axis for matrix.
   * </p>
   * 
   * @param matrix the rotation matrix from which to construct the quaternion.
   */
  public Quaternion(UnwritableRotationMatrixIJK matrix) {
    this.setTo(matrix);
  }

  /**
   * Retrieve the scalar component of the quaternion.
   * 
   * @return the scalar component of the quaternion which is the cosine of half the rotation angle.
   */
  public double getScalar() {
    return q0;
  }

  /**
   * Retrieve the vector components of the quaternion.
   * 
   * @param buffer the vector memory to store the results. The vector components are simply the sine
   *        of half the rotation angle multiplied by the rotation axis.
   * 
   * @return a reference to the input vector for convenience.
   */
  public VectorIJK getVector(VectorIJK buffer) {
    buffer.setTo(q1, q2, q3);
    return buffer;
  }

  /**
   * Extract the rotation axis from the quaternion.
   * 
   * @param buffer is the vector used to store the resultant axis of rotation represented by the
   *        quaternion. This axis is the axis of the smaller angle rotation, namely assuming that
   *        the angle of the rotation is bounded to [0, Pi]. Note: if the quaternion is the identity
   *        quaternion, this method simply selects the rotation axis to be (0, 0, 1). If the
   *        quaternion encodes a rotation of Pi radians, then both axis and -axis may be regarded as
   *        the rotational axis.
   * 
   * @return a reference to the input vector, axis for convenience.
   */
  public VectorIJK getRotationAxis(VectorIJK buffer) {
    /*
     * The vector components {q1, q2, q3} store the rotation axis, but have been scaled by the sine
     * of half the rotation angle. Just inject the vector components into a vector and normalize to
     * restore the unit length rotation axis.
     */
    buffer.setTo(q1, q2, q3);

    /*
     * Check to see if this is the identity quaternion, i.e. all of the vector components are the
     * zero vector.
     */
    if (buffer.getLength() == 0.0) {
      buffer.setTo(0.0, 0.0, 1.0);
    } else {
      buffer.unitize();
    }

    /*
     * Lastly, check the sign of q0. If it is negative, then we have an axis and a rotation of
     * greater than Pi radians, flip the axis.
     */
    if (q0 < 0) {
      buffer.negate();
    }

    return buffer;
  }

  /**
   * Retrieve the rotation angle from the quaternion.
   * 
   * @return the rotation angle, specified in radians. The angle returned is bounded between [0,
   *         Pi].
   */
  public double getRotationAngle() {
    /*
     * The leading component of the quaternion is the cosine of half the rotation angle. However, we
     * need to account for the fact that a quaternion and it's negative encode the same rotation.
     * Return the angle between [0,Pi].
     */
    if (q0 < 0) {
      return 2.0 * acos(-q0);
    } else {
      return 2.0 * acos(q0);
    }
  }

  /**
   * Conjugate a quaternion. The results are stored in the original instance.
   * <p>
   * Conjugation negates the vector components of the quaternion, effectively altering the sense of
   * the encapsulated rotation. In the rotation matrix world, it is the same as an inverse or
   * transpose.
   * </p>
   * 
   * @return a reference to the quaternion for convenience.
   */
  public Quaternion conjugate() {

    /*
     * Just negate the vector components.
     */
    q1 = -q1;
    q2 = -q2;
    q3 = -q3;

    return this;
  }

  /**
   * Negate the entire quaternion.
   * 
   * @return a reference to the instance for convenience
   */
  public Quaternion negate() {
    q0 = -q0;
    q1 = -q1;
    q2 = -q2;
    q3 = -q3;

    return this;
  }

  /**
   * Sets the contents of the quaternion to match the supplied quaternion components.
   * <p>
   * Note: the supplied components are normalized to ensure that the quaternion is of unit length.
   * </p>
   * 
   * @param q0 the scalar component
   * @param q1 the ith component
   * @param q2 the jth component
   * @param q3 the kth component
   * 
   * @return a reference to the instance for convenience
   */
  public Quaternion setTo(double q0, double q1, double q2, double q3) {

    /*
     * Normalize the inputs to force the quaternion to be as close as possible to unit length.
     */
    double factor = computeNorm(q0, q1, q2, q3);

    /*
     * Handle the zero quaternion case.
     */
    if (factor == 0.0) {
      factor = 1.0;
    }

    this.q0 = q0 / factor;
    this.q1 = q1 / factor;
    this.q2 = q2 / factor;
    this.q3 = q3 / factor;

    return this;
  }

  /**
   * Copy the contents of one quaternion to another.
   * 
   * @param q the quaternion to copy.
   * 
   * @return a reference to the copied quaternion for convenience.
   */
  public Quaternion setTo(Quaternion q) {
    q0 = q.q0;
    q1 = q.q1;
    q2 = q.q2;
    q3 = q.q3;
    return this;
  }

  /**
   * Construct a quaternion from a rotation axis and angle; store the resultant quaternion in an
   * existing one.
   * <p>
   * Note: the rotation axis is normalized to unit length before assigning the components to the
   * quaternion.
   * </p>
   * 
   * @param axis a vector containing the desired rotation axis.
   * @param angle an angle, specified in radians, that determines the magnitude of the rotation
   *        about axis.
   * 
   * @return a reference to the quaternion for convenience.
   */
  public Quaternion setTo(UnwritableVectorIJK axis, double angle) {

    double scale = sin(angle / 2.0) / axis.getLength();

    q0 = cos(angle / 2.0);
    q1 = scale * axis.getI();
    q2 = scale * axis.getJ();
    q3 = scale * axis.getK();

    return this;
  }

  /**
   * {@inheritDoc}
   * <p>
   * The mathematics behind this routine are described here for the convenience of the reader. The
   * details are largely unimportant, but it provides a framework for understanding the
   * implementation provided here. Note: this text is adapted almost directly from NAIF's M2Q
   * module.
   * </p>
   * <p>
   * If our quaternion is C, S1, S2, S3 (the S's being the imaginary part) and we let
   * 
   * <pre>
   *    CSi = C  * Si
   *    Sij = Si * Sj
   * </pre>
   * 
   * then the rotation matrix corresponding to our quaternion is:
   * 
   * <pre>
   *    R(1,1)      = 1.0D0 - 2*S22 - 2*S33
   *    R(2,1)      =         2*S12 + 2*CS3
   *    R(3,1)      =         2*S13 - 2*CS2
   *    
   *    R(1,2)      =         2*S12 - 2*CS3
   *    R(2,2)      = 1.0D0 - 2*S11 - 2*S33
   *    R(3,2)      =         2*S23 + 2*CS1
   *    R(1,3)      =         2*S13 + 2*CS2
   *    R(2,3)      =         2*S23 - 2*CS1
   *    R(3,3)      = 1.0D0 - 2*S11 - 2*S22
   * </pre>
   * 
   * </p>
   * <p>
   * From the above we can see that:
   * 
   * <pre>
   * TRACE = 3 - 4 * (S11 + S22 + S33)
   * </pre>
   * 
   * so that
   * 
   * <pre>
   *    1.0D0 + TRACE = 4 - 4*(S11 + S22 + S33)
   *                  = 4*(CC + S11 + S22 + S33)
   *                  - 4*(S11 + S22 + S33)
   *                  = 4*CC
   * </pre>
   * 
   * Thus up to sign
   * 
   * <pre>
   *    C = 0.5D0 * DSQRT( 1.0D0 + TRACE )
   * </pre>
   * 
   * </p>
   * <p>
   * But we also have
   * 
   * <pre>
   *    1.0D0 + TRACE - 2.0D0*R(i,i) = 4.0D0 - 4.0D0(Sii + Sjj + Skk)
   *                                 - 2.0D0 + 4.0D0(Sjj + Skk )
   *                                 = 2.0D0 - 4.0D0*Sii
   * </pre>
   * 
   * So that
   * 
   * <pre>
   *    1.0D0 - TRACE + 2.0D0*R(i,i) = 4.0D0*Sii
   * </pre>
   * 
   * and so up to sign
   * 
   * <pre>
   *    Si = 0.5D0*DSQRT( 1.0D0 - TRACE + 2.0D0*R(i,i) )
   * </pre>
   * 
   * in addition to this observation, we note that all of the product pairs can easily be computed
   * 
   * <pre>
   *     CS1 = (R(3,2) - R(2,3))/4.0D0
   *     CS2 = (R(1,3) - R(3,1))/4.0D0
   *     CS3 = (R(2,1) - R(1,2))/4.0D0
   *     S12 = (R(2,1) + R(1,2))/4.0D0
   *     S13 = (R(3,1) + R(1,3))/4.0D0
   *     S23 = (R(2,3) + R(3,2))/4.0D0
   * </pre>
   * 
   * </p>
   * <p>
   * But taking sums or differences of numbers that are nearly equal or nearly opposite results in a
   * loss of precision. As a result we should take some care in which terms to select when computing
   * C, S1, S2, S3. However, by simply starting with one of the large quantities cc, S11, S22, or
   * S33 we can make sure that we use the best of the 6 quantities above when computing the
   * remaining components of the quaternion.
   * </p>
   * 
   * @see Quaternion#Quaternion(UnwritableRotationMatrixIJK)
   */
  @Override
  public Quaternion setTo(UnwritableRotationMatrixIJK matrix) {

    double trace = matrix.getII() + matrix.getJJ() + matrix.getKK();
    double mtrace = 1.0 - trace;

    double cc4 = 1.0 + trace;
    double s114 = mtrace + 2.0 * matrix.getII();
    double s224 = mtrace + 2.0 * matrix.getJJ();
    double s334 = mtrace + 2.0 * matrix.getKK();

    double c, factor;
    double s1, s2, s3;

    /*
     * Note that if you simply add:
     * 
     * cc4 + s114 + s224 + s334
     * 
     * you get four. Thus at least one of the terms is greater than 1.
     */
    if (1.0 <= cc4) {
      c = sqrt(cc4 * 0.25);
      factor = 1.0 / (c * 4.0);

      s1 = (matrix.getKJ() - matrix.getJK()) * factor;
      s2 = (matrix.getIK() - matrix.getKI()) * factor;
      s3 = (matrix.getJI() - matrix.getIJ()) * factor;
    } else if (1.0 <= s114) {
      s1 = sqrt(s114 * 0.25);
      factor = 1.0 / (s1 * 4.0);

      c = (matrix.getKJ() - matrix.getJK()) * factor;
      s2 = (matrix.getIJ() + matrix.getJI()) * factor;
      s3 = (matrix.getIK() + matrix.getKI()) * factor;
    } else if (1.0 <= s224) {
      s2 = sqrt(s224 * 0.25);
      factor = 1.0 / (s2 * 4.0);

      c = (matrix.getIK() - matrix.getKI()) * factor;
      s1 = (matrix.getIJ() + matrix.getJI()) * factor;
      s3 = (matrix.getJK() + matrix.getKJ()) * factor;
    } else {
      s3 = sqrt(s334 * 0.25);
      factor = 1.0 / (s3 * 4.0);

      c = (matrix.getJI() - matrix.getIJ()) * factor;
      s1 = (matrix.getIK() + matrix.getKI()) * factor;
      s2 = (matrix.getJK() + matrix.getKJ()) * factor;
    }

    /*
     * If the magnitude of this quaternion is not one, polish it up a bit.
     */
    double l2 = c * c + s1 * s1 + s2 * s2 + s3 * s3;

    if (l2 != 1.0) {
      double polish = 1.0 / sqrt(l2);
      c = c * polish;
      s1 = s1 * polish;
      s2 = s2 * polish;
      s3 = s3 * polish;
    }

    /*
     * Return a quaternion with the scalar component that is positive. Other software in the
     * rotations package may rely on this. This does exactly what NAIF's M2Q routine does, but
     * should the boolean expression be: (c >= 0.0)? This results in -0.0 being generated if the
     * matrix would have a zero scalar component.
     */
    if (c > 0.0) {
      q0 = c;
      q1 = s1;
      q2 = s2;
      q3 = s3;
    } else {
      q0 = -c;
      q1 = -s1;
      q2 = -s2;
      q3 = -s3;
    }

    return this;
  }

  /**
   * {@inheritDoc}
   * 
   * <p>
   * If a quaternion, Q, satisfies the equality:
   * </p>
   * <p>
   * 
   * <pre>
   *    || Q ||   =  1
   * </pre>
   * 
   * </p>
   * <p>
   * or equivalently
   * </p>
   * <p>
   * 
   * <pre>
   *          2          2          2          2
   *      Q(0)   +   Q(1)   +   Q(2)   +   Q(3)   =  1,
   * </pre>
   * 
   * </p>
   * <p>
   * then we can always find a unit vector A and a scalar r such that:
   * </p>
   * <p>
   * 
   * <pre>
   *    Q = ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
   * </pre>
   * 
   * </p>
   * <p>
   * We can interpret A and r as the axis and rotation angle of a rotation in 3-space. If we
   * restrict r to the range [0, pi], then r and A are uniquely determined, except if r = pi. In
   * this special case, A and -A are both valid rotation axes.
   * </p>
   * <p>
   * Every rotation is represented by a unique orthogonal matrix; this routine returns that unique
   * rotation matrix corresponding to Q. Note: if the supplied quaternion is of zero length, the
   * identity matrix is returned.
   * </p>
   * <p>
   * The underlying mathematics employed by the implementation is spelled out in detail here for
   * reference, but in practice may be ignored. The text and the method are adapted almost directly
   * from NAIF's Q2M routine.
   * </p>
   * <p>
   * If a matrix R represents a rotation of r radians about the unit vector n, we know that R can be
   * represented as
   * 
   * <pre>
   *                                       2
   *    I  +  sin(r) N  +  [ 1 - cos(r) ] N ,
   * </pre>
   * 
   * where N is the matrix that satisfies
   * 
   * <pre>
   *    Nv = n x v
   * </pre>
   * 
   * for all vectors v, namely
   * 
   * <pre>
   *         +-                -+
   *         |  0    -n     n   |
   *         |         3     2  |
   *         |                  |
   *    N =  |  n     0    -n   |.
   *         |   3           1  |
   *         |                  |
   *         | -n     n     0   |
   *         |   2     1        |
   *         +-                -+
   * </pre>
   * 
   * Define S as
   * 
   * <pre>
   *    sin(r/2) N,
   * </pre>
   * 
   * and let our input quaternion Q be
   * 
   * <pre>
   *    ( q ,  q ,  q ,  q ).
   *       0    1    2    3
   * </pre>
   * 
   * Using the facts that
   * 
   * <pre>
   *                        2
   *    1 - cos(r)  =  2 sin (r/2)
   * </pre>
   * 
   * and
   * 
   * <pre>
   * 
   *    sin(r)      =  2 cos(r/2) sin(r/2),
   * </pre>
   * 
   * we can express R as
   * 
   * <pre>
   *                                 2
   *    I  +  2 cos(r/2) S    +   2 S,
   * </pre>
   * 
   * or
   * 
   * <pre>
   *                           2
   *    I  +  2 q  S    +   2 S.
   *             0
   * </pre>
   * 
   * Since S is just
   * 
   * <pre>
   *    +-                -+
   *    |  0    -q     q   |
   *    |         3     2  |
   *    |                  |
   *    |  q     0    -q   |,
   *    |   3           1  |
   *    |                  |
   *    | -q     q     0   |
   *    |   2     1        |
   *    +-                -+
   * </pre>
   * 
   * our expression for R comes out to
   * 
   * <pre>
   *    +-                                                         -+
   *    |          2   2                                            |
   *    | 1 - 2 ( q + q  )    2( q q  -  q q )     2 ( q q  + q q ) |
   *    |          2   3          1 2     0 3           1 3    0 2  |
   *    |                                                           |
   *    |                              2   2                        |
   *    | 2( q q  +  q q )    1 - 2 ( q + q  )     2 ( q q  - q q ) |.
   *    |     1 2     0 3              1   3            2 3    0 1  |
   *    |                                                           |
   *    |                                                   2   2   |
   *    | 2( q q  -  q q )    2 ( q q  + q q )     1 - 2 ( q + q  ) |
   *    |     1 3     0 2          2 3    0 1               1   2   |
   *    +-                                                         -+
   * </pre>
   * 
   * </p>
   */
  @Override
  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer) {

    double q01 = q0 * q1;
    double q02 = q0 * q2;
    double q03 = q0 * q3;
    double q12 = q1 * q2;
    double q13 = q1 * q3;
    double q23 = q2 * q3;
    double q1s = q1 * q1;
    double q2s = q2 * q2;
    double q3s = q3 * q3;

    /*
     * Sharpen the computation by effectively converting this quaternion to a unit quaternion if it
     * is not already one.
     */
    double l2 = q0 * q0 + q1s + q2s + q3s;

    if ((l2 != 1.0) && (l2 != 0.0)) {
      double sharpen = 1.0 / l2;

      q01 = q01 * sharpen;
      q02 = q02 * sharpen;
      q03 = q03 * sharpen;
      q12 = q12 * sharpen;
      q13 = q13 * sharpen;
      q23 = q23 * sharpen;
      q1s = q1s * sharpen;
      q2s = q2s * sharpen;
      q3s = q3s * sharpen;
    }

    /*
     * Now inject the results into the public fields of matrix directly. Note that the individual
     * components of matrix are laid out in this fashion: +- -+ | ii ij ik | buffer = | ji jj jk | |
     * ki kj kk | +- -+
     */
    PrivilegedRotationMatrixIJK assigner = new PrivilegedRotationMatrixIJK();
    assigner.setToWithoutCheck(1.0 - 2.0 * (q2s + q3s), 2.0 * (q12 + q03), 2.0 * (q13 - q02),
        2.0 * (q12 - q03), 1.0 - 2.0 * (q1s + q3s), 2.0 * (q23 + q01), 2.0 * (q13 + q02),
        2.0 * (q23 - q01), 1.0 - 2.0 * (q1s + q2s));
    buffer.setTo(assigner);

    return buffer;

  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(q0);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(q1);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(q2);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(q3);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    return result;
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this considers the equality of the components only in the comparison. It is possible that
   * the components, while different capture precisely the same rotation. This method only evaluates
   * whether the individual components are identical. Note, classes that derive from this class may
   * be considered equal to instances of the parent if they have identical fields. This method uses
   * instanceof in its determination of whether to proceed, not Class comparison.
   */
  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof Quaternion)) {
      return false;
    }
    final Quaternion other = (Quaternion) obj;
    if (Double.doubleToLongBits(q0) != Double.doubleToLongBits(other.q0)) {
      return false;
    }
    if (Double.doubleToLongBits(q1) != Double.doubleToLongBits(other.q1)) {
      return false;
    }
    if (Double.doubleToLongBits(q2) != Double.doubleToLongBits(other.q2)) {
      return false;
    }
    if (Double.doubleToLongBits(q3) != Double.doubleToLongBits(other.q3)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return ("[" + String.valueOf(q0) + " " + String.valueOf(q1) + " " + String.valueOf(q2) + " "
        + String.valueOf(q3) + "]");
  }

}
