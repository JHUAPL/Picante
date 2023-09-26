package picante.math.vectorspace;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkElementIndex;
import static picante.math.PicanteMath.cos;
import static picante.math.PicanteMath.sin;
import static picante.math.vectorspace.InternalOperations.absMaxComponent;
import static picante.math.vectorspace.InternalOperations.computeNorm;
import picante.designpatterns.Writable;
import picante.exceptions.BugException;

/**
 * Writable subclass of the unwritable 3D vector parent completing the implementation of the weak
 * immutability design pattern.
 * <p>
 * This class contains the mutator methods necessary to set or alter the internals of the parent
 * classes fields.
 * </p>
 */
public class VectorIJK extends UnwritableVectorIJK
    implements Writable.ImplementationInterface<UnwritableVectorIJK, VectorIJK> {

  /**
   * The ZERO vector.
   */
  public static final UnwritableVectorIJK ZERO = new UnwritableVectorIJK(0, 0, 0);

  /**
   * The I basis vector: (1,0,0).
   */
  public static final UnwritableVectorIJK I = new UnwritableVectorIJK(1, 0, 0);

  /**
   * The J basis vector: (0,1,0).
   */
  public static final UnwritableVectorIJK J = new UnwritableVectorIJK(0, 1, 0);

  /**
   * The K basis vector: (0,0,1).
   */
  public static final UnwritableVectorIJK K = new UnwritableVectorIJK(0, 0, 1);

  /**
   * The negative of the I basis vector: (-1,0,0).
   */
  public static final UnwritableVectorIJK MINUS_I = new UnwritableVectorIJK(-1, 0, 0);

  /**
   * The negative of the J basis vector: (0,-1,0).
   */
  public static final UnwritableVectorIJK MINUS_J = new UnwritableVectorIJK(0, -1, 0);

  /**
   * The negative of the K basis vector: (0,0,-1).
   */
  public static final UnwritableVectorIJK MINUS_K = new UnwritableVectorIJK(0, 0, -1);

  /**
   * Construct a vector with an initial value of {@link VectorIJK#ZERO}
   */
  public VectorIJK() {
    super(0, 0, 0);
  }

  /**
   * Constructs a vector from three basic components
   * 
   * @param i the ith component
   * @param j the jth component
   * @param k the kth component
   */
  public VectorIJK(double i, double j, double k) {
    super(i, j, k);
  }

  /**
   * Constructs a vector from the first three elements of an array of doubles.
   * 
   * @param data the array of doubles.
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least three
   *         elements
   */
  public VectorIJK(double[] data) {
    super(data);
  }

  /**
   * Constructs a vector from the three elements of an array of double starting with the offset
   * index.
   * 
   * @param offset index into the data array to copy into the ith component.
   * 
   * @param data the array of doubles.
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain three elements at
   *         indices offset through offset + 2
   */
  public VectorIJK(int offset, double[] data) {
    super(offset, data);
  }

  /**
   * Copy constructor, creates a vector by copying the values of a pre-exisiting one.
   * 
   * @param vector the vector whose contents are to be copied
   */
  public VectorIJK(UnwritableVectorIJK vector) {
    super(vector);
  }

  /**
   * Scaling constructor, creates a new vector by applying a scalar multiple to the components of a
   * pre-existing vector. This results in (scale*vector) being stored in the newly constructed
   * vector.
   * 
   * @param scale the scale factor to apply
   * @param vector the vector whose contents are to be scaled
   */
  public VectorIJK(double scale, UnwritableVectorIJK vector) {
    super(scale, vector);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public VectorIJK createUnitized() {
    return new VectorIJK(this).unitize();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public VectorIJK createNegated() {
    return new VectorIJK(this).negate();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public VectorIJK createScaled(double scale) {
    return new VectorIJK(this).scale(scale);
  }

  /**
   * Scale the vector.
   * 
   * @param scale the scale factor to apply.
   * 
   * @return a reference to the instance for convenience, which now contains (scale*this)
   */
  public VectorIJK scale(double scale) {
    this.i *= scale;
    this.j *= scale;
    this.k *= scale;

    return this;
  }



  /**
   * Unitize the vector.
   * 
   * @return a reference to the instance for convenience, which now contains a vector of unit length
   *         in the direction of the original vector.
   * 
   * @throws UnsupportedOperationException if the vector is equal to {@link VectorIJK#ZERO}
   */
  public VectorIJK unitize() {

    double norm = computeNorm(this.i, this.j, this.k);

    if (norm == 0.0) {
      throw new UnsupportedOperationException("Unable to unitize vector. Instance is zero length.");
    }

    this.i /= norm;
    this.j /= norm;
    this.k /= norm;

    return this;
  }

  /**
   * Negate the vector.
   * 
   * @return a reference to the instance, now containing -vector.
   */
  public VectorIJK negate() {
    this.i *= -1.0;
    this.j *= -1.0;
    this.k *= -1.0;

    return this;
  }

  /**
   * Clear the vector.
   * 
   * @return a reference to the instance, now containing {@link VectorIJK#ZERO}
   */
  public VectorIJK clear() {
    this.i = 0.0;
    this.j = 0.0;
    this.k = 0.0;

    return this;
  }

  /**
   * Sets the ith component of the vector.
   * 
   * @param i the ith component
   */
  public final void setI(double i) {
    this.i = i;
  }

  /**
   * Sets the jth component of the vector.
   * 
   * @param j the jth component
   */
  public final void setJ(double j) {
    this.j = j;
  }

  /**
   * Sets the kth component of the vector.
   * 
   * @param k the kth component
   */
  public final void setK(double k) {
    this.k = k;
  }

  /**
   * Sets the specified component of the vector to a supplied value.
   * 
   * @param index the index of the component to set. 0 = ith, 1 = jth, 2 = kth.
   * @param value the value with which to replace the specified component
   * 
   * @throws IllegalArgumentException if an invalid index, outside the range [0,2], is specified.
   */
  public final void set(int index, double value) {
    checkElementIndex(index, 3, "element");
    switch (index) {
      case 0:
        i = value;
        return;

      case 1:
        j = value;
        return;

      case 2:
        k = value;
        return;

      default:
        throw new BugException();
    }
  }

  /**
   * Set the vector contents to match those of another.
   * 
   * @param vector the vector whose contents are to be copied into the vector
   */
  @Override
  public final VectorIJK setTo(UnwritableVectorIJK vector) {
    this.i = vector.i;
    this.j = vector.j;
    this.k = vector.k;
    return this;
  }

  /**
   * Set the vector contents to match the scale of another.
   * 
   * @param scale the scale factor to apply to vector
   * @param vector the vector whose scaled contents are to be copied into the vector
   */
  public final VectorIJK setTo(double scale, UnwritableVectorIJK vector) {
    this.i = scale * vector.i;
    this.j = scale * vector.j;
    this.k = scale * vector.k;
    return this;
  }

  /**
   * Sets the basic components of a vector copying the values from the supplied array.
   * 
   * @param data the array of doubles to copy.
   * 
   * @return a reference to the instance
   */
  public final VectorIJK setTo(double[] data) {
    return setTo(data[0], data[1], data[2]);
  }

  /**
   * Sets the basic components of a vector copying the values from an array at the offset location.
   * 
   * @param offset the offset into the data array
   * @param data array of doubles to copy into vector
   * 
   * @return a reference to the instance
   */
  public final VectorIJK setTo(int offset, double[] data) {
    return setTo(data[offset], data[offset + 1], data[offset + 2]);
  }

  /**
   * Sets the basic components of the vector.
   * 
   * @param i the ith component
   * @param j the jth component
   * @param k the kth component
   */
  public final VectorIJK setTo(double i, double j, double k) {
    this.i = i;
    this.j = j;
    this.k = k;
    return this;
  }

  /**
   * Sets the vector content to the a unit length version of another.
   * 
   * @param vector the vector whose contents are to be unitized and stored in the instance
   * 
   * @return a reference to the instance
   * 
   * @throws UnsupportedOperationException if the supplied vector argument is {@link VectorIJK#ZERO}
   */
  public final VectorIJK setToUnitized(UnwritableVectorIJK vector) {
    setTo(vector);
    return unitize();
  }

  /**
   * Sets the vector content to a negated version of another.
   * 
   * @param vector the vector whose contents are to be negated and stored in the instance
   * 
   * @return a reference to the instance
   */
  public final VectorIJK setToNegated(UnwritableVectorIJK vector) {
    setTo(vector);
    return negate();
  }

  /**
   * Rotate one vector about another by an angle specified in radians.
   * 
   * @param vector the vector to rotate
   * @param axis the axis about which to rotate vector
   * @param angle the angle, in radians, through which to rotate
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if the axis is equal to {@link VectorIJK#ZERO}.
   * 
   * @see VectorIJK#rotate(UnwritableVectorIJK, UnwritableVectorIJK, double, VectorIJK)
   */
  public static VectorIJK rotate(UnwritableVectorIJK vector, UnwritableVectorIJK axis,
      double angle) {
    return rotate(vector, axis, angle, new VectorIJK());
  }

  /**
   * Rotate one vector about another by an angle specified in radians.
   * <p>
   * An example is perhaps the most straightforward means to explain this methods action. Given an
   * axis (0,0,1) and a rotation angle of PI/2, this method does the following:
   * 
   * <table>
   * <tr>
   * <th>vector</th>
   * <th>buffer</th>
   * </tr>
   * <tr>
   * <td>( 1, 2, 3 )</td>
   * <td>( -2, 1, 3 )</td>
   * </tr>
   * <tr>
   * <td>( 1, 0, 0 )</td>
   * <td>( 0, 1, 0 )</td>
   * </tr>
   * <tr>
   * <td>( 0, 1, 0 )</td>
   * <td>( -1, 0, 0 )</td>
   * </tr>
   * </table>
   * 
   * @param vector the vector to rotate
   * @param axis the axis about which to rotate vector
   * @param angle the angle, in radians, through which to rotate
   * @param buffer the buffer to receive the contents of the rotation
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if the axis is equal to {@link VectorIJK#ZERO}.
   */
  public static VectorIJK rotate(UnwritableVectorIJK vector, UnwritableVectorIJK axis, double angle,
      VectorIJK buffer) {

    /*
     * There is one exceptional case, namely if we try to rotate about the zero vector. We can check
     * this by using the project method, as it will throw the desired runtime exception. First cache
     * the contents of vector and axis as input, since we do not know if buffer is equivalent to
     * either of them.
     */
    double vi = vector.i;
    double vj = vector.j;
    double vk = vector.k;

    double ai = axis.i;
    double aj = axis.j;
    double ak = axis.k;

    /*
     * At this point, we are going to build a basis that is convenient for computing the rotated
     * vector. Start by projecting vector onto axis, one of the axes in our basis.
     */
    project(vector, axis, buffer);

    double norm = InternalOperations.computeNorm(ai, aj, ak);

    ai /= norm;
    aj /= norm;
    ak /= norm;

    /*
     * Store the contents of buffer as this is one of the components of our rotated vector in the
     * new basis.
     */
    double pi = buffer.i;
    double pj = buffer.j;
    double pk = buffer.k;

    /*
     * To determine one of the other vectors in the basis, simply subtract buffer from vector.
     */
    vi -= buffer.i;
    vj -= buffer.j;
    vk -= buffer.k;

    /*
     * Now determine the third basis vector by computing the cross product of a unit vector in the
     * direction of axis with buffer.
     */
    buffer.i = aj * vk - ak * vj;
    buffer.j = ak * vi - ai * vk;
    buffer.k = ai * vj - aj * vi;

    /*
     * The desired vector projection against this new basis is:
     * 
     * {pi,pj,pk} + cos(theta)*{v1i,v1j,v1k} + sin(theta)*buffer
     */
    buffer.i = pi + cos(angle) * vi + sin(angle) * buffer.i;
    buffer.j = pj + cos(angle) * vj + sin(angle) * buffer.j;
    buffer.k = pk + cos(angle) * vk + sin(angle) * buffer.k;

    return buffer;
  }

  /**
   * Compute the projection of one vector onto the plane normal to another.
   * 
   * @param vector the vector to project
   * @param normal the normal to the plane to project vector onto
   * 
   * @return a new <code>VectorIJK</code> containing the results of the projection
   * 
   * @throws IllegalArgumentException if normal is equal to {@link VectorIJK#ZERO}
   * 
   * @see VectorIJK#planeProject(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK planeProject(UnwritableVectorIJK vector, UnwritableVectorIJK normal) {
    return planeProject(vector, normal, new VectorIJK());
  }

  /**
   * Compute the projection of one vector onto the plane normal to another.
   * <p>
   * Algebraicly, this routine effectively computes:
   * 
   * <pre>
   *                           &lt;vector, to&gt; * to
   *                  vector - ---------------------
   *                              || to ||
   * </pre>
   * 
   * where &lt;,&gt; denotes the standard scalar product and ||x|| the norm of x. For numeric
   * precision reasons the implementation may vary slightly from the above prescription.
   * </p>
   * 
   * @param vector the vector to project
   * @param normal the normal to the plane to project vector onto
   * @param buffer the buffer to receive the contents of the projection
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if normal is equal to {@link VectorIJK#ZERO}
   */
  public static VectorIJK planeProject(UnwritableVectorIJK vector, UnwritableVectorIJK normal,
      VectorIJK buffer) {

    /*
     * If the supplied normal vector is the zero vector, generate the necessary exception.
     */
    checkArgument(!normal.equals(VectorIJK.ZERO), "Normal must not be the zero vector.");

    double maxVector = absMaxComponent(vector.i, vector.j, vector.k);

    /*
     * Check to see if maxVector is zero length. If it is, populate buffer with VectorIJK.ZERO.
     */
    if (maxVector == 0.0) {
      buffer.clear();
      return buffer;
    }

    /*
     * Create a scaled copy of the input vector to project.
     */
    VectorIJK scaledVector =
        new VectorIJK(vector.i / maxVector, vector.j / maxVector, vector.k / maxVector);

    project(scaledVector, normal, buffer);

    /*
     * The second unusual case is when vector itself is zero. This is simple enough, the zero vector
     * projects as the zero vector.
     */
    if (maxVector == 0.0) {
      buffer.clear();
      return buffer;
    }

    /*
     * Subtract buffer from the v components to place the result in the plane.
     */
    VectorIJK.subtract(scaledVector, buffer, buffer);

    /*
     * Rescale the result.
     */
    buffer.scale(maxVector);

    return buffer;

  }

  /**
   * Compute the projection of one vector onto another.
   * 
   * @param vector the vector to project
   * @param onto the vector onto which vector is to be projected
   * 
   * @return a new <code>VectorIJK</code> containing the results of the projection
   * 
   * @throws IllegalArgumentException if onto is the equal to {@link VectorIJK#ZERO}.
   * 
   * @see VectorIJK#project(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK project(UnwritableVectorIJK vector, UnwritableVectorIJK onto) {
    return project(vector, onto, new VectorIJK());

  }

  /**
   * Compute the projection of one vector onto another.
   * <p>
   * Algebraicly, this routine effectively computes:
   * 
   * <pre>
   *                  &lt;vector, onto&gt; * onto
   *                  ---------------------
   *                       || onto ||
   * </pre>
   * 
   * where &lt;,&gt; denotes the standard scalar product and ||x|| the norm of x. For numeric
   * precision reasons the implementation may vary slightly from the above prescription.
   * </p>
   * 
   * @param vector the vector to project
   * @param onto the vector onto which vector is to be projected
   * @param buffer the buffer to receive the contents of the projection
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if onto is the equal to {@link VectorIJK#ZERO}.
   */
  public static VectorIJK project(UnwritableVectorIJK vector, UnwritableVectorIJK onto,
      VectorIJK buffer) {

    double maxVector = absMaxComponent(vector.i, vector.j, vector.k);
    double maxOnto = absMaxComponent(onto.i, onto.j, onto.k);

    if (maxOnto == 0) {
      throw new IllegalArgumentException("Unable to project vector onto the zero vector.");
    }

    if (maxVector == 0) {
      buffer.clear();
      return buffer;
    }

    double r1 = onto.i / maxOnto;
    double r2 = onto.j / maxOnto;
    double r3 = onto.k / maxOnto;

    double t1 = vector.i / maxVector;
    double t2 = vector.j / maxVector;
    double t3 = vector.k / maxVector;

    double scaleFactor = (t1 * r1 + t2 * r2 + t3 * r3) * maxVector / (r1 * r1 + r2 * r2 + r3 * r3);

    buffer.i = r1;
    buffer.j = r2;
    buffer.k = r3;

    buffer.scale(scaleFactor);
    return buffer;
  }

  /**
   * Linearly combine eight vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param scaleF the scale factor for vector f
   * @param f the sixth vector
   * @param scaleG the scale factor for vector g
   * @param g the seventh vector
   * @param scaleH the scale factor for vector h
   * @param h the eighth vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b + scaleC*c +
   *         scaleD*d + scaleE*e + scaleF*f + scaleG*g + scaleH*h)
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, double scaleF,
      UnwritableVectorIJK f, double scaleG, UnwritableVectorIJK g, double scaleH,
      UnwritableVectorIJK h) {
    return combine(scaleA, a, scaleB, b, scaleC, c, scaleD, d, scaleE, e, scaleF, f, scaleG, g,
        scaleH, h, new VectorIJK());
  }

  /**
   * Linearly combine eight vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param scaleF the scale factor for vector f
   * @param f the sixth vector
   * @param scaleG the scale factor for vector g
   * @param g the seventh vector
   * @param scaleH the scale factor for vector h
   * @param h the eighth vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b +
   *         scaleC*c + scaleD*d + scaleE*e + scaleF*f + scaleG*g + scaleH*h)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, double scaleF,
      UnwritableVectorIJK f, double scaleG, UnwritableVectorIJK g, double scaleH,
      UnwritableVectorIJK h, VectorIJK buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i + scaleC * c.i + scaleD * d.i + scaleE * e.i
        + scaleF * f.i + scaleG * g.i + scaleH * h.i;
    buffer.j = scaleA * a.j + scaleB * b.j + scaleC * c.j + scaleD * d.j + scaleE * e.j
        + scaleF * f.j + scaleG * g.j + scaleH * h.j;
    buffer.k = scaleA * a.k + scaleB * b.k + scaleC * c.k + scaleD * d.k + scaleE * e.k
        + scaleF * f.k + scaleG * g.k + scaleH * h.k;

    return buffer;
  }

  /**
   * Linearly combine seven vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param scaleF the scale factor for vector f
   * @param f the sixth vector
   * @param scaleG the scale factor for vector g
   * @param g the sixth vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b + scaleC*c +
   *         scaleD*d + scaleE*e + scaleF*f + scaleG*g )
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, double scaleF,
      UnwritableVectorIJK f, double scaleG, UnwritableVectorIJK g) {
    return combine(scaleA, a, scaleB, b, scaleC, c, scaleD, d, scaleE, e, scaleF, f, scaleG, g,
        new VectorIJK());
  }

  /**
   * Linearly combine seven vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param scaleF the scale factor for vector f
   * @param f the sixth vector
   * @param scaleG the scale factor for vector g
   * @param g the sixth vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b +
   *         scaleC*c + scaleD*d + scaleE*e + scaleF*f + scaleG*g )
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, double scaleF,
      UnwritableVectorIJK f, double scaleG, UnwritableVectorIJK g, VectorIJK buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i + scaleC * c.i + scaleD * d.i + scaleE * e.i
        + scaleF * f.i + scaleG * g.i;
    buffer.j = scaleA * a.j + scaleB * b.j + scaleC * c.j + scaleD * d.j + scaleE * e.j
        + scaleF * f.j + scaleG * g.j;
    buffer.k = scaleA * a.k + scaleB * b.k + scaleC * c.k + scaleD * d.k + scaleE * e.k
        + scaleF * f.k + scaleG * g.k;

    return buffer;
  }

  /**
   * Linearly combine six vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param scaleF the scale factor for vector f
   * @param f the sixth vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b + scaleC*c +
   *         scaleD*d + scaleE*e + scaleF*f )
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, double scaleF,
      UnwritableVectorIJK f) {
    return combine(scaleA, a, scaleB, b, scaleC, c, scaleD, d, scaleE, e, scaleF, f,
        new VectorIJK());
  }

  /**
   * Linearly combine six vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param scaleF the scale factor for vector f
   * @param f the sixth vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b +
   *         scaleC*c + scaleD*d + scaleE*e + scaleF*f )
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, double scaleF,
      UnwritableVectorIJK f, VectorIJK buffer) {

    buffer.i =
        scaleA * a.i + scaleB * b.i + scaleC * c.i + scaleD * d.i + scaleE * e.i + scaleF * f.i;
    buffer.j =
        scaleA * a.j + scaleB * b.j + scaleC * c.j + scaleD * d.j + scaleE * e.j + scaleF * f.j;
    buffer.k =
        scaleA * a.k + scaleB * b.k + scaleC * c.k + scaleD * d.k + scaleE * e.k + scaleF * f.k;

    return buffer;
  }

  /**
   * Linearly combine five vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b + scaleC*c +
   *         scaleD*d + scaleE*e )
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e) {
    return combine(scaleA, a, scaleB, b, scaleC, c, scaleD, d, scaleE, e, new VectorIJK());
  }

  /**
   * Linearly combine five vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param scaleE the scale factor for vector e
   * @param e the fifth vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b +
   *         scaleC*c + scaleD*d + scaleE*e )
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, double scaleE, UnwritableVectorIJK e, VectorIJK buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i + scaleC * c.i + scaleD * d.i + scaleE * e.i;
    buffer.j = scaleA * a.j + scaleB * b.j + scaleC * c.j + scaleD * d.j + scaleE * e.j;
    buffer.k = scaleA * a.k + scaleB * b.k + scaleC * c.k + scaleD * d.k + scaleE * e.k;

    return buffer;
  }

  /**
   * Linearly combine four vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b + scaleC*c +
   *         scaleD*d)
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d) {
    return combine(scaleA, a, scaleB, b, scaleC, c, scaleD, d, new VectorIJK());
  }

  /**
   * Linearly combine four vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param scaleD the scale factor for vector d
   * @param d the fourth vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b +
   *         scaleC*c + scaleD*d)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, double scaleD,
      UnwritableVectorIJK d, VectorIJK buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i + scaleC * c.i + scaleD * d.i;
    buffer.j = scaleA * a.j + scaleB * b.j + scaleC * c.j + scaleD * d.j;
    buffer.k = scaleA * a.k + scaleB * b.k + scaleC * c.k + scaleD * d.k;

    return buffer;
  }

  /**
   * Linearly combine three vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b + scaleC*c )
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, double,
   *      UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c) {
    return combine(scaleA, a, scaleB, b, scaleC, c, new VectorIJK());
  }

  /**
   * Linearly combine three vectors.
   * 
   * @param scaleA the scale factor for vector a
   * @param a a vector
   * @param scaleB the scale vector for vector b
   * @param b another vector
   * @param scaleC the scale factor for vector c
   * @param c the third vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b +
   *         scaleC*c )
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, double scaleC, UnwritableVectorIJK c, VectorIJK buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i + scaleC * c.i;
    buffer.j = scaleA * a.j + scaleB * b.j + scaleC * c.j;
    buffer.k = scaleA * a.k + scaleB * b.k + scaleC * c.k;

    return buffer;
  }

  /**
   * Linearly combine two vectors.
   * 
   * @param scaleA a scale factor to apply to the vector a
   * @param a one vector
   * @param scaleB a scale factor to apply to the vector b
   * @param b another vector
   * 
   * @return a new <code>VectorIJK</code> which now contains ( scaleA*a + scaleB*b )
   * 
   * @see VectorIJK#combine(double, UnwritableVectorIJK, double, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b) {
    return combine(scaleA, a, scaleB, b, new VectorIJK());
  }

  /**
   * Linearly combine two vectors.
   * 
   * @param scaleA a scale factor to apply to the vector a
   * @param a one vector
   * @param scaleB a scale factor to apply to the vector b
   * @param b another vector
   * @param buffer the buffer to receive the contents of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b )
   */
  public static VectorIJK combine(double scaleA, UnwritableVectorIJK a, double scaleB,
      UnwritableVectorIJK b, VectorIJK buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i;
    buffer.j = scaleA * a.j + scaleB * b.j;
    buffer.k = scaleA * a.k + scaleB * b.k;

    return buffer;
  }

  /**
   * Compute the cross product of a and b; unitize the result.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * 
   * @return a new <code>VectorIJK</code> which now contains (a x b)/||a||/||b||.
   * 
   * @throws IllegalArgumentException if either a or b are equivalent to the {@link VectorIJK#ZERO}
   * 
   * @throws UnsupportedOperationException if the result of crossing a with b results in
   *         {@link VectorIJK#ZERO}
   * 
   * @see VectorIJK#uCross(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK uCross(UnwritableVectorIJK a, UnwritableVectorIJK b) {
    return uCross(a, b, new VectorIJK());
  }

  /**
   * Compute the cross product of a and b; unitize the result.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * @param buffer the buffer to receive the contents of the unitized cross product
   * @return a reference to buffer for convenience which now contains (a x b)/||a||/||b||.
   * 
   * @throws IllegalArgumentException if either a or b are equivalent to the {@link VectorIJK#ZERO}
   * 
   * @throws UnsupportedOperationException if the result of crossing a with b results in
   *         {@link VectorIJK#ZERO}
   */
  public static VectorIJK uCross(UnwritableVectorIJK a, UnwritableVectorIJK b, VectorIJK buffer) {

    /*
     * We should scale each vector by its maximal component.
     */
    double amax = absMaxComponent(a.i, a.j, a.k);
    double bmax = absMaxComponent(b.i, b.j, b.k);

    if ((amax == 0.0) || (bmax == 0.0)) {
      throw new IllegalArgumentException("At least one input vector is of zero"
          + " length. Unable to unitize resultant" + " cross product.");
    }

    double ti = (a.j / amax) * (b.k / bmax) - (a.k / amax) * (b.j / bmax);
    double tj = (a.k / amax) * (b.i / bmax) - (a.i / amax) * (b.k / bmax);
    double tk = (a.i / amax) * (b.j / bmax) - (a.j / amax) * (b.i / bmax);

    buffer.i = ti;
    buffer.j = tj;
    buffer.k = tk;

    return buffer.unitize();
  }

  /**
   * Compute the cross product of a and b.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * 
   * @return a new <code>VectorIJK</code> which now contains (a x b)
   * 
   * @see VectorIJK#cross(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK cross(UnwritableVectorIJK a, UnwritableVectorIJK b) {
    return cross(a, b, new VectorIJK());
  }

  /**
   * Compute the cross product of a and b.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * @param buffer the buffer to receive the contents of the cross product
   * 
   * @return a reference to buffer for convenience which now contains (a x b)
   */
  public static VectorIJK cross(UnwritableVectorIJK a, UnwritableVectorIJK b, VectorIJK buffer) {

    double ti = a.j * b.k - a.k * b.j;
    double tj = a.k * b.i - a.i * b.k;
    double tk = a.i * b.j - a.j * b.i;

    buffer.i = ti;
    buffer.j = tj;
    buffer.k = tk;

    return buffer;
  }


  /**
   * Create the Pointwise (aka Hadamard aka Schur) product of two vectors by multiplying their
   * corresponding components
   * 
   * @param a a vector
   * @param b another vector
   * 
   * @return a new <code>VectorIJK</code> which now contains (a .* b).
   * 
   * @see VectorIJK#pointwiseMultiply(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK pointwiseMultiply(UnwritableVectorIJK a, UnwritableVectorIJK b) {
    return pointwiseMultiply(a, b, new VectorIJK());
  }


  /**
   * Create the Pointwise (aka Hadamard aka Schur) product of two vectors by multiplying their
   * corresponding components
   * 
   * @param a a vector
   * @param b another vector
   * @param buffer the buffer to receive the results of the pointwise multiplication
   * 
   * @return a reference to buffer for convenience which now contains (a .* b).
   */
  public static VectorIJK pointwiseMultiply(UnwritableVectorIJK a, UnwritableVectorIJK b,
      VectorIJK buffer) {
    buffer.i = a.i * b.i;
    buffer.j = a.j * b.j;
    buffer.k = a.k * b.k;

    return buffer;

  }

  /**
   * Subtract one vector from another.
   * 
   * @param a the minuend
   * @param b the subtrahend
   * 
   * @return a new <code>VectorIJK</code> which now contains (a - b)
   * 
   * @see VectorIJK#subtract(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK subtract(UnwritableVectorIJK a, UnwritableVectorIJK b) {
    return subtract(a, b, new VectorIJK());
  }

  /**
   * Subtract one vector from another.
   * 
   * @param a the minuend
   * @param b the subtrahend
   * @param buffer the buffer to receive the results of the subtraction
   * 
   * @return a reference to buffer for convenience which now contains (a - b)
   */
  public static VectorIJK subtract(UnwritableVectorIJK a, UnwritableVectorIJK b, VectorIJK buffer) {
    buffer.i = a.i - b.i;
    buffer.j = a.j - b.j;
    buffer.k = a.k - b.k;

    return buffer;
  }

  /**
   * Add two vectors.
   * 
   * @param a a vector
   * @param b another vector
   * 
   * @return a new <code>VectorIJK</code> which now contains (a + b).
   * 
   * @see VectorIJK#add(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK add(UnwritableVectorIJK a, UnwritableVectorIJK b) {
    return add(a, b, new VectorIJK());
  }

  /**
   * Add two vectors.
   * 
   * @param a a vector
   * @param b another vector
   * @param buffer the buffer to receive the results of the addition
   * 
   * @return a reference to buffer for convenience which now contains (a + b).
   */
  public static VectorIJK add(UnwritableVectorIJK a, UnwritableVectorIJK b, VectorIJK buffer) {
    buffer.i = a.i + b.i;
    buffer.j = a.j + b.j;
    buffer.k = a.k + b.k;

    return buffer;
  }

  /**
   * Adds all the vectors in an {@link Iterable} of vectors.
   * 
   * @param vectors an {@link Iterable} of vectors to be added
   * @param buffer the buffer to receive the results of the addition
   * 
   * @return a reference to buffer for convenience which now contains (a + b + ... + n).
   */
  public static VectorIJK addAll(Iterable<? extends UnwritableVectorIJK> vectors,
      VectorIJK buffer) {
    double sumI = 0.0;
    double sumJ = 0.0;
    double sumK = 0.0;

    for (UnwritableVectorIJK vector : vectors) {
      sumI += vector.i;
      sumJ += vector.j;
      sumK += vector.k;
    }

    return buffer.setTo(sumI, sumJ, sumK);
  }

  /**
   * Adds all the vectors in an {@link Iterable} of vectors.
   * 
   * @param vectors an {@link Iterable} of vectors to be added
   * 
   * @return a new <code>VectorIJK</code> for convenience which now contains (a + b + ... + n).
   * 
   * @see VectorIJK#addAll(Iterable, VectorIJK)
   */
  public static VectorIJK addAll(Iterable<? extends UnwritableVectorIJK> vectors) {
    return addAll(vectors, new VectorIJK());
  }

  /**
   * Performs a component wise root sum square of two vectors (add in quadrature).
   * 
   * @param a a vector
   * @param b another vector
   * 
   * @return a new <code>VectorIJK</code> which now contains (sqrt(a<sub>i</sub> + b<sub>i</sub> ) ,
   *         sqrt(a<sub>j</sub> + b<sub>j</sub>) , sqrt(a<sub>k</sub> + b<sub>k</sub> )).
   * 
   * @see VectorIJK#addRSS(UnwritableVectorIJK, UnwritableVectorIJK, VectorIJK)
   */
  public static VectorIJK addRSS(UnwritableVectorIJK a, UnwritableVectorIJK b) {
    return addRSS(a, b, new VectorIJK());
  }

  /**
   * Performs a component wise root sum square of two vectors (add in quadrature).
   * 
   * @param a a vector
   * @param b another vector
   * @param buffer the buffer to receive the results of the root sum square
   * 
   * @return a reference to buffer for convenience which now contains (sqrt(a<sub>i</sub> + b
   *         <sub>i</sub> ) , sqrt(a<sub>j</sub> + b<sub>j</sub>) , sqrt(a<sub>k</sub> + b
   *         <sub>k</sub> )).
   */
  public static VectorIJK addRSS(UnwritableVectorIJK a, UnwritableVectorIJK b, VectorIJK buffer) {

    double i = computeNorm(a.i, b.i);
    double j = computeNorm(a.j, b.j);
    double k = computeNorm(a.k, b.k);

    return buffer.setTo(i, j, k);
  }


}
