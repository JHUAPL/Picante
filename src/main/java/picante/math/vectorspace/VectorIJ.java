package picante.math.vectorspace;

import static com.google.common.base.Preconditions.checkElementIndex;
import static picante.math.vectorspace.InternalOperations.absMaxComponent;
import static picante.math.vectorspace.InternalOperations.computeNorm;
import picante.designpatterns.Writable;
import picante.exceptions.BugException;

/**
 * Writable subclass of the unwritable 2D vector parent completing the implementation of the weak
 * immutability design pattern.
 * <p>
 * This class contains the mutator methods necessary to set or alter the internals of the parent
 * classes fields.
 * </p>
 * 
 * This was a simple copy and paste of the {@link VectorIJK} class. The cross product methods return
 * a {@link VectorIJK}.
 * 
 * TODO add rotation methods if when needed
 * 
 * @author G.K.Stephens copy 
 */
public class VectorIJ extends UnwritableVectorIJ
    implements Writable.ImplementationInterface<UnwritableVectorIJ, VectorIJ> {

  /**
   * The ZERO vector.
   */
  public static final UnwritableVectorIJ ZERO = new UnwritableVectorIJ(0, 0);

  /**
   * The I basis vector: (1,0).
   */
  public static final UnwritableVectorIJ I = new UnwritableVectorIJ(1, 0);

  /**
   * The J basis vector: (0,1).
   */
  public static final UnwritableVectorIJ J = new UnwritableVectorIJ(0, 1);

  /**
   * The negative of the I basis vector: (-1,0).
   */
  public static final UnwritableVectorIJ MINUS_I = new UnwritableVectorIJ(-1, 0);

  /**
   * The negative of the J basis vector: (0,-1).
   */
  public static final UnwritableVectorIJ MINUS_J = new UnwritableVectorIJ(0, -1);

  /**
   * Construct a vector with an initial value of {@link VectorIJ#ZERO}
   */
  public VectorIJ() {
    super(0, 0);
  }

  /**
   * Constructs a vector from two basic components
   * 
   * @param i the ith component
   * @param j the jth component
   */
  public VectorIJ(double i, double j) {
    super(i, j);
  }

  /**
   * Constructs a vector from the first two elements of an array of doubles.
   * 
   * @param data the array of doubles.
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least two
   *         elements
   */
  public VectorIJ(double[] data) {
    super(data);
  }

  /**
   * Constructs a vector from the two elements of an array of double starting with the offset index.
   * 
   * @param offset index into the data array to copy into the ith component.
   * 
   * @param data the array of doubles.
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain two elements at
   *         indices offset through offset + 2
   */
  public VectorIJ(int offset, double[] data) {
    super(offset, data);
  }

  /**
   * Copy constructor, creates a vector by copying the values of a pre-exisiting one.
   * 
   * @param vector the vector whose contents are to be copied
   */
  public VectorIJ(UnwritableVectorIJ vector) {
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
  public VectorIJ(double scale, UnwritableVectorIJ vector) {
    super(scale, vector);
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public VectorIJ createUnitized() {
    return new VectorIJ(this).unitize();
  }

  /**
   * {@inheritDoc}
   * 
   * Note: this method is overridden to return an instance of the writable subclass rather than the
   * unwritable parent.
   */
  @Override
  public VectorIJ createNegated() {
    return new VectorIJ(this).negate();
  }

  /**
   * Scale the vector.
   * 
   * @param scale the scale factor to apply.
   * 
   * @return a reference to the instance for convenience, which now contains (scale*this)
   */
  public VectorIJ scale(double scale) {
    this.i *= scale;
    this.j *= scale;

    return this;
  }

  /**
   * Unitize the vector.
   * 
   * @return a reference to the instance for convenience, which now contains a vector of unit length
   *         in the direction of the original vector.
   * 
   * @throws UnsupportedOperationException if the vector is equal to {@link VectorIJ#ZERO}
   */
  public VectorIJ unitize() {

    double norm = computeNorm(this.i, this.j);

    if (norm == 0.0) {
      throw new UnsupportedOperationException("Unable to unitize vector. Instance is zero length.");
    }

    this.i /= norm;
    this.j /= norm;

    return this;
  }

  /**
   * Negate the vector.
   * 
   * @return a reference to the instance, now containing -vector.
   */
  public VectorIJ negate() {
    this.i *= -1.0;
    this.j *= -1.0;

    return this;
  }

  /**
   * Clear the vector.
   * 
   * @return a reference to the instance, now containing {@link VectorIJ#ZERO}
   */
  public VectorIJ clear() {
    this.i = 0.0;
    this.j = 0.0;

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
   * Sets the specified component of the vector to a supplied value.
   * 
   * @param index the index of the component to set. 0 = ith, 1 = jth.
   * @param value the value with which to replace the specified component
   * 
   * @throws IndexOutOfBoundsException if an invalid index, outside the range [0,1], is specified.
   */
  public final void set(int index, double value) {
    checkElementIndex(index, 2, "component");
    switch (index) {
      case 0:
        i = value;
        return;

      case 1:
        j = value;
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
  public final VectorIJ setTo(UnwritableVectorIJ vector) {
    this.i = vector.i;
    this.j = vector.j;
    return this;
  }

  /**
   * Set the vector contents to match the scale of another.
   * 
   * @param scale the scale factor to apply to vector
   * @param vector the vector whose scaled contents are to be copied into the vector
   */
  public final VectorIJ setTo(double scale, UnwritableVectorIJ vector) {
    this.i = scale * vector.i;
    this.j = scale * vector.j;
    return this;
  }

  /**
   * Sets the basic components of a vector copying the values from the supplied array.
   * 
   * @param data the array of doubles to copy.
   * 
   * @return a reference to the instance
   */
  public final VectorIJ setTo(double[] data) {
    return setTo(data[0], data[1]);
  }

  /**
   * Sets the basic components of a vector copying the values from an array at the offset location.
   * 
   * @param offset the offset into the data array
   * @param data array of doubles to copy into vector
   * 
   * @return a reference to the instance
   */
  public final VectorIJ setTo(int offset, double[] data) {
    return setTo(data[offset], data[offset + 1]);
  }

  /**
   * Sets the basic components of the vector.
   * 
   * @param i the ith component
   * @param j the jth component
   */
  public final VectorIJ setTo(double i, double j) {
    this.i = i;
    this.j = j;
    return this;
  }

  /**
   * Sets the vector content to the a unit length version of another.
   * 
   * @param vector the vector whose contents are to be unitized and stored in the instance
   * 
   * @return a reference to the instance
   * 
   * @throws UnsupportedOperationException if the supplied vector argument is {@link VectorIJ#ZERO}
   */
  public final VectorIJ setToUnitized(UnwritableVectorIJ vector) {
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
  public final VectorIJ setToNegated(UnwritableVectorIJ vector) {
    setTo(vector);
    return negate();
  }

  /**
   * Wraps this vector as a new {@link VectorIJK}. K is zero.
   * 
   * @return a new {@link VectorIJK}
   */
  public VectorIJK asVectorIJK() {
    return asVectorIJK(new VectorIJK());
  }

  /**
   * Wraps this vector in the {@link VectorIJK} buffer. K is zero.
   * 
   * @return the buffer for convenience.
   */
  public VectorIJK asVectorIJK(VectorIJK buffer) {
    buffer.setI(this.i);
    buffer.setJ(this.j);
    buffer.setK(0.0);
    return buffer;
  }

  /**
   * Compute the projection of one vector onto the line normal to another.
   * 
   * @param vector the vector to project
   * @param normal the normal to the line to project vector onto
   * 
   * @return a new <code>VectorIJ</code> containing the results of the projection
   * 
   * @throws IllegalArgumentException if normal is equal to {@link ZERO}
   * 
   * @see lineProject
   */
  public static VectorIJ lineProject(UnwritableVectorIJ vector, UnwritableVectorIJ normal) {
    return lineProject(vector, normal, new VectorIJ());
  }

  /**
   * Compute the projection of one vector onto the line normal to another.
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
   * @param normal the normal to the line to project vector onto
   * @param buffer the buffer to receive the contents of the projection
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if normal is equal to {@link ZERO}
   */
  public static VectorIJ lineProject(UnwritableVectorIJ vector, UnwritableVectorIJ normal,
      VectorIJ buffer) {

    double maxVector = absMaxComponent(vector.i, vector.j);

    /*
     * There are two unusual cases that require special treatment. The first is if the normal vector
     * is the zero vector. Fortunately, the necessary exception is generated by the project()
     * method. So, start by performing the necessary projection. Buffer the components of vector, in
     * case buffer and vector are the same object.
     */
    double vi = vector.i;
    double vj = vector.j;

    project(vector, normal, buffer);

    /*
     * The second unusual case is when vector itself is zero. This is simple enough, the zero vector
     * projects as the zero vector.
     */
    if (maxVector == 0.0) {
      buffer.clear();
      return buffer;
    }

    /*
     * Scale buffer and the v components by 1.0/maxVector to bring them closer to similar
     * magnitudes.
     */
    vi /= maxVector;
    vj /= maxVector;
    buffer.i /= maxVector;
    buffer.j /= maxVector;

    /*
     * Subtract buffer from the v components to place the result in the line.
     */
    buffer.i = vi - buffer.i;
    buffer.j = vj - buffer.j;

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
   * @return a new <code>VectorIJ</code> containing the results of the projection
   * 
   * @throws IllegalArgumentException if onto is the equal to {@link ZERO}.
   * 
   * @see project
   */
  public static VectorIJ project(UnwritableVectorIJ vector, UnwritableVectorIJ onto) {
    return project(vector, onto, new VectorIJ());

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
   * @throws IllegalArgumentException if onto is the equal to {@link ZERO}.
   */
  public static VectorIJ project(UnwritableVectorIJ vector, UnwritableVectorIJ onto,
      VectorIJ buffer) {

    double maxVector = absMaxComponent(vector.i, vector.j);
    double maxOnto = absMaxComponent(onto.i, onto.j);

    if (maxOnto == 0) {
      throw new IllegalArgumentException("Unable to project vector onto the zero vector.");
    }

    if (maxVector == 0) {
      buffer.clear();
      return buffer;
    }

    double r1 = onto.i / maxOnto;
    double r2 = onto.j / maxOnto;

    double t1 = vector.i / maxVector;
    double t2 = vector.j / maxVector;

    double scaleFactor = (t1 * r1 + t2 * r2) * maxVector / (r1 * r1 + r2 * r2);

    buffer.i = r1;
    buffer.j = r2;

    buffer.scale(scaleFactor);
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
   * @return a new <code>VectorIJ</code> which now contains ( scaleA*a + scaleB*b + scaleC*c )
   * 
   * @see combine
   */
  public static VectorIJ combine(double scaleA, UnwritableVectorIJ a, double scaleB,
      UnwritableVectorIJ b, double scaleC, UnwritableVectorIJ c) {
    return combine(scaleA, a, scaleB, b, scaleC, c, new VectorIJ());
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
  public static VectorIJ combine(double scaleA, UnwritableVectorIJ a, double scaleB,
      UnwritableVectorIJ b, double scaleC, UnwritableVectorIJ c, VectorIJ buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i + scaleC * c.i;
    buffer.j = scaleA * a.j + scaleB * b.j + scaleC * c.j;

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
   * @return a new <code>VectorIJ</code> which now contains ( scaleA*a + scaleB*b )
   * 
   * @see combine
   */
  public static VectorIJ combine(double scaleA, UnwritableVectorIJ a, double scaleB,
      UnwritableVectorIJ b) {
    return combine(scaleA, a, scaleB, b, new VectorIJ());
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
  public static VectorIJ combine(double scaleA, UnwritableVectorIJ a, double scaleB,
      UnwritableVectorIJ b, VectorIJ buffer) {

    buffer.i = scaleA * a.i + scaleB * b.i;
    buffer.j = scaleA * a.j + scaleB * b.j;

    return buffer;
  }

  /**
   * Compute the cross product of a and b; unitize the result.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * 
   * @return a new <code>VectorIJ</code> which now contains (a x b)/||a||/||b||.
   * 
   * @throws IllegalArgumentException if either a or b are equivalent to the {@link ZERO}
   * 
   * @throws UnsupportedOperationException if the result of crossing a with b results in
   *         {@link ZERO}
   * 
   * @see uCross
   */
  public static VectorIJK uCross(UnwritableVectorIJ a, UnwritableVectorIJ b) {
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
   * @throws IllegalArgumentException if either a or b are equivalent to the {@link ZERO}
   * 
   * @throws UnsupportedOperationException if the result of crossing a with b results in
   *         {@link ZERO}
   */
  public static VectorIJK uCross(UnwritableVectorIJ a, UnwritableVectorIJ b, VectorIJK buffer) {

    /*
     * We should scale each vector by its maximal component.
     */
    double amax = absMaxComponent(a.i, a.j);
    double bmax = absMaxComponent(b.i, b.j);

    if ((amax == 0.0) || (bmax == 0.0)) {
      throw new IllegalArgumentException("At least one input vector is of zero"
          + " length. Unable to unitize resultant" + " cross product.");
    }

    double ti = 0.0;
    double tj = 0.0;
    double tk = (a.i / amax) * (b.j / bmax) - (a.j / amax) * (b.i / bmax);

    buffer.setI(ti);
    buffer.setJ(tj);
    buffer.setK(tk);

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
   * @see cross
   */
  public static VectorIJK cross(UnwritableVectorIJ a, UnwritableVectorIJ b) {
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
  public static VectorIJK cross(UnwritableVectorIJ a, UnwritableVectorIJ b, VectorIJK buffer) {

    double ti = 0.0;
    double tj = 0.0;
    double tk = a.i * b.j - a.j * b.i;

    buffer.setI(ti);
    buffer.setJ(tj);
    buffer.setK(tk);

    return buffer;
  }

  /**
   * Subtract one vector from another.
   * 
   * @param a the minuend
   * @param b the subtrahend
   * 
   * @return a new <code>VectorIJ</code> which now contains (a - b)
   * 
   * @see subtract
   */
  public static VectorIJ subtract(UnwritableVectorIJ a, UnwritableVectorIJ b) {
    return subtract(a, b, new VectorIJ());
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
  public static VectorIJ subtract(UnwritableVectorIJ a, UnwritableVectorIJ b, VectorIJ buffer) {
    buffer.i = a.i - b.i;
    buffer.j = a.j - b.j;

    return buffer;
  }

  /**
   * Add two vectors.
   * 
   * @param a a vector
   * @param b another vector
   * 
   * @return a new <code>VectorIJ</code> which now contains (a + b).
   * 
   * @see add
   */
  public static VectorIJ add(UnwritableVectorIJ a, UnwritableVectorIJ b) {
    return add(a, b, new VectorIJ());
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
  public static VectorIJ add(UnwritableVectorIJ a, UnwritableVectorIJ b, VectorIJ buffer) {
    buffer.i = a.i + b.i;
    buffer.j = a.j + b.j;

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
  public static VectorIJ addAll(Iterable<? extends UnwritableVectorIJ> vectors, VectorIJ buffer) {
    double sumI = 0.0;
    double sumJ = 0.0;

    for (UnwritableVectorIJ vector : vectors) {
      sumI += vector.i;
      sumJ += vector.j;
    }

    return buffer.setTo(sumI, sumJ);
  }

  /**
   * Adds all the vectors in an {@link Iterable} of vectors.
   * 
   * @param vectors an {@link Iterable} of vectors to be added
   * 
   * @return a new <code>VectorIJ</code> for convenience which now contains (a + b + ... + n).
   * 
   * @see VectorIJ#addAll(Iterable, VectorIJ)
   */
  public static VectorIJ addAll(Iterable<? extends UnwritableVectorIJ> vectors) {
    return addAll(vectors, new VectorIJ());
  }

  /**
   * Performs a component wise root sum square of two vectors (add in quadrature).
   * 
   * @param a a vector
   * @param b another vector
   * 
   * @return a new <code>VectorIJ</code> which now contains (sqrt(a<sub>i</sub> + b<sub>i</sub> ) ,
   *         sqrt(a<sub>j</sub> + b<sub>j</sub>)).
   * 
   * @see VectorIJ#addRSS(UnwritableVectorIJ, UnwritableVectorIJ, VectorIJ)
   */
  public static VectorIJ addRSS(UnwritableVectorIJ a, UnwritableVectorIJ b) {
    return addRSS(a, b, new VectorIJ());
  }

  /**
   * Performs a component wise root sum square of two vectors (add in quadrature).
   * 
   * @param a a vector
   * @param b another vector
   * @param buffer the buffer to receive the results of the root sum square
   * 
   * @return a reference to buffer for convenience which now contains (sqrt(a<sub>i</sub> +
   *         b<sub>i</sub> ) , sqrt(a<sub>j</sub> + b<sub>j</sub>)).
   */
  public static VectorIJ addRSS(UnwritableVectorIJ a, UnwritableVectorIJ b, VectorIJ buffer) {

    double i = computeNorm(a.i, b.i);
    double j = computeNorm(a.j, b.j);

    return buffer.setTo(i, j);
  }

}
