package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import static java.lang.Math.abs;
import com.google.common.base.MoreObjects;
import com.google.common.primitives.Doubles;
import picante.designpatterns.Writable;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * A weakly immutable 3-dimensional plane designed to properly support a writable subclass.
 * <p>
 * This class captures a canonical representation of a plane in 3-dimensions. It supports a variety
 * of representations:
 * <ul>
 * <li>Normal vector and constant: &lt; x, normal &gt; = constant</li>
 * <li>Normal vector and point: &lt; x - point, normal &gt; = 0</li>
 * <li>Point and spanning vectors: point + s*u + t*v</li>
 * </ul>
 * </p>
 */
public class UnwritablePlane implements Surface {

  private static final double PLANE_CONTAINMENT_TOLERANCE = 5e-14;

  private static final double INVERSE_PROJECTION_BOUND = 10.0;

  /**
   * The plane constant. In the canonical form this is always non-negative and captures the minimum
   * distance to the plane from the origin.
   */
  private double constant;

  /**
   * The unit normal that points from the origin to the point closest in the plane.
   */
  private final VectorIJK theNormal;

  /**
   * Package private accessible read-only view of the actual normal vector.
   */
  final UnwritableVectorIJK normal;


  /**
   * Private constructor that builds a default plane. This is only to be utilized within the
   * constructors that exist on this class.
   */
  private UnwritablePlane() {
    theNormal = new VectorIJK(VectorIJK.K);
    normal = theNormal;
    constant = 0;
  }

  public static UnwritablePlane copyOf(UnwritablePlane plane) {
    if (plane.getClass().equals(UnwritablePlane.class)) {
      return plane;
    }
    return new UnwritablePlane(plane);
  }

  /**
   * Constructs a plane from the supplied normal vector and constant.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x, normal &gt; = constant
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * 
   * @param normal the normal vector
   * @param constant the constant
   * 
   * @throws IllegalArgumentException if normal is zero length.
   */
  public UnwritablePlane(UnwritableVectorIJK normal, double constant) {
    this();
    setTo(normal, constant);
  }

  /**
   * Constructs a plane from the supplied normal vector and constant.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x, normal &gt; = constant
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * 
   * @param normal the normal vector
   * @param constant the constant
   * 
   * @throws IllegalArgumentException if normal is zero length.
   */
  public UnwritablePlane(UnwritableVectorIJK normal, UnwritableVectorIJK point) {
    this();
    setTo(normal, point);
  }

  /**
   * Constructs a plane from the supplied point and two spanning vectors.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   * point + s * u + t * v
   * </pre>
   * 
   * where s and t are real numbers, u and v are the linearly independent spanning vectors, and
   * point is an arbitrary point in the plane of interest.
   * </p>
   * 
   * @param point an arbitrary point in the plane
   * @param u one of the two linearly independent spanning vectors
   * @param v the other of the spanning vectors
   * 
   * @throws IllegalArgumentException if u and v are parallel
   */
  public UnwritablePlane(UnwritableVectorIJK point, UnwritableVectorIJK u, UnwritableVectorIJK v) {
    this();
    setTo(point, u, v);
  }

  /**
   * Copy constructor.
   * 
   * @param plane creates a new plane, copying the contents of the supplied plane.
   */
  public UnwritablePlane(UnwritablePlane plane) {
    this();
    setTo(plane);
  }

  /**
   * Convenience method that creates a new vector and sets it to the unit normal to the plane.
   * 
   * @return the newly created unit normal vector
   */
  public VectorIJK getNormal() {
    return getNormal(new VectorIJK());
  }

  /**
   * Retrieves the unit normal vector.
   * 
   * @param buffer the buffer to receive the normal.
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJK getNormal(VectorIJK buffer) {
    return buffer.setTo(this.theNormal);
  }

  /**
   * Retrieves the plane constant.
   * 
   * @return the constant, also the distance of the plane from the origin.
   */
  public double getConstant() {
    return this.constant;
  }

  /**
   * Convenience method that creates a new vector and assigns it to the point in the plane.
   * 
   * @return the newly created vector capturing the point in the plane closest to the origin
   */
  public VectorIJK getPoint() {
    return getPoint(new VectorIJK());
  }

  /**
   * Retrieves a point in the plane.
   * 
   * @param buffer the buffer to receive the point in the plane.
   * 
   * @return the closest point in the plane to the origin. Point is always a non-negative multiple
   *         of normal.
   */
  public VectorIJK getPoint(VectorIJK buffer) {
    return buffer.setTo(this.theNormal).scale(this.constant);
  }

  /**
   * Retrieves a pair of vectors that span the plane.
   * 
   * @param uBuffer one of two orthonormal vectors that span the plane
   * @param vBuffer the other of two orthonormal vectors that span the plane
   */
  public void getSpanningVectors(VectorIJK uBuffer, VectorIJK vBuffer) {

    double a = this.theNormal.getI() * this.theNormal.getI();
    double b = this.theNormal.getJ() * this.theNormal.getJ();
    double c = this.theNormal.getK() * this.theNormal.getK();

    /*
     * This is a bit complicated. Determine the component of the normal that has the smallest
     * magnitude. This component with then arbitrarily be selected to be zero in uBuffer, the first
     * of the two spanning vectors.
     * 
     * The other two components of the normal will be put into uBuffer, swapped with the sign of the
     * first changed. From this selection, vBuffer can have only one possible set of values which it
     * obtains from the smallest component of the normal, the non-zero components of uBuffer, and
     * the length of uBuffer.
     */
    double f;
    int s1, s2, s3;
    if ((a <= b) && (a <= c)) {

      /*
       * The i component of the normal vector is the smallest.
       */
      f = Math.sqrt(b + c);
      s1 = 0;
      s2 = 1;
      s3 = 2;

    } else if ((b <= a) && (b <= c)) {

      /*
       * The j component of the normal is the smallest.
       */
      f = Math.sqrt(a + c);
      s1 = 1;
      s2 = 2;
      s3 = 0;

    } else {

      /*
       * The k component of the normal is the smallest.
       */
      f = Math.sqrt(a + b);
      s1 = 2;
      s2 = 0;
      s3 = 1;
    }

    uBuffer.set(s1, 0.0);
    uBuffer.set(s2, -this.theNormal.get(s3) / f);
    uBuffer.set(s3, this.theNormal.get(s2) / f);

    vBuffer.set(s1, f);
    vBuffer.set(s2, -this.theNormal.get(s1) * uBuffer.get(s3));
    vBuffer.set(s3, this.theNormal.get(s1) * uBuffer.get(s2));

  }

  /**
   * Canonicalize the internal representation such that the constant is the distance from the origin
   * to the plane.
   */
  private void canonicalizeIfConstantNegative() {
    if (this.constant < 0.0) {
      this.constant = -this.constant;
      this.theNormal.negate();
    }
  }

  /**
   * Canonical method for copying the contents of one plane into another.
   * 
   * @param plane the plane to copy
   * 
   * @return a reference to the instance for convenience
   */
  UnwritablePlane setTo(UnwritablePlane plane) {
    this.constant = plane.constant;
    this.theNormal.setTo(plane.theNormal);
    return this;
  }

  /**
   * Canonical method for setting the contents of a plane to a normal vector and a constant.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x, normal &gt; = constant
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * <p>
   * Setters do not typically belong on the unwritable parent classes in the weak immutability
   * pattern {@link Writable}. In this particular case, these package private methods allow the
   * convenient consolidation of code.
   * </p>
   * 
   * @param normal the normal vector in the plane definition above
   * @param constant the constant in the plane definition above
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if normal is zero length.
   */
  UnwritablePlane setTo(UnwritableVectorIJK normal, double constant) {

    double normalLength = normal.getLength();
    checkArgument(normalLength != 0, "A plane's normal vector must be non-zero.");

    this.constant = constant / normalLength;
    this.theNormal.setTo(1.0 / normalLength, normal);

    canonicalizeIfConstantNegative();

    return this;
  }

  /**
   * Canonical method for setting the contents of a plane to a normal vector and a point.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x - point, normal &gt; = 0
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * <p>
   * Setters do not typically belong on the unwritable parent classes in the weak immutability
   * pattern {@link Writable}. In this particular case, these package private methods allow the
   * convenient consolidation of code.
   * </p>
   * 
   * @param normal the normal vector in the plane definition above
   * @param point a point in the plane
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if normal is zero length
   */
  UnwritablePlane setTo(UnwritableVectorIJK normal, UnwritableVectorIJK point) {
    checkArgument(!VectorIJK.ZERO.equals(normal), "A plane's normal must be non-zero.");
    this.theNormal.setTo(normal).unitize();
    this.constant = point.getDot(this.theNormal);

    canonicalizeIfConstantNegative();

    return this;
  }

  /**
   * Canonical method for setting the contents of a plane to a point and a pair of spanning vectors.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   * point + s * u + t * v
   * </pre>
   * 
   * where s and t are real numbers, u and v are the linearly independent spanning vectors, and
   * point is an arbitrary point in the plane of interest.
   * </p>
   * <p>
   * Setters do not typically belong on the unwritable parent classes in the weak immutability
   * pattern {@link Writable}. In this particular case, these package private methods allow the
   * convenient consolidation of code.
   * </p>
   * 
   * @param point the arbitrary point in the plane
   * @param u one of the two linearly independent spanning vectors
   * @param v the other of the two linearly independent spanning vectors
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if u and v are linearly dependent
   */
  UnwritablePlane setTo(UnwritableVectorIJK point, UnwritableVectorIJK u, UnwritableVectorIJK v) {

    /*
     * This is a bit convoluted. We are going to allow the normal vector field to be modified in
     * order to verify that u and v are linearly independent. If they are found to be dependent,
     * we'll restore the normal vector before returning. This may be unexpected behavior, but we're
     * going out of our way to preserve the prior state.
     */
    double i = this.theNormal.getI();
    double j = this.theNormal.getJ();
    double k = this.theNormal.getK();

    VectorIJK.uCross(u, v, this.theNormal);

    if (VectorIJK.ZERO.equals(this.theNormal)) {
      this.theNormal.setTo(i, j, k);
      throw new IllegalArgumentException("Spanning vectors are parallel.");
    }

    /*
     * Determine the constant corresponding to the unit normal vector.
     */
    this.constant = theNormal.getDot(point);

    canonicalizeIfConstantNegative();

    return this;
  }

  public VectorIJK projectOnto(UnwritableVectorIJK vector, VectorIJK buffer) {
    return VectorIJK.combine(1.0, vector, constant - vector.getDot(theNormal), theNormal, buffer);

  }

  public VectorIJK projectOnto(UnwritableVectorIJK vector) {
    return projectOnto(vector, new VectorIJK());
  }

  /**
   * Computes the inverse projection of a vector onto the plane.
   * 
   * @param vector the vector that lies in the instance plane
   * @param inversePlane the plane from which the vector originated under projection
   * @param buffer the buffer to receive the inverse image of vector under the projection from
   *        inversePlane. The result captured here lies in inversePlane such that
   *        inversePlane.projectOnto(buffer) yields vector.
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IllegalArgumentException if the computation can not proceed reliably due to numerical
   *         considerations. Typically this happens if the instance and inversePlane are
   *         sufficiently close to orthogonal to one another.
   */
  public VectorIJK inverseProjectOnto(UnwritableVectorIJK vector, UnwritablePlane inversePlane,
      VectorIJK buffer) {
    double numerator = inversePlane.constant - vector.getDot(inversePlane.normal);
    double denominator = normal.getDot(inversePlane.normal);

    double limit = (Math.abs(numerator) < 1.0) ? INVERSE_PROJECTION_BOUND / Double.MAX_VALUE
        : Math.abs((INVERSE_PROJECTION_BOUND / Double.MAX_VALUE) * numerator);

    checkArgument(Math.abs(denominator) > limit, "Unable to compute inverse projection.");

    return VectorIJK.combine(1.0, vector, numerator / denominator, normal, buffer);

  }

  public VectorIJK inverseProjectOnto(UnwritableVectorIJK vector, UnwritablePlane inversePlane) {
    return inverseProjectOnto(vector, inversePlane, new VectorIJK());
  }

  /**
   * Determines if the supplied vector is contained in the plane subject to a sufficiently tight
   * tolerance.
   * 
   * @param vector
   * 
   * @return
   */
  public boolean contains(UnwritableVectorIJK vector) {
    return contains(vector, PLANE_CONTAINMENT_TOLERANCE);
  }

  /**
   * 
   * @param vector
   * @param tolerance
   * 
   * @return
   * 
   * @throws IllegalArgumentException if tolerance is not strictly positive
   */
  public boolean contains(UnwritableVectorIJK vector, double tolerance) {
    checkArgument(tolerance > 0, "Tolerance must be strictly positive");
    double vectorConstant = vector.getDot(normal);

    if (vectorConstant == constant) {
      return true;
    }
    if (constant * vectorConstant == 0) {
      return Math.abs(constant - vectorConstant) < tolerance;
    }
    return Math.abs(constant - vectorConstant)
        / (Math.abs(vectorConstant) + Math.abs(constant)) < tolerance;

  }

  /**
   * Computes the directed distance from a point to the plane. The sign of this value is positive if
   * the point is directed along the normal of the plane, and negative if it is directed against the
   * normal.
   * <p>
   * The shortest distance from the plane to the plane is the length of the vector perpendicular to
   * the plane that passes through the point, thus is parallel or anti-parallel to the normal of the
   * plane.
   * 
   * @param point an {@link UnwritableVectorIJK} representing a point
   * @return the directed distance from the plane to the point, the sign is positive if along normal
   */
  public double directedDistanceTo(UnwritableVectorIJK point) {

    double i = point.getI();
    double j = point.getJ();
    double k = point.getK();

    // scale all the components by the 1/maxSize to make all the distances of the same order, so
    // that when the sum is computed, it minimizes rounding errors
    double maxSize = Doubles.max(abs(i), abs(j), abs(k), abs(constant));

    if (maxSize == 0) {
      return 0.0;
    }

    // scale the values
    double vi = i / maxSize;
    double vj = j / maxSize;
    double vk = k / maxSize;

    double c = constant / maxSize;

    // undo the scaling
    return (vi * normal.getI() + vj * normal.getJ() + vk * normal.getK() - c) * maxSize;
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(constant);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + ((theNormal == null) ? 0 : theNormal.hashCode());
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
    if (!(obj instanceof UnwritablePlane)) {
      return false;
    }
    UnwritablePlane other = (UnwritablePlane) obj;
    if (Double.doubleToLongBits(constant) != Double.doubleToLongBits(other.constant)) {
      return false;
    }
    if (theNormal == null) {
      if (other.theNormal != null) {
        return false;
      }
    } else if (!theNormal.equals(other.theNormal)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return MoreObjects.toStringHelper(getClass()).add("constant", this.constant)
        .add("normal", this.normal).toString();
  }

  /**
   * Find whether the given ray intersects this plane. This method will return true only if the ray
   * intersects the plane in exactly one spot. If the ray lies in the plane, this method will return
   * false.
   */
  @Override
  public boolean intersects(UnwritableVectorIJK source, UnwritableVectorIJK ray) {
    PlaneIntersectionComputer intersectionComputer =
        new PlaneIntersectionComputer(constant, theNormal);
    return intersectionComputer.intersects(source, ray);
  }

  /**
   * Find the intersection of a ray and this plane. If there is no intersection or an infinite
   * number of intersections (the ray lies in the plane), this method will throw a
   * {@link NoIntersectionException}
   */
  @Override
  public VectorIJK compute(UnwritableVectorIJK source, UnwritableVectorIJK ray, VectorIJK buffer) {
    PlaneIntersectionComputer intersectionComputer =
        new PlaneIntersectionComputer(constant, theNormal);
    return intersectionComputer.compute(source, ray, buffer);
  }

  /**
   * {@inheritDoc}
   * 
   * "outward normal" does not mean much for a surface that isn't closed. For the infinite plane,
   * the "outward" normal could be on either side. We just return the normal of the plane.
   */
  @Override
  public VectorIJK computeOutwardNormal(UnwritableVectorIJK surfacePoint, VectorIJK buffer) {
    return theNormal;
  }

}
