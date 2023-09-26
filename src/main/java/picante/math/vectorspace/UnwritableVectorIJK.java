package picante.math.vectorspace;

import static com.google.common.base.Preconditions.checkElementIndex;
import static picante.math.PicanteMath.PI;
import static picante.math.PicanteMath.asin;
import static picante.math.vectorspace.InternalOperations.computeNorm;
import picante.exceptions.BugException;
import picante.units.FundamentalPhysicalConstants;

/**
 * A weakly immutable 3-dimensional vector designed to properly support a writable subclass.
 * <p>
 * <b>Note:</b>Subclass implementers, you should only use the protected fields in this class to
 * store the contents of the vector components, otherwise all of the methods here and in the
 * operations class may break.
 * </p>
 * <p>
 * The basic data fields on this class are marked as protected to allow direct access to them
 * through subclassing. This will get around any performance issues that one may have in using this
 * vector arithmetic toolkit due to the enforcement of access to the component values through
 * accessor methods.
 * </p>
 * <p>
 * Note, the equals and hashcode implementations in this class support proper comparisons between
 * subclasses of this class and this class. The reason this works, is because by design the only
 * member variables of this class live in the parent class. If one subclasses this class and defines
 * additional members then this will most certainly break the implementation presented here.
 * </p>
 */
public class UnwritableVectorIJK {

  /**
   * The ith component of the vector, synonymous with the &quot;X-axis&quot;.
   */
  protected double i;

  /**
   * The jth component of the vector, synonymous with the &quot;Y-axis&quot;.
   */
  protected double j;

  /**
   * The kth component of the vector, synonymous with the &quot;Z-axis&quot;.
   */
  protected double k;

  /**
   * Constructs a vector from the three basic components.
   * 
   * @param i the ith component
   * @param j the jth component
   * @param k the kth component
   */
  public UnwritableVectorIJK(double i, double j, double k) {
    super();
    this.i = i;
    this.j = j;
    this.k = k;
  }

  /**
   * Constructs a vector from the first three elements of an array of doubles.
   * 
   * @param data the array of doubles
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least three
   *         elements
   */
  public UnwritableVectorIJK(double[] data) {
    this(data[0], data[1], data[2]);
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
  public UnwritableVectorIJK(int offset, double[] data) {
    this(data[offset], data[offset + 1], data[offset + 2]);
  }

  /**
   * Copy constructor, creates a vector by copying the values of a pre-existing one.
   * 
   * @param vector the vector whose contents are to be copied
   */
  public UnwritableVectorIJK(UnwritableVectorIJK vector) {
    this(vector.i, vector.j, vector.k);
  }

  /**
   * Scaling constructor, creates a new vector by applying a scalar multiple to the components of a
   * pre-existing vector.
   * 
   * @param scale the scale factor to apply
   * @param vector the vector whose contents are to be scaled
   */
  public UnwritableVectorIJK(double scale, UnwritableVectorIJK vector) {
    this(scale * vector.i, scale * vector.j, scale * vector.k);
  }

  /**
   * Creates a new, unit length copy of the existing vector.
   * <p>
   * This code is just a convenience method that implements:
   * <code>new UnwritableVectorIJK(1.0/this.getLength(), this)</code> in a safe manner.
   * </p>
   * 
   * @return a vector of unit length in the direction of the instance
   * 
   * @throws UnsupportedOperationException if the instance vector has zero length.
   */
  public UnwritableVectorIJK createUnitized() {
    double norm = getLength();

    if (norm > 0.0) {
      return new UnwritableVectorIJK(1.0 / norm, this);
    }

    throw new UnsupportedOperationException("Unable to unitize. Supplied vector has zero length.");
  }

  /**
   * Creates a new, negated copy of an the existing vector.
   * <p>
   * Convenience method for: <code>new UnwritableVectorIJK(-1.0, this)</code>.
   * </p>
   * 
   * @return the negated vector, -this.
   */
  public UnwritableVectorIJK createNegated() {
    return new UnwritableVectorIJK(-1.0, this);
  }

  /**
   * Creates a new, scaled copy of an the existing vector by applying a scalar multiple to the
   * components.
   * <p>
   * Convenience method for: <code>new UnwritableVectorIJK(scale, this)</code>.
   * </p>
   * 
   * @param scale the scale factor to apply
   * @return the scaled vector, scale*this.
   */
  public UnwritableVectorIJK createScaled(double scale) {
    return new UnwritableVectorIJK(scale, this);
  }

  /**
   * Gets the ith component.
   * 
   * @return the ith component.
   */
  public final double getI() {
    return i;
  }

  /**
   * Gets the jth component.
   * 
   * @return the jth component.
   */
  public final double getJ() {
    return j;
  }

  /**
   * Gets the kth component.
   * 
   * @return the kth component.
   */
  public final double getK() {
    return k;
  }

  /**
   * Retrieves the specified component of the vector.
   * 
   * @param index the index of the component to retrieve. 0 = ith, 1 = jth, 2 = kth.
   * 
   * @return the value for the requested component
   * 
   * @throws IndexOutOfBoundsException if an invalid index, outside the range [0,2], is specified.
   */
  public final double get(int index) {
    checkElementIndex(index, 3, "component");
    switch (index) {
      case 0:
        return i;
      case 1:
        return j;
      case 2:
        return k;
      default:
        throw new BugException();
    }
  }

  /**
   * Computes the standard L-2 norm, or length, of the vector.
   * 
   * @return (i*i + j*j + k*k)^(1/2) without danger of overflow.
   */
  public double getLength() {
    return computeNorm(this.i, this.j, this.k);
  }

  /**
   * Compute the dot product of this instance with another vector.
   * 
   * @param vector the vector to dot against the instance.
   * 
   * @return i*vector.i + j*vector.j + k*vector.k
   */
  public double getDot(UnwritableVectorIJK vector) {
    return i * vector.i + j * vector.j + k * vector.k;
  }

  /**
   * Compute the angular separation in radians between this instance and another vector.
   * 
   * @param vector
   * 
   * @return the angular separation between vector and this instance in radians.
   * 
   * @throws UnsupportedOperationException if either this instance or the supplied vector are
   *         {@link VectorIJK#ZERO}
   */
  public double getSeparation(UnwritableVectorIJK vector) {
    double thisNorm = getLength();
    double vectorNorm = vector.getLength();

    if (thisNorm == 0.0) {
      throw new UnsupportedOperationException(
          "Unable to compute angular separation. " + "This vector is the zero vector.");
    } else if (vectorNorm == 0.0) {
      throw new UnsupportedOperationException(
          "Unable to compute angular separation. " + "The argument supplied is the zero vector.");
    }

    double dotProduct = getDot(vector);

    if (dotProduct > 0) {
      double x = i / thisNorm - vector.i / vectorNorm;
      double y = j / thisNorm - vector.j / vectorNorm;
      double z = k / thisNorm - vector.k / vectorNorm;

      return 2.0 * asin(0.5 * computeNorm(x, y, z));
    } else if (dotProduct < 0) {
      double x = i / thisNorm + vector.i / vectorNorm;
      double y = j / thisNorm + vector.j / vectorNorm;
      double z = k / thisNorm + vector.k / vectorNorm;

      return PI - 2.0 * asin(0.5 * computeNorm(x, y, z));
    }

    return FundamentalPhysicalConstants.HALFPI;
  }

  /**
   * Compute the angle this instance lies out of the plane specified by normal.
   * <p>
   * Note: this really is simply {@link FundamentalPhysicalConstants#HALFPI} -
   * {@link UnwritableVectorIJK#getSeparation(UnwritableVectorIJK)}, but is useful as a convenience
   * method.
   * </p>
   * 
   * @param normal the normal to the plane
   * 
   * @return the angular separation of this vector and the plane with the specified normal. Positive
   *         values lie on the same side as normal, negative on the other.
   */
  public double getSeparationOutOfPlane(UnwritableVectorIJK normal) {
    return FundamentalPhysicalConstants.HALFPI - getSeparation(normal);
  }

  /**
   * Compute the distance between this instance and another vector.
   * 
   * @param vector
   * 
   * @return the distance between vector and this instance in radians.
   */
  public double getDistance(UnwritableVectorIJK vector) {
    return VectorIJK.subtract(this, vector).getLength();
  }


  /**
   * Compute the minimum value in this vector
   * 
   * @return the min value
   */
  public double min() {
    return Math.min(i, Math.min(j, k));
  }

  /**
   * Compute the maximum value in this vector
   * 
   * @return the min value
   */
  public double max() {
    return Math.max(i, Math.max(j, k));
  }

  /**
   * Makes an unwritable copy of the supplied vector.
   * <p>
   * This method makes an unwritable copy only if necessary. It tries to avoid making a copy
   * wherever possible.
   * </p>
   * 
   * @param vector a vector to copy.
   * 
   * @return either a reference to vector (if vector is already only an instance of
   *         {@link UnwritableVectorIJK}, otherwise an unwritable copy of vector's contents
   */
  public static UnwritableVectorIJK copyOf(UnwritableVectorIJK vector) {
    if (vector.getClass().equals(UnwritableVectorIJK.class)) {
      return vector;
    }
    return new UnwritableVectorIJK(vector);
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    long temp;
    temp = Double.doubleToLongBits(i);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(j);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(k);
    result = prime * result + (int) (temp ^ (temp >>> 32));
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
    if (!(obj instanceof UnwritableVectorIJK)) {
      return false;
    }
    final UnwritableVectorIJK other = (UnwritableVectorIJK) obj;
    if (Double.doubleToLongBits(i) != Double.doubleToLongBits(other.i)) {
      return false;
    }
    if (Double.doubleToLongBits(j) != Double.doubleToLongBits(other.j)) {
      return false;
    }
    if (Double.doubleToLongBits(k) != Double.doubleToLongBits(other.k)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "[" + i + "," + j + "," + k + "]";
  }

}
