package picante.math.vectorspace;

import static com.google.common.base.Preconditions.checkElementIndex;
import static picante.math.PicanteMath.PI;
import static picante.math.PicanteMath.asin;
import static picante.math.vectorspace.InternalOperations.computeNorm;
import static picante.units.FundamentalPhysicalConstants.HALFPI;
import picante.exceptions.BugException;

/**
 * A weakly immutable 2-dimensional vector designed to properly support a writable subclass.
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
 * 
 * This was a simple copy and paste of the {@link UnwritableVectorIJK} class.
 * 
 * @author G.K.Stephens
 */
public class UnwritableVectorIJ {

  /**
   * The ith component of the vector, synonymous with the &quot;X-axis&quot;.
   */
  protected double i;

  /**
   * The jth component of the vector, synonymous with the &quot;Y-axis&quot;.
   */
  protected double j;

  /**
   * Constructs a vector from the two basic components.
   * 
   * @param i the ith component
   * @param j the jth component
   */
  public UnwritableVectorIJ(double i, double j) {
    super();
    this.i = i;
    this.j = j;
  }

  /**
   * Constructs a vector from the first two elements of an array of doubles.
   * 
   * @param data the array of doubles
   * 
   * @throws IndexOutOfBoundsException if the supplied data array does not contain at least two
   *         elements
   */
  public UnwritableVectorIJ(double[] data) {
    this(data[0], data[1]);
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
  public UnwritableVectorIJ(int offset, double[] data) {
    this(data[offset], data[offset + 1]);
  }

  /**
   * Copy constructor, creates a vector by copying the values of a pre-existing one.
   * 
   * @param vector the vector whose contents are to be copied
   */
  public UnwritableVectorIJ(UnwritableVectorIJ vector) {
    this(vector.i, vector.j);
  }

  /**
   * Scaling constructor, creates a new vector by applying a scalar multiple to the components of a
   * pre-existing vector.
   * 
   * @param scale the scale factor to apply
   * @param vector the vector whose contents are to be scaled
   */
  public UnwritableVectorIJ(double scale, UnwritableVectorIJ vector) {
    this(scale * vector.i, scale * vector.j);
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
  public UnwritableVectorIJ createUnitized() {
    double norm = getLength();

    if (norm > 0.0) {
      return new UnwritableVectorIJ(1.0 / norm, this);
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
  public UnwritableVectorIJ createNegated() {
    return new UnwritableVectorIJ(-1.0, this);
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
   * Get the specified component of the vector.
   * 
   * @param index the index of the component to retrieve. 0 = ith, 1 = jth.
   * 
   * @return the value from the requested component
   * 
   * @throws IndexOutOfBoundsException if an invalid index, outside the range [0,1], is specified.
   */
  public final double get(int index) {
    checkElementIndex(index, 2, "component");
    switch (index) {
      case 0:
        return i;
      case 1:
        return j;
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
    return computeNorm(this.i, this.j);
  }

  /**
   * Compute the dot product of this instance with another vector.
   * 
   * @param vector the vector to dot against the instance.
   * 
   * @return i*vector.i + j*vector.j + k*vector.k
   */
  public double getDot(UnwritableVectorIJ vector) {
    return i * vector.i + j * vector.j;
  }

  /**
   * Compute the angular separation in radians between this instance and another vector.
   * 
   * @param vector
   * 
   * @return the angular separation between vector and this instance in radians.
   * 
   * @throws UnsupportedOperationException if either this instance or the supplied vector are
   *         {@link VectorIJ#ZERO}
   */
  public double getSeparation(UnwritableVectorIJ vector) {
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

      return 2.0 * asin(0.5 * computeNorm(x, y));
    } else if (dotProduct < 0) {
      double x = i / thisNorm + vector.i / vectorNorm;
      double y = j / thisNorm + vector.j / vectorNorm;

      return PI - 2.0 * asin(0.5 * computeNorm(x, y));
    }

    return HALFPI;
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
   *         {@link UnwritableVectorIJ}, otherwise an unwritable copy of vector's contents
   */
  public static UnwritableVectorIJ copyOf(UnwritableVectorIJ vector) {
    if (vector.getClass().equals(UnwritableVectorIJ.class)) {
      return vector;
    }
    return new UnwritableVectorIJ(vector);
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
    if (!(obj instanceof UnwritableVectorIJ)) {
      return false;
    }
    final UnwritableVectorIJ other = (UnwritableVectorIJ) obj;
    if (Double.doubleToLongBits(i) != Double.doubleToLongBits(other.i)) {
      return false;
    }
    if (Double.doubleToLongBits(j) != Double.doubleToLongBits(other.j)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "[" + i + "," + j + "]";
  }

}
