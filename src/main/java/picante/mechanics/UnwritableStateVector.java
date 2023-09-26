package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.abs;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * The unwritable parent of a simple container class that captures the position and velocity vector
 * components of a state vector.
 * <p>
 * The class is simply a container for two vectors, a state's position and it's time derivative.
 * This class provides functions to obtain unwritable views of the internal memory allocated by the
 * class.
 * </p>
 * <p>
 * Note: When you utilize the get methods on this class, you are receiving an actual unwritable view
 * of the actual buffers allocated in the class. As a direct consequence of this, if you are holding
 * onto a reference to the writable subclass, the contents of the buffer could change out from
 * underneath you. If this is a concern, then copy the contents of what you retrieved into memory
 * outside the control of this class.
 * </p>
 */
public class UnwritableStateVector {

  /**
   * The field containing the buffer that holds the position component of the state.
   */
  protected final VectorIJK position;

  /**
   * The field containing the buffer that holds the velocity component of the state.
   */
  protected final VectorIJK velocity;

  /**
   * Default constructor, useful for buffer creation.
   */
  private UnwritableStateVector() {
    this(new VectorIJK(VectorIJK.ZERO), new VectorIJK(VectorIJK.ZERO));
  }

  /**
   * Constructor that uses the supplied position and velocity references for its internals.
   * 
   * @param position the position to use internally
   * @param velocity the velocity to use internally
   */
  private UnwritableStateVector(VectorIJK position, VectorIJK velocity) {
    this.position = position;
    this.velocity = velocity;
  }

  /**
   * Creates an unwritable state vector.
   * 
   * @param position the position of one object relative to another.
   * 
   * @param velocity the time derivative of the supplied position.
   */
  public UnwritableStateVector(UnwritableVectorIJK position, UnwritableVectorIJK velocity) {
    this();
    this.position.setTo(position);
    this.velocity.setTo(velocity);
  }

  /**
   * Copy constructor.
   * 
   * @param state the state whose contents are to be copied.
   */
  public UnwritableStateVector(UnwritableStateVector state) {
    this();
    this.position.setTo(state.position);
    this.velocity.setTo(state.velocity);
  }

  /**
   * Creates an unwritable state vector from six double components.
   * 
   * @param i the ith component of the position
   * @param j the jth component of the position
   * @param k the kth component of the position
   * @param di the ith component of the velocity
   * @param dj the jth component of the velocity
   * @param dk the kth component of the velocity
   */
  public UnwritableStateVector(double i, double j, double k, double di, double dj, double dk) {
    this();
    this.position.setTo(i, j, k);
    this.velocity.setTo(di, dj, dk);
  }

  /**
   * Creates a negated copy of the instance.
   * 
   * @return a newly created unwritable state that is equivalent to -this
   */
  public UnwritableStateVector createNegated() {
    return new UnwritableStateVector(position.createNegated(), velocity.createNegated());
  }

  /**
   * Creates a unitized copy of the instance.
   * 
   * @return a newly created unwritable state that is equivalent to the unitized instance
   */
  public UnwritableStateVector createUnitized() {
    UnwritableStateVector unitized = new UnwritableStateVector(this);
    unitize(unitized);
    return unitized;
  }

  /**
   * Get the position component.
   * 
   * @return an unwritable view of the position component.
   */
  public UnwritableVectorIJK getPosition() {
    return position;
  }

  /**
   * Get the velocity component.
   * 
   * @return an unwritable view of the velocity component.
   */
  public UnwritableVectorIJK getVelocity() {
    return velocity;
  }

  /**
   * Convenience method that computes the angular separation of the two position components of the
   * state vector.
   * 
   * @param state the state to compute the angular separation with
   * 
   * @return the angular separation of the position components expressed in radians
   */
  public double getSeparation(UnwritableStateVector state) {
    return this.position.getSeparation(state.position);
  }

  /**
   * Convenience method that computes the dot product of the two position components of the state
   * vector.
   * 
   * @param state the state to compute the position dot product with
   * 
   * @return &lt; this.getPosition(), state.getPosition &gt;
   */
  public double getDot(UnwritableStateVector state) {
    return this.position.getDot(state.position);
  }

  /**
   * Convenience method that computes the length of the position components of the state vector.
   * 
   * @return the length of the position vector components.
   */
  public double getLength() {
    return this.position.getLength();
  }

  /**
   * Computes the derivative of the dot product of this state vector with another.
   * <p>
   * The dot product can be computed by retrieving the position vectors directly and applying
   * {@link VectorIJK#getDot(UnwritableVectorIJK)} or using the convenience method:
   * {@link UnwritableStateVector#getDot(UnwritableStateVector)} that does this for you.
   * </p>
   * 
   * @param state the state vector to compute the dot product with
   * 
   * @return the derivative of the dot product of the two position components of the instance and
   *         the supplied state
   */
  public double getDotDerivative(UnwritableStateVector state) {
    return this.position.getDot(state.velocity) + this.velocity.getDot(state.position);
  }

  /**
   * Computes the derivative of the angular separation of this state vector with another.
   * 
   * @param state the state vector to retrieve the angular separation derivative.
   * 
   * @return the derivative of the angular separation of the position components
   */
  public double getSeparationDerivative(UnwritableStateVector state) {

    /*
     * Create two work buffers to hold unitized versions of the two states involved. We are going to
     * mutate these, but they'll never leave this method.
     */
    UnwritableStateVector uThis = new UnwritableStateVector(this);
    unitize(uThis);
    UnwritableStateVector uState = new UnwritableStateVector(state);
    unitize(uState);

    /*
     * Compute the cross product of the position components of the unitized vectors.
     */
    VectorIJK pCross = VectorIJK.cross(uThis.position, uState.position);

    /*
     * Now compute the time derivative of the angular separation between the two states.
     * 
     * This methods needs to prevent division by zero and numeric overflow.
     */
    if (pCross.equals(VectorIJK.ZERO)) {
      return 0.0;
    }

    double numerator =
        uThis.position.getDot(uState.velocity) + uThis.velocity.getDot(uState.position);
    double denominator = pCross.getLength();

    /*
     * Check for numeric overflow, a factor of 10.0 should be reasonable enough of a guard against
     * it.
     */
    checkArgument(denominator > 10.0 * abs(numerator) / Double.MAX_VALUE,
        "Derivative computation overflows numerically.");

    return -numerator / denominator;

  }

  /**
   * Computes the derivative of the {@link UnwritableStateVector#getLength()} operation.
   * <p>
   * This method computes the value of the derivative of the norm of the position components of the
   * state vector utilizing the velocity components like so:
   * 
   * <pre>
   *           d||x||   < x, v >  
   *           ------ =  ------     =  < xhat, v >
   *             ds            1/2
   *                    < x, x >
   * 
   * </pre>
   * 
   * where,
   * 
   * <pre>
   * 
   *                           1/2         2    2    2  1/2
   *          ||x|| = < x, x >    =  ( x1 + x2 + x3 )
   *         
   *              v = ( dx1, dx2, dx3 )
   *                    ---  ---  ---
   *                    ds   ds   ds
   * </pre>
   * 
   * </p>
   * 
   * @return the derivative of the length
   */
  public double getLengthDerivative() {
    if (position.getLength() == 0.0) {
      throw new UnsupportedOperationException(
          "Length derivative of state with zero length position is not defined.");
    }
    VectorIJK xhat = new VectorIJK(this.position).unitize();
    return this.velocity.getDot(xhat);
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
   *         {@link UnwritableStateVector}, otherwise an unwritable copy of vector's contents
   */
  public static UnwritableStateVector copyOf(UnwritableStateVector vector) {
    if (vector.getClass().equals(UnwritableStateVector.class)) {
      return vector;
    }
    return new UnwritableStateVector(vector);
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((position == null) ? 0 : position.hashCode());
    result = prime * result + ((velocity == null) ? 0 : velocity.hashCode());
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
    if (!(obj instanceof UnwritableStateVector)) {
      return false;
    }
    UnwritableStateVector other = (UnwritableStateVector) obj;
    if (position == null) {
      if (other.position != null) {
        return false;
      }
    } else if (!position.equals(other.position)) {
      return false;
    }
    if (velocity == null) {
      if (other.velocity != null) {
        return false;
      }
    } else if (!velocity.equals(other.velocity)) {
      return false;
    }
    return true;
  }

  @Override
  public final String toString() {
    return "[" + position.getI() + "," + position.getJ() + "," + position.getK() + "; "
        + velocity.getI() + "," + velocity.getJ() + "," + velocity.getK() + "]";
  }

  /**
   * Convenience method that unitizes a state vector. Note: this mutates the state vector in place,
   * which is a violation of the unwritable state vector contract. This should only be utilized with
   * mutable vectors or for buffer instances created in this class as workspaces only.
   */
  static void unitize(UnwritableStateVector stateToUnitize) {
    unitizeAsState(stateToUnitize.position, stateToUnitize.velocity);
  }

  /**
   * Method that unitizes in place, the position and velocity components of a state vector.
   * 
   * @param position the position component of the state
   * @param velocity the velocity component of the state
   */
  static void unitizeAsState(VectorIJK position, VectorIJK velocity) {
    double norm = position.getLength();

    if (norm == 0.0) {
      throw new UnsupportedOperationException(
          "Unable to unitize position. Instance is zero length.");
    }

    position.scale(1.0 / norm);
    VectorIJK.planeProject(velocity, position, velocity);
    velocity.scale(1.0 / norm);
  }

}
