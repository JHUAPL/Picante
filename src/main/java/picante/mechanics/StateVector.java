package picante.mechanics;

import picante.designpatterns.Writable;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * The writable child of a container class capturing the position and velocity vector components of
 * a state.
 * <p>
 * The fields containing the actual data are defined in the parent class, per the usual weak
 * immutability pattern.
 * </p>
 */
public class StateVector extends UnwritableStateVector
    implements Writable.ImplementationInterface<UnwritableStateVector, StateVector> {

  /**
   * The zero state vector: zero position and zero velocity components.
   */
  public static final UnwritableStateVector ZERO =
      new UnwritableStateVector(VectorIJK.ZERO, VectorIJK.ZERO);

  /**
   * Default constructor, creates a zero state vector.
   */
  public StateVector() {
    this(ZERO);
  }

  /**
   * Creates a state vector.
   * 
   * @param position the position of one body relative to another.
   * 
   * @param velocity the velocity, the time derivative of the supplied position.
   */
  public StateVector(UnwritableVectorIJK position, UnwritableVectorIJK velocity) {
    super(position, velocity);
  }

  /**
   * Copy constructor.
   * 
   * @param state the state whose contents are to be copied.
   */
  public StateVector(UnwritableStateVector state) {
    super(state);
  }

  /**
   * Creates a state vector from six double components.
   * 
   * @param i the ith component of the position
   * @param j the jth component of the position
   * @param k the kth component of the position
   * @param di the ith component of the velocity
   * @param dj the jth component of the velocity
   * @param dk the kth component of the velocity
   */
  public StateVector(double i, double j, double k, double di, double dj, double dk) {
    super(i, j, k, di, dj, dk);
  }

  @Override
  public StateVector createNegated() {
    StateVector result = new StateVector(this);
    result.negate();
    return result;
  }

  @Override
  public StateVector createUnitized() {
    StateVector result = new StateVector(this);
    result.unitize();
    return result;
  }

  /**
   * Retrieves a writable view of the position component. Changes to the returned reference's state
   * will be captured here.
   * 
   * @return the position component
   */
  @Override
  public final VectorIJK getPosition() {
    return this.position;
  }

  /**
   * Retrieves a writable view of the velocity component. Changes to the returned reference's state
   * will be captured here.
   * 
   * @return the velocity component
   */
  @Override
  public final VectorIJK getVelocity() {
    return this.velocity;
  }

  /**
   * Set the position component of the state vector.
   * 
   * @param position the position whose contents are copied into memory local to the instance
   */
  public final void setPosition(UnwritableVectorIJK position) {
    this.position.setTo(position);
  }

  /**
   * Set the velocity component of the state vector.
   * 
   * @param velocity the velocity whose contents are copied into memory local to the instance
   */
  public final void setVelocity(UnwritableVectorIJK velocity) {
    this.velocity.setTo(velocity);
  }

  /**
   * Set the contents of this state vector to the supplied state vector.
   * 
   * @param state a state which is to have its contents copied to the memory local to the instance
   * 
   * @return a reference to the instance for convenience
   */
  @Override
  public final StateVector setTo(UnwritableStateVector state) {
    position.setTo(state.position);
    velocity.setTo(state.velocity);
    return this;
  }

  /**
   * Set the contents of the state vector to the zero state.
   * 
   * @return a reference to the instance for convenience
   */
  public StateVector clear() {
    position.setTo(VectorIJK.ZERO);
    velocity.setTo(VectorIJK.ZERO);
    return this;
  }

  /**
   * Negate the two vector components of the instance.
   * 
   * @return a reference to the instance for convenience
   */
  public StateVector negate() {
    position.negate();
    velocity.negate();
    return this;
  }

  /**
   * Unitize the position vector and make the appropriate adjustment to the velocity derivative
   * components.
   * 
   * @return a reference to the instance, now containing the unitized vector and appropriately
   *         adjusted derivative components
   * 
   * @throws UnsupportedOperationException if the position vector is equal to {@link VectorIJK#ZERO}
   */
  public StateVector unitize() {
    UnwritableStateVector.unitizeAsState(position, velocity);
    return this;
  }

  /**
   * Subtract one state vector from another.
   * 
   * @param a the minuend
   * @param b the subtrahend
   * @param buffer the buffer to receive the results of the subtraction
   * 
   * @return a reference to buffer for convenience which now contains (a - b)
   */
  public static StateVector subtract(UnwritableStateVector a, UnwritableStateVector b,
      StateVector buffer) {
    VectorIJK.subtract(a.position, b.position, buffer.position);
    VectorIJK.subtract(a.velocity, b.velocity, buffer.velocity);
    return buffer;
  }

  /**
   * Subtract one state vector from another.
   * 
   * @param a the minuend
   * @param b the subtrahend
   * 
   * @return a reference to the newly created {@link StateVector} that contains (a - b)
   */
  public static StateVector subtract(UnwritableStateVector a, UnwritableStateVector b) {
    return subtract(a, b, new StateVector());
  }

  /**
   * Add two state vectors.
   * 
   * @param a a state vector
   * @param b another state vector
   * @param buffer the buffer to receive the results of the addition
   * 
   * @return a reference to buffer for convenience which now contains (a + b).
   */
  public static StateVector add(UnwritableStateVector a, UnwritableStateVector b,
      StateVector buffer) {
    VectorIJK.add(a.position, b.position, buffer.position);
    VectorIJK.add(a.velocity, b.velocity, buffer.velocity);
    return buffer;
  }

  /**
   * Add two state vectors.
   * 
   * @param a a state vector
   * @param b another state vector
   * @param buffer the buffer to receive the results of the addition
   * 
   * @return a reference to the newly created {@link StateVector} which now contains (a + b).
   */
  public static StateVector add(UnwritableStateVector a, UnwritableStateVector b) {
    return add(a, b, new StateVector());
  }

  /**
   * Adds all the state vectors in an {@link Iterable} of state vectors.
   * 
   * @param vectors an {@link Iterable} of state vectors to be added
   * @param buffer the buffer to receive the results of the addition
   * 
   * @return a reference to buffer for convenience which now contains (a + b + ... + n).
   */
  public static StateVector addAll(Iterable<? extends UnwritableStateVector> vectors,
      StateVector buffer) {

    double sumI = 0.0;
    double sumJ = 0.0;
    double sumK = 0.0;

    double sumDI = 0.0;
    double sumDJ = 0.0;
    double sumDK = 0.0;

    for (UnwritableStateVector vector : vectors) {
      sumI += vector.position.getI();
      sumJ += vector.position.getJ();
      sumK += vector.position.getK();

      sumDI += vector.velocity.getI();
      sumDJ += vector.velocity.getJ();
      sumDK += vector.velocity.getK();
    }

    buffer.getPosition().setTo(sumI, sumJ, sumK);
    buffer.getVelocity().setTo(sumDI, sumDJ, sumDK);

    return buffer;
  }

  /**
   * Adds all the state vectors in an {@link Iterable} of state vectors.
   * 
   * @param vectors an {@link Iterable} of state vectors to be added
   * 
   * @return a new {@link StateVector} for convenience which now contains (a + b + ... + n).
   * 
   * @see StateVector#addAll(Iterable, StateVector)
   */
  public static StateVector addAll(Iterable<? extends UnwritableStateVector> vectors) {
    return addAll(vectors, new StateVector());
  }

  /**
   * Computes the cross product and its derivative of two state vectors a and b.
   * 
   * @param a the left hand state vector to cross
   * @param b the right hand state vector to cross
   * 
   * @return a new {@link StateVector} which now contains (a x b) and its derivative
   */
  public static StateVector cross(UnwritableStateVector a, UnwritableStateVector b) {
    return cross(a, b, new StateVector());
  }

  /**
   * Computes the cross product and its derivative of two state vectors a and b.
   * 
   * @param a the left hand state vector to cross
   * @param b the right hand state vector to cross
   * 
   * @return a reference to buffer for convenience which now contains (a x b) and its derivative
   */
  public static StateVector cross(UnwritableStateVector a, UnwritableStateVector b,
      StateVector buffer) {

    /*
     * Start by computing the cross product of the position components of a and b. We have to do
     * this into local double precision buffers, as we explicitly permit buffer to be the same as a
     * or b.
     */
    double ti = a.position.getJ() * b.position.getK() - a.position.getK() * b.position.getJ();
    double tj = a.position.getK() * b.position.getI() - a.position.getI() * b.position.getK();
    double tk = a.position.getI() * b.position.getJ() - a.position.getJ() * b.position.getI();

    double vi = a.velocity.getJ() * b.position.getK() - a.velocity.getK() * b.position.getJ();
    double vj = a.velocity.getK() * b.position.getI() - a.velocity.getI() * b.position.getK();
    double vk = a.velocity.getI() * b.position.getJ() - a.velocity.getJ() * b.position.getI();

    vi += a.position.getJ() * b.velocity.getK() - a.position.getK() * b.velocity.getJ();
    vj += a.position.getK() * b.velocity.getI() - a.position.getI() * b.velocity.getK();
    vk += a.position.getI() * b.velocity.getJ() - a.position.getJ() * b.velocity.getI();

    buffer.position.setTo(ti, tj, tk);
    buffer.velocity.setTo(vi, vj, vk);

    return buffer;

  }

  /**
   * Compute the unitized cross product of a and b and its derivative.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * 
   * @return a newly created {@link StateVector} that contains (a x b)/||a||/||b||
   * 
   * @throws UnsupportedOperationException if the resultant cross product results in the zero vector
   */
  public static StateVector uCross(UnwritableStateVector a, UnwritableStateVector b) {
    return uCross(a, b, new StateVector());
  }

  /**
   * Compute the unitized cross product of a and b and its derivative.
   * 
   * @param a the left hand vector to cross
   * @param b the right hand vector to cross
   * @param buffer a buffer to receive the contents of the unitized cross product and its derivative
   * 
   * @return a reference to buffer, for convenience, which now contains (a x b)/||a||/||b|| and its
   *         derivative
   * 
   * @throws UnsupportedOperationException
   */
  public static StateVector uCross(UnwritableStateVector a, UnwritableStateVector b,
      StateVector buffer) {

    // TODO: NAIF's code for manipulating state vector's first scales both a and b by the largest of
    // their individual components for numerical reasons. We're not doing that here, should we?
    return cross(a, b, buffer).unitize();
  }

  /**
   * Linearly combine two state vectors.
   * 
   * @param scaleA the scale factor for state vector a
   * @param a a vector
   * @param scaleB the scale factor for state vector b
   * @param b another vector
   * 
   * @return a newly created <code>StateVector</code> which now contains ( scaleA*a + scaleB*b ).
   * 
   * @see StateVector#combine(double, UnwritableStateVector, double, UnwritableStateVector,
   *      StateVector)
   */
  public static StateVector combine(double scaleA, UnwritableStateVector a, double scaleB,
      UnwritableStateVector b) {
    return combine(scaleA, a, scaleB, b, new StateVector());
  }

  /**
   * Linearly combine two state vectors.
   * 
   * @param scaleA the scale factor for state vector a
   * @param a a vector
   * @param scaleB the scale factor for state vector b
   * @param b another vector
   * @param buffer the buffer to receive the results of the combination
   * 
   * @return a reference to buffer for convenience which now contains ( scaleA*a + scaleB*b )
   */
  public static StateVector combine(double scaleA, UnwritableStateVector a, double scaleB,
      UnwritableStateVector b, StateVector buffer) {
    VectorIJK.combine(scaleA, a.getPosition(), scaleB, b.getPosition(), buffer.getPosition());
    VectorIJK.combine(scaleA, a.getVelocity(), scaleB, b.getVelocity(), buffer.getVelocity());
    return buffer;
  }



}
