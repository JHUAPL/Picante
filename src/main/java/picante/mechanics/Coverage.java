package picante.mechanics;

import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

/**
 * Interface describing the time coverage of mechanics data sources.
 * <p>
 * The mechanics framework does not specify precisely what the double precision &quot;time&quot;
 * input means. It is largely up to the user of the framework to decide and load sources into it
 * that are all using a consistent, matching definition. In general the implementations provided by
 * the crucible library should be utilizing seconds past J2000.0 expressed in barycentric dynamical
 * time, or TDB. This is consistent with the SPICE Toolkit, among other JPL navigation products.
 * </p>
 * <p>
 * Implementors of this interface should read the specification carefully, as consistency of the
 * implementation is necessary for proper functioning of the mechanics framework. There are several
 * useful implementations of the coverage interface exposed in the the {@link Coverages} static
 * utility class. It maybe useful to start there before attempting to implement this interface
 * yourself. The reason this interface exists is to enable implementors of ephemeris and frame
 * sources completely freedom with regards to how requests for coverage information are satisfied:
 * both in terms of memory and performance. If the {@link IntervalSet} class meets your coverage
 * implementation needs, then have a look at {@link Coverages#create(IntervalSet)}; it will save you
 * the trouble of implementing this interface.
 * </p>
 * <p>
 * Further implementors are strongly encouraged to override equals and hashCode and utilize the
 * static methods {@link Coverages#equalsImplementation(Coverage, Object)} and
 * {@link Coverages#hashCodeImplementation(Coverage)}
 * </p>
 * <p>
 * With regards to mutability: the coverage API does not provide any sort of listener mechanism for
 * notification if a coverage instance undergoes a change. It was largely intended for
 * implementations to be invariant through an application run, and some of the mechanics framework
 * software may depend on this fact (caching values for performance enhancement, etc.). That said,
 * there is no specific restriction that the implementation be invariant over time. Code in the
 * framework that relies upon this fact should document it clearly, and obviously.
 * </p>
 * <p>
 * The exceptions allowed by this interface are all runtime exceptions, and as such are not
 * enforceable. However, as an implementor it is your responsibility to ensure that the methods you
 * provide here throw the appropriate runtime exceptions. In general the framework does not rely on
 * the specific exception, but it's still a good idea to throw the appropriate type.
 * </p>
 * <p>
 * TODO: Sort this out: There is an efficiency issue latent in the implementation of this interface
 * and its ultimate conversion to an {@link IntervalSet} or {@link Iterable} of
 * {@link UnwritableInterval}. The standard way an iteration over the intervals of coverage is
 * performed is through the {@link Coverage#hasNextInterval(double)} and
 * {@link Coverage#getNextInterval(double, Interval)} methods. Implementors of these methods are
 * left with no choice but to perform a search as they do not know an iteration is being performed,
 * so we end up with something that is O(n*log(n)) assuming binary search on the implementation
 * side. If a future version of this interface requires it implements {@link Iterable} we may be
 * better off, but it could result in more complicated implementations. For now I'm leaving this as
 * it is, but it is an issue that should be sorted out and locked down.
 * </p>
 */
public interface Coverage {

  /**
   * Coverage indicating all possible times.
   */
  public static final Coverage ALL_TIME = new Coverage() {

    @Override
    public boolean contains(@SuppressWarnings("unused") double time) {
      return true;
    }

    @Override
    public Interval getBoundingInterval(Interval buffer) {
      buffer.set(-Double.MAX_VALUE, Double.MAX_VALUE);
      return buffer;
    }

    @Override
    public Interval getBracketingInterval(@SuppressWarnings("unused") double time,
        Interval buffer) {
      buffer.set(-Double.MAX_VALUE, Double.MAX_VALUE);
      return buffer;
    }

    @Override
    public boolean hasNextInterval(@SuppressWarnings("unused") double time) {
      return false;
    }

    @Override
    public Interval getNextInterval(@SuppressWarnings("unused") double time,
        @SuppressWarnings("unused") Interval buffer) {
      throw new TimeOutOfBoundsException(
          "This schedule covers all valid times, so there is only one interval.");
    }

    @Override
    public boolean equals(Object object) {
      return Coverages.equalsImplementation(this, object);
    }

    @Override
    public int hashCode() {
      return Coverages.hashCodeImplementation(this);
    }

  };

  /**
   * Determines if the coverage of the data source contains the requested time.
   * 
   * @param time the time of interest
   * 
   * @return true if the coverage contains time, false otherwise
   */
  public boolean contains(double time);

  /**
   * Retrieve the interval that bounds the entire coverage. In general the start of the bounding
   * interval must be equal to (or at worst precede) the start of the first interval of coverage.
   * Similarly the end of the bounding interval must be equal to (or at worst follow) the end of the
   * final interval of coverage.
   * 
   * @param buffer a buffer to receive the time interval.
   * 
   * @return a reference to buffer for chaining convenience
   */
  public Interval getBoundingInterval(Interval buffer);

  /**
   * Obtain an interval, of contiguous coverage, that brackets the requested time and is contained
   * entirely within the coverage of the instance.
   * 
   * @param time the time of interest
   * @param buffer a buffer to receive the bracketing interval
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws TimeOutOfBoundsException if the supplied time argument lies outside an interval
   *         contained in the coverage
   */
  public Interval getBracketingInterval(double time, Interval buffer);

  /**
   * Determines whether an additional interval of coverage follows the supplied time.
   * 
   * @param time the time of interest
   * 
   * @return true, if {@link Coverage#getNextInterval(double, Interval)} will succeed with time as
   *         it's argument; false otherwise.
   */
  public boolean hasNextInterval(double time);

  /**
   * Obtain the next interval of coverage that follows the supplied time.
   * 
   * @param time the time of interest
   * @param buffer a buffer to receive the bracketing interval
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws TimeOutOfBoundsException if the supplied time argument lies within or after the last
   *         actual interval of coverage supported by the instance.
   */
  public Interval getNextInterval(double time, Interval buffer);

  /**
   * {@inheritDoc}
   * 
   * This method should be implemented, and users are strongly encouraged to utilize:
   * {@link Coverages#equalsImplementation(Coverage, Object)}.
   */
  @Override
  public boolean equals(Object object);

  /**
   * {@inheritDoc}
   * 
   * This method should be implemented, and users are strongly encouraged to utilize:
   * {@link Coverages#hashCodeImplementation(Coverage)}
   */
  @Override
  public int hashCode();

}
