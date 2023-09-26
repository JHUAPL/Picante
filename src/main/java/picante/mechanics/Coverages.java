package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;
import static picante.math.PicanteMath.min;
import static picante.math.PicanteMath.ulp;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.MoreObjects;
import com.google.common.base.MoreObjects.ToStringHelper;
import com.google.common.collect.UnmodifiableIterator;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;

/**
 * Collection of static utility methods for working with implementations of the {@link Coverage}
 * interface.
 */
public class Coverages {

  /**
   * Lock up the constructor as this is a static utility method.
   */
  private Coverages() {}

  /**
   * Adapts an {@link IntervalSet} to a {@link Coverage}.
   * 
   * @param set the set to adapt
   * 
   * @return an implementation of the interface utilizing the contents of the interval set.
   * 
   * @throws IllegalArgumentException if the supplied set is empty. Coverages must contain at least
   *         one point of validity.
   */
  public static Coverage create(IntervalSet set) {
    return new IntervalSetCoverage(set);
  }

  public static Interval getFirstInterval(Coverage coverage, Interval buffer) {

    /*
     * This is a bit tricky, mainly because of how the Coverage interface method semantics are
     * defined. We know the individual intervals within coverage must be a subset of the bounding
     * interval.
     */
    coverage.getBoundingInterval(buffer);

    double start = buffer.getBegin();

    /*
     * However, we do not know whether it is a proper subset or not. Determine if that is the case
     * first.
     */
    if (coverage.contains(start)) {
      return coverage.getBracketingInterval(start, buffer);
    }

    /*
     * If we reach here, then the individual intervals in the coverage, particularly the first are
     * proper subsets of the bounding interval. Fetch the next interval.
     */
    return coverage.getNextInterval(start, buffer);
  }

  /**
   * Returns the tight boundary for the supplied coverage. Coverage implementations are allowed to
   * supply a loose boundary in the {@link Coverage#getBoundingInterval(Interval)} method. While, in
   * general, this is a bad idea for performance reasons it is permitted by the interface. This
   * method extracts the tight bounds of the supplied coverage.
   * 
   * @param coverage
   * @param buffer
   * 
   * @return a reference to buffer for convenience
   */
  public static Interval tightBoundary(Coverage coverage, Interval buffer) {

    coverage.getBoundingInterval(buffer);

    double end = buffer.getEnd();
    double begin = buffer.getBegin();

    boolean containsBegin = coverage.contains(begin);
    boolean containsEnd = coverage.contains(end);

    /*
     * Simple case is that the boundary is contained in the coverage.
     */
    if (containsBegin && containsEnd) {
      return buffer;
    }

    /*
     * At this point we have to tighten one end of the boundary or both. The beginning is simple,
     * just pop the next interval out of the coverage instance.
     */
    if (!containsBegin) {
      begin = coverage.getNextInterval(begin, buffer).getBegin();
    }

    /*
     * This isn't too helpful, as the only way to handle this is to iterate through all of the
     * intervals in the coverage instance.
     */
    if (!containsEnd) {
      double time = begin - ulp(begin);

      while (coverage.hasNextInterval(time)) {
        coverage.getNextInterval(time, buffer);
        time = buffer.getEnd();
      }
      end = time;
    }

    buffer.set(begin, end);
    return buffer;
  }

  /**
   * Captures a snapshot of the supplied coverage into an interval set.
   * 
   * @param coverage the coverage to copy
   * 
   * @return a newly created interval set containing the intervals of coverage specified by coverage
   */
  public static IntervalSet snapshotToIntervalSet(Coverage coverage) {

    /*
     * Don't do the work unless it's absolutely necessary.
     */
    if (coverage instanceof IntervalSetCoverage) {
      return ((IntervalSetCoverage) coverage).getSet();
    }

    IntervalSet.Builder builder = IntervalSet.builder();

    Interval buffer = new Interval();

    /*
     * Fetch the first interval, then bootstrap our way to the end using the usual hasNext getNext
     * mantra.
     */
    builder.add(getFirstInterval(coverage, buffer));

    double time = buffer.getEnd();

    while (coverage.hasNextInterval(time)) {
      builder.add(coverage.getNextInterval(time, buffer));
      time = buffer.getEnd();
    }

    return builder.build();
  }

  /**
   * Creates an implementation of the coverage interface that captures a snapshot of the supplied
   * interval.
   * 
   * @param interval the interval from which a coverage is to be created
   * 
   * @return a newly created coverage instance capturing the supplied interval
   */
  public static Coverage createSnapshot(UnwritableInterval interval) {
    return new IntervalCoverage(interval);
  }

  /**
   * Creates an implementation of the coverage interface that captures the specified interval
   * 
   * @param begin the start of the coverage region
   * @param end the end of the coverage region
   * 
   * @return a newly created coverage instance that captures the interval [begin,end].
   * 
   * @throws IllegalArgumentException if end < begin
   */
  public static Coverage create(double begin, double end) {
    return new IntervalCoverage(begin, end);
  }

  /**
   * Tests if a coverage instance consists of a single interval of coverage.
   * 
   * @param coverage the coverage instance to test
   * 
   * @return true if the coverage instance consists of a single interval of coverage, false
   *         otherwise. Note: the interval may be a singleton.
   */
  public static boolean isSingleInterval(Coverage coverage) {

    /*
     * Handle two trivial cases first.
     */
    if (coverage instanceof IntervalCoverage) {
      return true;
    }
    if (coverage instanceof IntervalSetCoverage) {
      return ((IntervalSetCoverage) coverage).getSet().size() == 1;
    }

    Interval buffer = new Interval();

    getFirstInterval(coverage, buffer);
    return !coverage.hasNextInterval(buffer.getEnd());
  }

  /**
   * Wraps the coverage instance into an {@link Iterable}. If coverage mutates while an iteration is
   * on-going, then the behavior of the resultant iterator is unspecified.
   * 
   * @param coverage the coverage to wrap
   * 
   * @return a newly created {@link Iterable} that produces intervals from coverage
   */
  public static Iterable<UnwritableInterval> iterable(final Coverage coverage) {

    /*
     * Take the obvious short cut, if we can.
     */
    if (coverage instanceof IntervalSetCoverage) {
      return ((IntervalSetCoverage) coverage).getSet();
    }

    return new Iterable<UnwritableInterval>() {
      @Override
      public Iterator<UnwritableInterval> iterator() {
        return new UnmodifiableIterator<UnwritableInterval>() {
          private final Interval buffer = getFirstInterval(coverage, new Interval());
          private boolean hasNextInterval = true;

          @Override
          public boolean hasNext() {
            return hasNextInterval;
          }

          @Override
          public UnwritableInterval next() {

            /*
             * We're going to return the interval currently captured in buffer, if hasNextInterval
             * is true.
             */
            if (hasNextInterval) {
              hasNextInterval = coverage.hasNextInterval(buffer.getEnd());
              UnwritableInterval result = new UnwritableInterval(buffer);

              if (hasNextInterval) {
                coverage.getNextInterval(buffer.getEnd(), buffer);
              }

              return result;
            }

            throw new NoSuchElementException("No element remains in the iterator.");
          }
        };
      }
    };
  }

  /**
   * Determines whether or not the coverage instance contains the supplied interval.
   * 
   * @param coverage the coverage to interrogate
   * @param interval the interval to consider for inclusion
   * 
   * @return true if interval is completely contained within coverage, false otherwise
   */
  public static boolean containsInterval(Coverage coverage, UnwritableInterval interval) {
    return containsInterval(coverage, interval, new Interval());
  }

  /**
   * Determines whether or not the coverage instance contains the supplied interval. This method is
   * here to allow the caller to exercise control of the allocation of the working buffer.
   * 
   * @param coverage the coverage to interrogate
   * @param interval the interval to consider for inclusion
   * @param workBuffer an interval working buffer to utilize internally
   * 
   * @return true if interval is completely contained within coverage, false otherwise
   */
  @VisibleForTesting
  static boolean containsInterval(Coverage coverage, UnwritableInterval interval,
      Interval workBuffer) {

    if (!coverage.contains(interval.getBegin())) {
      return false;
    }

    coverage.getBracketingInterval(interval.getBegin(), workBuffer);
    return workBuffer.closedContains(interval);

  }

  /**
   * Determines whether or not the coverage instance contains the specified interval.
   * 
   * @param coverage the coverage to interrogate
   * @param begin the beginning of the interval to consider for inclusion
   * @param end the end of the interval to consider for inclusion
   * 
   * @return true, if coverage contains [begin,end] entirely, otherwise false
   * 
   * @throws IllegalArgumentException if begin &gt; end
   */
  public static boolean containsInterval(Coverage coverage, double begin, double end) {
    checkArgument(begin <= end);

    if (!coverage.contains(begin)) {
      return false;
    }

    Interval interval = coverage.getBracketingInterval(begin, new Interval());
    return interval.closedContains(begin, end);
  }

  /**
   * Determines whether one coverage is completely contained within another.
   * 
   * @param container a coverage instance
   * @param coverage another coverage instance to test for containment in container
   * 
   * @return true if coverage is completely contained within container, false otherwise
   */
  public static boolean containsAll(Coverage container, Coverage coverage) {

    Interval buffer = new Interval();
    Interval containing = new Interval();

    /*
     * Treat the first interval separately due to the odd way Coverage works.
     */
    getFirstInterval(coverage, buffer);

    if (!containsInterval(container, buffer, containing)) {
      return false;
    }

    double time = buffer.getEnd();

    while (coverage.hasNextInterval(time)) {
      coverage.getNextInterval(time, buffer);
      if (!containsInterval(container, buffer, containing)) {
        return false;
      }
      time = buffer.getEnd();
    }
    return true;

  }

  /**
   * Determines whether one coverage is completely contained within another over the specified
   * boundary.
   * 
   * @param boundary the boundary over which to consider the containment
   * @param container a coverage instance
   * @param coverage another coverage instance to test for containment in container over boundary
   * 
   * @return true for all elements of coverage that intersect boundary are contained within
   *         container; false otherwise
   */
  public static boolean containsAllWithin(UnwritableInterval boundary, Coverage container,
      Coverage coverage) {

    Interval buffer = new Interval();
    Interval containing = new Interval();

    /*
     * Before jumping into the loop, check to see if the start of the boundary is contained within
     * coverage. Handle that special case first.
     */
    if (coverage.contains(boundary.getBegin())) {
      coverage.getBracketingInterval(boundary.getBegin(), buffer);

      /*
       * Constrain buffer to lie within boundary, if necessary.
       */
      if (buffer.getBegin() < boundary.getBegin()) {
        buffer.set(boundary.getBegin(), buffer.getEnd());
      }
      if (buffer.getEnd() > boundary.getEnd()) {
        buffer.set(buffer.getBegin(), boundary.getEnd());
      }

      if (!containsInterval(container, buffer, containing)) {
        return false;
      }
    }

    /*
     * If we reach here, start locating intervals present in coverage that occur strictly after the
     * start of the boundary. We have already handled any intervals that include the start of the
     * boundary.
     */
    double time = boundary.getBegin();

    while (coverage.hasNextInterval(time)) {
      coverage.getNextInterval(time, buffer);

      /*
       * At this point there are three cases that arise:
       * 
       * (1) buffer is contained entirely within boundary
       * 
       * (2) buffer splits across the end of boundary
       * 
       * (3) buffer follows boundary entirely
       */
      if (buffer.getBegin() > boundary.getEnd()) {
        /*
         * Case #3: We've made it this far, then container must include all of coverage within
         * boundary.
         */
        return true;
      }

      if (buffer.getEnd() >= boundary.getEnd()) {
        /*
         * Case #2: Buffer is split across the end of the bounding interval. Just update the end of
         * buffer and test containment.
         */
        buffer.set(buffer.getBegin(), boundary.getEnd());
      }

      /*
       * Case #1 and #2 (after adjusting buffer) are handled by the code below.
       */
      if (!containsInterval(container, buffer, containing)) {
        return false;
      }
      time = buffer.getEnd();
    }

    return true;
  }

  /**
   * Creates a new coverage instance that is the union of the supplied coverages.
   * <p>
   * TODO: As implemented, the instance returned by this method is horribly inefficient.
   * </p>
   * 
   * @param a a coverage instance
   * @param others an array of other coverage instances
   * 
   * @return a derived coverage implementation that captures the union of all the supplied coverages
   */
  public static Coverage union(final Coverage a, final Coverage... others) {

    /*
     * TODO: Check to see if the inputs are all instances of IntervalCoverage or
     * IntervalSetCoverage. We can greatly accelerate the performance of the resultant coverage if
     * this is the case.
     */

    return new AbstractCoverage() {

      private Coverage createCoverage() {
        IntervalSet.Builder builder = IntervalSet.builder(Coverages.snapshotToIntervalSet(a));
        for (Coverage c : others) {
          builder.union(Coverages.snapshotToIntervalSet(c));
        }
        return Coverages.create(builder.build());
      }

      @Override
      public boolean hasNextInterval(double time) {
        return createCoverage().hasNextInterval(time);
      }

      @Override
      public Interval getNextInterval(double time, Interval buffer) {
        return createCoverage().getNextInterval(time, buffer);
      }

      @Override
      public Interval getBracketingInterval(double time, Interval buffer) {
        return createCoverage().getBracketingInterval(time, buffer);
      }

      @Override
      public Interval getBoundingInterval(Interval buffer) {
        return createCoverage().getBoundingInterval(buffer);
      }

      @Override
      public boolean contains(double time) {
        if (a.contains(time)) {
          return true;
        }
        for (Coverage c : others) {
          if (c.contains(time)) {
            return true;
          }
        }
        return false;
      }

    };
  }

  /**
   * Creates a new coverage instance that is the union of the supplied coverages.
   * <p>
   * TODO: As implemented, the instance returned by this method is horribly inefficient.
   * </p>
   * 
   * @param a a coverage instance
   * @param b another coverage instance
   * 
   * @return a derived implementation of {@link Coverage} that captures the union of the supplied
   *         coverages
   */
  public static Coverage union(final Coverage a, final Coverage b) {

    /*
     * TODO: Check to see if the inputs are all instances of IntervalCoverage or
     * IntervalSetCoverage. We can greatly accelerate the performance of the resultant coverage if
     * this is the case.
     */

    return new AbstractCoverage() {

      private Coverage createCoverage() {
        IntervalSet.Builder builder = IntervalSet.builder(Coverages.snapshotToIntervalSet(a));
        builder.union(Coverages.snapshotToIntervalSet(b));
        return Coverages.create(builder.build());
      }

      @Override
      public boolean contains(double time) {
        return a.contains(time) || b.contains(time);
      }

      @Override
      public Interval getBoundingInterval(Interval buffer) {
        return createCoverage().getBoundingInterval(buffer);
      }

      @Override
      public Interval getBracketingInterval(double time, Interval buffer) {
        return createCoverage().getBracketingInterval(time, buffer);
      }

      @Override
      public boolean hasNextInterval(double time) {
        return createCoverage().hasNextInterval(time);
      }

      @Override
      public Interval getNextInterval(double time, Interval buffer) {
        return createCoverage().getNextInterval(time, buffer);
      }

    };
  }

  /**
   * Creates a new coverage by intersecting the supplied coverages.
   * <p>
   * TODO: As implemented the instance returned by this method is horribly inefficient.
   * </p>
   * 
   * @param a a coverage instance
   * @param others an array of other coverage instances
   * 
   * @return an implementation of {@link Coverage} that provides the intersection of the supplied
   *         coverages. As the empty set is not supported by the {@link Coverage} interface, if this
   *         derived coverage mutates in such a way as to become empty, the methods on the instance
   *         may begin to throw {@link IllegalStateException}.
   * 
   * @throws IllegalArgumentException if the supplied coverages do not intersect
   */
  public static Coverage intersect(final Coverage a, final Coverage... others) {

    /*
     * TODO: Check to see if the inputs are all instances of IntervalCoverage or
     * IntervalSetCoverage. We can greatly accelerate the performance of the resultant coverage if
     * this is the case.
     */

    checkArgument(intersects(a, others),
        "Coverages do not intersect. Empty set coverages are not permitted.");

    return new AbstractCoverage() {

      private Coverage createCoverage() {
        IntervalSet.Builder builder = IntervalSet.builder(Coverages.snapshotToIntervalSet(a));
        for (Coverage c : others) {
          builder.intersect(Coverages.snapshotToIntervalSet(c));
        }
        IntervalSet set = builder.build();
        checkState(!set.isEmpty(), "Intersected coverage has become empty.");
        return Coverages.create(builder.build());
      }

      @Override
      public boolean hasNextInterval(double time) {
        return createCoverage().hasNextInterval(time);
      }

      @Override
      public Interval getNextInterval(double time, Interval buffer) {
        return createCoverage().getNextInterval(time, buffer);
      }

      @Override
      public Interval getBracketingInterval(double time, Interval buffer) {
        return createCoverage().getBracketingInterval(time, buffer);
      }

      @Override
      public Interval getBoundingInterval(Interval buffer) {
        return createCoverage().getBoundingInterval(buffer);
      }

      @Override
      public boolean contains(double time) {
        if (a.contains(time)) {
          for (Coverage c : others) {
            if (!c.contains(time)) {
              return false;
            }
          }
        }
        return true;
      }

    };
  }

  /**
   * Creates a coverage that is the intersection of the supplied coverages.
   * <p>
   * TODO: The instance returned from this method is horribly inefficient.
   * </p>
   * 
   * @param a a coverage instance
   * @param b another coverage instance
   * 
   * @return an implementation of the {@link Coverage} interface that provides the intersection of a
   *         and b. As the empty set is not supported by the {@link Coverage} interface, if a and b
   *         mutate in such a way as to become empty, the methods on the implementation may throw
   *         {@link IllegalStateException}.
   * 
   * @throws IllegalArgumentException if a and b do not intersect.
   */
  public static Coverage intersect(final Coverage a, final Coverage b) {

    /*
     * Simple check, if a and b represent the exact same instance, no work is to be done.
     */
    if (a == b) {
      return a;
    }

    /*
     * TODO: Check to see if the inputs are all instances of IntervalCoverage or
     * IntervalSetCoverage. We can greatly accelerate the performance of the resultant coverage if
     * this is the case.
     */

    /*
     * Check the intersection of a and b. If they do not currently intersect, then
     */
    checkArgument(intersects(a, b),
        "Coverages do not intersect. Empty coverages are not permitted.");

    return new AbstractCoverage() {

      private Coverage createCoverage() {
        IntervalSet.Builder builder = IntervalSet.builder(Coverages.snapshotToIntervalSet(a));
        builder.intersect(Coverages.snapshotToIntervalSet(b));

        IntervalSet set = builder.build();
        checkState(!set.isEmpty(), "Intersected coverage has become empty.");

        return Coverages.create(set);
      }

      @Override
      public boolean contains(double time) {
        return a.contains(time) && b.contains(time);
      }

      @Override
      public Interval getBoundingInterval(Interval buffer) {
        return createCoverage().getBoundingInterval(buffer);
      }

      @Override
      public Interval getBracketingInterval(double time, Interval buffer) {
        return createCoverage().getBracketingInterval(time, buffer);
      }

      @Override
      public boolean hasNextInterval(double time) {
        return createCoverage().hasNextInterval(time);
      }

      @Override
      public Interval getNextInterval(double time, Interval buffer) {
        return createCoverage().getNextInterval(time, buffer);
      }

    };
  }

  /**
   * Determines whether the sequence of coverages intersect.
   * 
   * @param a a coverage object
   * @param others other coverage objects
   * 
   * @return true if a and others have non-empty intersection, false otherwise
   */
  public static boolean intersects(Coverage a, Coverage... others) {
    IntervalSet.Builder builder = IntervalSet.builder(snapshotToIntervalSet(a));
    for (Coverage c : others) {
      builder.intersect(snapshotToIntervalSet(c));
    }
    return !(builder.size() == 0);
  }

  /**
   * Determines whether two coverage instances intersect.
   * 
   * @param a a coverage object
   * @param b another coverage object
   * 
   * @return true if a and b have non-empty intersection, false otherwise
   */
  public static boolean intersects(Coverage a, Coverage b) {
    IntervalSet setA = snapshotToIntervalSet(a);
    IntervalSet setB = snapshotToIntervalSet(b);
    return !setA.intersect(setB).isEmpty();
  }

  /**
   * An implementation of {@link Coverage#toString()}.
   * 
   * @param coverage the coverage object for which a string representation is desired
   * 
   * @return the string representation, in general of the form &quot;ClassName{[interval],
   *         [interval]...}&quot;
   */
  public static String toStringImplementation(Coverage coverage) {
    ToStringHelper helper = MoreObjects.toStringHelper(coverage);

    for (UnwritableInterval interval : iterable(coverage)) {
      helper.addValue(interval);
    }

    return helper.toString();
  }

  /**
   * An implementation of {@link Coverage#equals(Object)}. Note: two coverage instances are equal if
   * and only if all of the intervals contained in one are present in the other. They may have
   * different {@link Coverage#getBoundingInterval(Interval)} specifications, but are still equal.
   * 
   * @param coverage the coverage object upon which equals is invoked.
   * @param object the object to determine equality with coverage
   * 
   * @return true if the coverage objects are equivalent, false otherwise
   */
  public static boolean equalsImplementation(Coverage coverage, Object object) {

    if (object == checkNotNull(coverage)) {
      return true;
    }
    if (!(object instanceof Coverage)) {
      return false;
    }

    Coverage o = (Coverage) object;

    /*
     * Unfortunately we can't check the boundary interval, because they are not required to be
     * tightly bound to the actual coverage which is what is in question here. Start the deep
     * comparison.
     */
    Interval coverageBuffer = new Interval();
    Interval oBuffer = new Interval();

    /*
     * Start the loop, but handle the first interval separately.
     */
    getFirstInterval(coverage, coverageBuffer);
    getFirstInterval(o, oBuffer);

    if (!coverageBuffer.equals(oBuffer)) {
      return false;
    }

    /*
     * Enter the loop at the minimum time necessary to pop the next interval in either of the two
     * coverages.
     */
    double time = min(coverageBuffer.getEnd(), oBuffer.getEnd());

    boolean coverageHasNext = coverage.hasNextInterval(time);
    boolean oHasNext = o.hasNextInterval(time);

    while (coverageHasNext && oHasNext) {
      coverage.getNextInterval(time, coverageBuffer);
      o.getNextInterval(time, oBuffer);

      if (!coverageBuffer.equals(oBuffer)) {
        return false;
      }

      time = coverageBuffer.getEnd();
      coverageHasNext = coverage.hasNextInterval(time);
      oHasNext = o.hasNextInterval(time);
    }

    /*
     * If we reach here then the coverage instances are equivalent only if they both do not have any
     * remaining intervals.
     */
    return (!coverageHasNext) && (!oHasNext);
  }

  /**
   * An implementation of {@link Coverage#hashCode()} consistent with
   * {@link Coverages#equalsImplementation(Coverage, Object)}.
   * 
   * @param coverage the coverage object for which the hashCode is to be computed
   * 
   * @return the hashCode
   */
  public static int hashCodeImplementation(Coverage coverage) {

    int hashCode = 1;
    Interval buffer = new Interval();

    /*
     * Handle the first interval separately.
     */
    getFirstInterval(coverage, buffer);
    double time = buffer.getEnd();
    hashCode = 31 * hashCode + buffer.hashCode();

    while (coverage.hasNextInterval(time)) {
      coverage.getNextInterval(time, buffer);
      hashCode = 31 * hashCode + buffer.hashCode();
      time = buffer.getEnd();
    }

    return hashCode;
  }

}
